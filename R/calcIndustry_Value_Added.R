#' Function for calculating industry activity trajectories.
#'
#' @md
#' @param subtype One of
#'   - `physical` Returns physical production trajectories for cement.
#'   - `economic` Returns value added trajectories for all subsectors.
#' @param scenarios Vector of strings designating the scenarios to be returned.
#' @param match.steel.historic.values Should steel production trajectories match
#'   historic values?
#' @param match.steel.estimates Should steel production trajectories match
#'   exogenous estimates?  `NULL` or one of
#'   - `IEA_ETP` IEA 2017 Energy Transition Pathways steel production totals for
#'     OECD and Non-OECD countries from the _Reference Technologies Scenario_
#'     until 2060, and original growth rates after that.
#' @param save.plots `NULL` (default) if no plots are saved, or the path to save
#'     directories to.
#' @param do_use_expert_guess_steel Whether or not to overwrite steel productions with
#'     expert guesses from input data in the sources folder.
#' @param INDSTAT Gets passed to [`readUNIDO()`] as `subtype` argument.
#'
#' @return A list with a [`magpie`][magclass::magclass] object `x`, `weight`,
#'   `unit`, `description`, `min`, and `max`.
#'
#' @author Michaja Pehl
#'
#' @seealso [`calcOutput()`]
#'
#' @importFrom assertr assert not_na verify within_bounds
#' @importFrom dplyr case_when bind_rows between distinct first last n
#'   mutate pull right_join select semi_join vars
#' @importFrom ggplot2 aes coord_cartesian expand_limits facet_wrap geom_area
#'   geom_line geom_path geom_point ggplot ggsave guide_legend labs
#'   scale_colour_manual scale_fill_discrete scale_fill_manual
#'   scale_linetype_manual scale_shape_manual theme theme_minimal
#' @importFrom quitte character.data.frame df_populate_range duplicate
#'   list_to_data_frame madrat_mule magclass_to_tibble order.levels
#'   seq_range sum_total_
#' @importFrom readr write_rds
#' @importFrom stats lm nls nls.control SSlogis sd
#' @importFrom tibble as_tibble tibble tribble
#' @importFrom tidyr expand_grid pivot_longer pivot_wider replace_na
#' @importFrom utils head
#' @importFrom dplyr bind_cols
#' @importFrom magclass setNames
#' @export
#'
calcIndustry_Value_Added <- function(subtype = 'physical',
                                     scenarios,
                                     match.steel.historic.values = TRUE,
                                     match.steel.estimates = 'none',
                                     save.plots = NULL,
                                     do_use_expert_guess_steel = TRUE,
                                     INDSTAT = 'INDSTAT3') {
  if (!is.null(save.plots)) {
    if (!all(isTRUE(file.info(save.plots)$isdir),
             448L == bitwAnd(file.info(save.plots)$mode, 448L))) {
      stop('No writable directory `save.plots`: ', save.plots)
    }
  }

  linetype_scenarios <- c(regression = 'dashed',
                          # SSP1       = 'dotted',
                          SSP2       = 'solid',
                          # SSP5       = 'dashed'
                          NULL)

  # load required data ----
  ## region mapping for aggregation ----
  region_mapping <- toolGetMapping(name = 'regionmapping_21_EU11.csv',
                                   type = 'regional',
                                   where = 'mappingfolder') %>%
    as_tibble() %>%
    select(region = 'RegionCode', iso3c = 'CountryCode')

  ## UNIDO INSTATA2 data ----
  INDSTAT <- readSource('UNIDO', subtype = INDSTAT) %>%
    as_tibble() %>%
    filter(!is.na(.data$value)) %>%
    left_join(region_mapping, 'iso3c')


  ## population data ----
  population <- calcOutput("Population", scenario = scenarios, aggregate = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::select("scenario" = "variable", "iso3c", "year", "population" = "value") %>%
    quitte::character.data.frame() %>%
    # million people * 1e6/million = people
    dplyr::mutate(population = .data$population * 1e6)

  ## GDP data ----
  GDP <- calcOutput(type = "GDP", scenario = scenarios, average2020 = FALSE, aggregate = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::select("scenario" = "variable", "iso3c", "year", "GDP" = "value") %>%
    quitte::character.data.frame() %>%
    # $m * 1e6 $/$m = $
    dplyr::mutate(GDP = .data$GDP * 1e6)

  ## ---- load cement production data ----
  data_cement_production <- calcOutput('Cement', aggregate = FALSE,
                                       warnNA = FALSE) %>%
    magclass_to_tibble() %>%
    select(-'data') %>%
    filter(!is.na(.data$value))

  # calc manufacturing share in GDP ----
  manufacturing_share <- INDSTAT %>%
    filter('manufacturing' == .data$subsector) %>%
    pivot_wider(names_from = 'subsector') %>%
    inner_join(
      GDP %>%
        filter(max(INDSTAT$year) >= .data$year) %>%
        filter('SSP2' == .data$scenario) %>% # TODO: define default scenario
        select(-'scenario'),

      c('iso3c', 'year')
    ) %>%
    inner_join(
      population %>%
        filter(max(INDSTAT$year) >= .data$year) %>%
        filter('SSP2' == .data$scenario) %>% # TODO: define default scenario
        select(-'scenario'),

      c('iso3c', 'year')
    )

  # regress per-capita industry value added ----
  regression_data <- manufacturing_share %>%
    duplicate(region = 'World') %>%
    pivot_longer(c('population', 'GDP', 'manufacturing')) %>%
    group_by(.data$region, .data$iso3c, .data$year, .data$name) %>%
    summarise(value = sum(.data$value), .groups = 'drop') %>%
    sum_total_('iso3c') %>%
    pivot_wider() %>%
    mutate(
      # mfg.share = .data$manufacturing / .data$GDP,  FIXME
      GDPpC     = .data$GDP / .data$population)

  regression_parameters <- tibble()
  for (r in sort(unique(regression_data$region))) {
    regression_parameters <- bind_rows(
      regression_parameters,

      nls(formula = manufacturing / population ~ a * exp(b / GDPpC),
          data = regression_data %>%
            filter(.data$region == r,
                   'Total' == .data$iso3c),
          start = list(a = 1000, b = -2000),
          trace = FALSE,
          control = nls.control(maxiter = 1000,
                                minFactor = 2 ^ -12)) %>%
        broom::tidy() %>%
        select('term', 'estimate') %>%
        pivot_wider(names_from = 'term', values_from = 'estimate') %>%
        mutate(region = r)
    )
  }

  # FIXME add scenario differentiation of regression parameters ----

  # project total manufacturing share ----
  ## calculate GDPpC projection scenarios ----
  GDPpC <- full_join(population, GDP, c('scenario', 'iso3c', 'year')) %>%
    mutate(GDPpC = .data$GDP / .data$population) %>%
    full_join(region_mapping, 'iso3c')

  ## converge regional towards global limit ----
  parameter_a_world <- regression_parameters %>%
    filter('World' == .data$region) %>%
    getElement('a')

  regression_parameters_converging <- regression_parameters %>%
    filter('World' != .data$region) %>%
    mutate(year = min(regression_data$year)) %>%
    complete(nesting(!!!syms(c('region', 'a', 'b'))),
             year = min(regression_data$year):2150) %>%
    mutate(a = .data$a + ( (parameter_a_world - .data$a)
                         / (2200 - 2000) * (.data$year - 2000)
                         )
                       * (.data$year >= 2000))

  # calc projection ----
  projected_data <- inner_join(
    regression_parameters_converging,
    GDPpC,
    c('region', 'year')
  ) %>%
    mutate(manufacturing = .data$a * exp(.data$b / .data$GDPpC)
                         * .data$population) %>%
    select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP',
           'manufacturing')

  projected_data_regions <- projected_data %>%
    pivot_longer(c('GDP', 'population', 'manufacturing')) %>%
    group_by(!!!syms(c('scenario', 'region', 'year', 'name'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop') %>%
    sum_total_('region', name = 'World') %>%
    pivot_wider() %>%
    mutate(mfg.share = .data$manufacturing / .data$GDP,
           GDPpC     = .data$GDP / .data$population)

  # calc VA of steel production ----
  data_steel_production <- readSource('worldsteel', 'long', convert = FALSE) %>%
    madrat_mule()

  regression_data_steel <- inner_join(
    INDSTAT %>%
      filter('steel' == .data$subsector) %>%
      select('region', 'iso3c', 'year', steel.VA = 'value'),

    data_steel_production %>%
      filter(0 != .data$value) %>%
      rename(steel.production = 'value'),

    c('iso3c', 'year')
  )

  ## compute regional and World aggregates ----
  regression_data_steel <- regression_data_steel %>%
    inner_join(
      population %>%
        filter('SSP2' == .data$scenario) %>% # TODO: define default scenario
        select(-'scenario'),

      c('iso3c', 'year')
    ) %>%
    inner_join(
      GDP %>%
        filter('SSP2' == .data$scenario) %>% # TODO: define default scenario
        select(-'scenario'),

      c('iso3c', 'year')
    ) %>%
    pivot_longer(c('population', 'steel.production', 'GDP', 'steel.VA'),
                 names_to = 'variable',
                 names_transform = list(variable = factor)) %>%
    duplicate(region = 'World')

  regression_data_steel <- bind_rows(
    regression_data_steel,

    regression_data_steel %>%
      group_by(.data$region, .data$year, .data$variable) %>%
      summarise(value = sum(.data$value, na.rm = TRUE),
                iso3c = 'Total',
                .groups = 'drop')
  ) %>%
    group_by(!!!syms(c('region', 'iso3c', 'year', 'variable'))) %>%
    pivot_wider(names_from = 'variable')

  ## compute regression parameters ----
  regression_parameters_steel <- tibble()
  for (r in sort(unique(regression_data_steel$region))) {
    regression_parameters_steel <- bind_rows(
      regression_parameters_steel,

      nls(formula = steel.VApt ~ a * exp(b / GDPpC),
          data = regression_data_steel %>%
            filter(r == .data$region,
                   'Total' == .data$iso3c) %>%
            mutate(steel.VApt = .data$steel.VA / .data$steel.production,
                   GDPpC      = .data$GDP / .data$population),
          start = list(a = 500, b = 500),
          trace = FALSE) %>%
        broom::tidy() %>%
        select('term', 'estimate') %>%
        pivot_wider(names_from = 'term', values_from = 'estimate') %>%
        mutate(region = r)
    )
  }

  ### substitute World for AFR parameters ----
  if ('AFR' %in% region_mapping$region) {
    replacement_region <- 'AFR'
  } else if ('SSA' %in% region_mapping$region) {
    replacement_region <- 'SSA'
  } else {
    replacement_region <- NA
  }

  if (!is.na(replacement_region)) {
    regression_parameters_steel <- bind_rows(
      regression_parameters_steel %>%
        filter(replacement_region != .data$region),

      regression_parameters_steel %>%
        filter('World' == .data$region) %>%
        mutate(region = replacement_region)
    )
    rm(replacement_region)
  }

  ## project steel VA per tonne of steel ----
  parameter_a_world_steel <- regression_parameters_steel %>%
    filter('World' == .data$region) %>%
    pull('a')

  regression_parameters_steel_converging <- regression_parameters_steel %>%
    filter('World' != .data$region) %>%
    mutate(year = as.integer(2000)) %>%
    complete(nesting(!!!syms(c('region', 'a', 'b'))), year = 2000:2100) %>%
    mutate(a = .data$a
             + ( (parameter_a_world_steel - .data$a)
               / (2200 - 2000)
               * (.data$year - 2000)
               ))

  projected_steel_data <- inner_join(
    inner_join(
      regression_parameters_steel_converging,

      GDPpC,

      c('region', 'year')
    ) %>%
      mutate(steel.VApt = .data$a * exp(.data$b / .data$GDPpC)) %>%
      select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP',
             'GDPpC', 'steel.VApt'),

    calcOutput(type = 'Steel_Projections',
               scenarios = scenarios,
               match.steel.historic.values = match.steel.historic.values,
               match.steel.estimates = match.steel.estimates,
               do_use_expert_guess = do_use_expert_guess_steel,
               aggregate = FALSE, supplementary = FALSE) %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(scenario = 'Data1', iso3c = 'Region', variable = 'Data2',
             year = 'Year', value = 'Value') %>%
      character.data.frame() %>%
      mutate(year = as.integer(.data$year),
             # Gt/year * 1e9 t/Gt = t/year
             value = .data$value * 1e9,
             variable = sub('^ue_steel_(primary|secondary)$',
                            '\\1.production', .data$variable)) %>%
      filter(between(.data$year, 2000, 2100)) %>%
      full_join(region_mapping, 'iso3c') %>%
      assert(not_na, everything()) %>%
      group_by(!!!syms(c('scenario', 'region', 'iso3c', 'year'))) %>%
      summarise(steel.production = sum(.data$value), .groups = 'drop'),

    c('scenario', 'region', 'iso3c', 'year')
  ) %>%
    mutate(steel.VA = .data$steel.VApt * .data$steel.production) %>%
    select(-'steel.VApt', -'GDPpC') %>%
    pivot_longer(c('population', 'GDP', 'steel.production', 'steel.VA')) %>%
    duplicate(region = 'World') %>%
    sum_total_('iso3c') %>%
    pivot_wider() %>%
    mutate(GDPpC      = .data$GDP / .data$population,
           steel.VApt = .data$steel.VA / .data$steel.production) %>%
    select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP',
           'steel.production', 'steel.VA', 'GDPpC', 'steel.VApt')

  ## plot steel VA =============================================================
  if (!is.null(save.plots)) {
    d_plot_region_totals <- regression_data_steel %>%
      ungroup() %>%
      filter('Total' == .data$iso3c) %>%
      mutate(GDPpC = .data$GDP / .data$population,
             steel.VA.pt = .data$steel.VA / .data$steel.production) %>%
      # filter outliers
      filter(2000 >= .data$steel.VA.pt) %>%
      select('region', 'year', 'GDPpC', 'steel.VA.pt')

    d_plot_region_totals %>%
      filter('SSA' == .data$region) %>%
      select('region', 'steel.VA.pt') %>%
      mutate(cuts = cut(x = .data$steel.VA.pt,
                        breaks = seq_range(range(.data$steel.VA.pt),
                                           length.out = 31),
                        labels = 1:30, include.lowest = TRUE)) %>%
      group_by(!!!syms(c('region', 'cuts'))) %>%
      summarise(count = n(), .groups = 'drop_last') %>%
      mutate(cuts = as.integer(.data$cuts)) %>%
      ungroup() %>%
      complete(nesting(!!sym('region')), cuts = 1:30) %>%
      mutate(foo = cumsum(is.na(.data$count))) %>%
      filter(cumsum(is.na(.data$count)) > 30 / 2) %>%
      head(n = 1) %>%
      select(-'count', -'foo')

    d_plot_regression <- full_join(
      regression_parameters_steel,

      d_plot_region_totals %>%
        select('region', 'GDPpC') %>%
        df_populate_range('GDPpC'),

      'region'
    ) %>%
      mutate(steel.VA.pt = .data$a * exp(.data$b / .data$GDPpC)) %>%
      select('region', 'GDPpC', 'steel.VA.pt')

    d_plot_projections <- projected_steel_data %>%
      filter(.data$scenario %in% names(linetype_scenarios),
             'Total' == .data$iso3c) %>%
      select('scenario', 'region', 'year', 'GDPpC', steel.VA.pt = 'steel.VApt')

    d_plot_projections <- left_join(
      d_plot_projections,

      d_plot_projections %>%
        select('region', 'year', 'GDPpC') %>%
        filter(max(.data$year) == .data$year) %>%
        group_by(.data$region) %>%
        filter(min(.data$GDPpC) == .data$GDPpC) %>%
        select('region', max.GDPpC = 'GDPpC'),

      'region'
    ) %>%
      filter(.data$GDPpC <= .data$max.GDPpC) %>%
      select(-'max.GDPpC')

    p <- ggplot(mapping = aes(x = !!sym('GDPpC') / 1000,
                              y = !!sym('steel.VA.pt'))) +
      geom_point(data = d_plot_region_totals,
                 mapping = aes(shape = 'region totals')) +
      scale_shape_manual(values = c('region totals' = 'cross'),
                         name = NULL) +
      geom_line(data = d_plot_projections %>%
                  filter(2050 >= .data$year),
                mapping = aes(colour = !!sym('scenario'))) +
      geom_line(data = d_plot_regression,
                mapping = aes(colour = 'regression')) +
      scale_colour_manual(values = c('regression' = 'red',
                                     'SSP2' = 'black'),
                          name = NULL,
                          guide = guide_legend(direction = 'horizontal')) +
      facet_wrap(vars(!!sym('region')), scales = 'free') +
      expand_limits(x = 0, y = 0) +
      labs(x = 'per-capita GDP [1000$/yr]',
           y = 'specific Steel Value Added [$/t]') +
      theme_minimal() +
      theme(legend.justification = c(1, 0),
            legend.position = c(1, 0))

    ggsave(plot = p, filename = '04_Steel_VA_regressions_projections.png',
           device = 'png', path = save.plots, bg = 'white',
           width = 18, height = 14, units = 'cm', scale = 1.73)

    write_rds(x = p,
              file = file.path(save.plots,
                               '04_Steel_VA_regressions_projections.rds'))
  }

  # ========================================================================== =

  # project cement production ----
  ## calculate regression data ----
  regression_data_cement <- full_join(
    INDSTAT %>%
      filter('cement' == .data$subsector) %>%
      select('region', 'iso3c', 'year', cement.VA = 'value') %>%
      filter(.data$year >= min(data_cement_production$year)),

    data_cement_production %>%
      rename(cement.production = 'value') %>%
      left_join(region_mapping, 'iso3c'),

    c('region', 'iso3c', 'year')
  ) %>%
    filter(!is.na(.data$cement.production))

  ### censor nonsensical data ----
  cement_censor <- list_to_data_frame(list(
    BDI = 1980:2010,   # zero cement production
    CIV = 1990:1993,   # cement VA 100 times higher than before and after
    NAM = 2007:2010,   # zero cement production
    HKG = 1973:1979,   # no data for CHN prior to 1980
    IRQ = 1992:1997,   # cement VA 100 times higher than before and after
    RUS = 1970:1990,   # exclude data from Soviet period which biases
    # projections up
    NULL),
    'iso3c', 'year') %>%
    mutate(censored = TRUE)

  regression_data_cement <- regression_data_cement %>%
    anti_join(cement_censor, c('iso3c', 'year'))

  ### compute regional and World aggregates ----
  regression_data_cement <- regression_data_cement %>%
    inner_join(
      population %>%
        filter('SSP2' == .data$scenario) %>% # TODO: define default scenario
        select(-'scenario'),

      c('iso3c', 'year')
    ) %>%
    inner_join(
      GDP %>%
        filter('SSP2' == .data$scenario) %>% # TODO: define default scenario
        select(-'scenario'),

      c('iso3c', 'year')
    ) %>%
    pivot_longer(c('population', 'cement.production', 'GDP', 'cement.VA')) %>%
    duplicate(region = 'World')

  regression_data_cement <- bind_rows(
    regression_data_cement,

    regression_data_cement %>%
      filter(# exclude CHA from global regression data, because it dominates
             # the global regression
             !('World' == .data$region & 'CHN' == .data$iso3c)) %>%
      group_by(!!!syms(c('region', 'year', 'name'))) %>%
      summarise(value = sum(.data$value, na.rm = TRUE),
                iso3c = 'Total',
                .groups = 'drop')
  ) %>%
    pivot_wider()

  ## compute regression parameters ----
  regression_parameters_cement_production <- tibble()
  for (r in sort(unique(regression_data_cement$region))) {
    regression_parameters_cement_production <- bind_rows(
      regression_parameters_cement_production,

      nls(formula = cement.PpC ~ a * exp(b / GDPpC),
          data = regression_data_cement %>%
            filter(r == .data$region,
                   'Total' == .data$iso3c) %>%
            mutate(cement.PpC = .data$cement.production / .data$population,
                   GDPpC      = .data$GDP / .data$population),
          start = list(a = 1, b = -1000),
          trace = FALSE) %>%
        broom::tidy() %>%
        select('term', 'estimate') %>%
        pivot_wider(names_from = 'term', values_from = 'estimate') %>%
        mutate(region = r)
    )
  }

  ## project cement production per capita ----
  param_a <- regression_parameters_cement_production %>%
    filter('World' == .data$region) %>%
    pull('a')

  regression_parameters_cement_production_converging <-
    regression_parameters_cement_production %>%
    filter('World' != .data$region) %>%
    mutate(year = 2000L) %>%
    complete(nesting(!!!syms(c('region', 'a', 'b'))),
             year = 2000:2100) %>%
    left_join(
      toolGetCementConvergenceParameters(),
      'region'
    ) %>%
    mutate(a = .data$a
             + ( (param_a * .data$convergence.level - .data$a)
               / (.data$convergence.year - 2000)
               * (.data$year             - 2000)
               )) %>%
    select(-'convergence.level', -'convergence.year')

  projected_cement_data <- inner_join(
    regression_parameters_cement_production_converging,

    GDPpC,

    c('region', 'year')
  ) %>%
    mutate(cement.production = .data$a * exp(.data$b / .data$GDPpC)
                             * .data$population) %>%
    select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP',
           'cement.production') %>%
    pivot_longer(c('population', 'GDP', 'cement.production')) %>%
    duplicate(region = 'World') %>%
    sum_total_('iso3c') %>%
    pivot_wider() %>%
    mutate(GDPpC      = .data$GDP / .data$population,
           cement.PpC = .data$cement.production / .data$population)

  last_cement_year <- max(data_cement_production$year)

  projected_cement_data <- left_join(
    projected_cement_data %>%
      select(-'GDPpC', -'cement.PpC') %>%
      filter('Total' != .data$iso3c),

    data_cement_production %>%
      rename(data = 'value'),

    c('iso3c', 'year')
  ) %>%
    group_by(!!!syms(c('scenario', 'region', 'iso3c'))) %>%
    mutate(
      shift.factor = .data$data / .data$cement.production,
      shift.factor = ifelse(
        between(.data$year, last_cement_year, last_cement_year + 14),
        1 + ( ( last(na.omit(.data$shift.factor)) - 1)
            * ((last_cement_year + 14 - .data$year) / 14) ^ 2
            ),
        ifelse(last_cement_year > .data$year, .data$shift.factor, 1)),
      shift.factor = zoo::na.approx(object = .data$shift.factor, x = .data$year,
                                    yleft = first(na.omit(.data$shift.factor)),
                                    yright = last(na.omit(.data$shift.factor)),
                                    na.rm = FALSE),
      cement.production = .data$shift.factor * .data$cement.production) %>%
    ungroup() %>%
    select(-'data', -'shift.factor') %>%
    pivot_longer(c('cement.production', 'GDP', 'population')) %>%
    sum_total_('iso3c') %>%
    pivot_wider() %>%
    mutate(GDPpC      = .data$GDP / .data$population,
           cement.PpC = .data$cement.production / .data$population)

  # project cement VA ----
  ## compute regression parameters ----
  regression_parameters_cement <- tibble()
  for (r in sort(unique(regression_data_cement$region))) {
    regression_parameters_cement <- bind_rows(
      regression_parameters_cement,

      nls(formula = cement.VApt ~ a * exp(b / GDPpC),
          data = regression_data_cement %>%
            filter(r == .data$region,
                   'Total' != .data$iso3c,
                   !is.na(.data$cement.VA)) %>%
            pivot_longer(all_of(c('population', 'cement.production', 'GDP',
                                  'cement.VA'))) %>%
            group_by(.data$region, .data$year, .data$name) %>%
            summarise(value = sum(.data$value),
                      iso3c = 'Total',
                      .groups = 'drop') %>%
            pivot_wider() %>%
            mutate(cement.VApt = .data$cement.VA / .data$cement.production,
                   GDPpC       = .data$GDP / .data$population),
          start = list(a = 250, b = 1500),
          trace = FALSE) %>%
        broom::tidy() %>%
        select('term', 'estimate') %>%
        pivot_wider(names_from = 'term', values_from = 'estimate') %>%
        mutate(region = r)
    )
  }

  # project cement VA per tonne of cement ----
  parameter_a_world_cement <- regression_parameters_cement %>%
    filter('World' == .data$region) %>%
    pull('a')

  regression_parameters_cement_converging <- regression_parameters_cement %>%
    filter('World' != .data$region) %>%
    mutate(year = as.integer(2000)) %>%
    complete(nesting(!!!syms(c('region', 'a', 'b'))), year = 2000:2100) %>%
    mutate(a = .data$a
             + ( (parameter_a_world_cement - .data$a)
               / (2200 - 2000)
               * (.data$year - 2000)
               ))

  projected_cement_data <- inner_join(
    regression_parameters_cement_converging,

    projected_cement_data,

    c('region', 'year')
  ) %>%
    filter('Total' != .data$iso3c) %>%
    mutate(cement.VA = .data$a * exp(.data$b / (.data$GDP / .data$population))
                     * .data$cement.production) %>%
    select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP',
           'cement.production', 'cement.VA') %>%
    pivot_longer(c('population', 'GDP', 'cement.production', 'cement.VA')) %>%
    sum_total_('iso3c') %>%
    pivot_wider()

  ## plot cement regressions ====
  if (!is.null(save.plots)) {
    d_plot_region_totals <- regression_data_cement %>%
      filter('Total' == .data$iso3c) %>%
      mutate(GDPpC = .data$GDP / .data$population)

    d_plot_countries <- regression_data_cement %>%
      semi_join(
        regression_data_cement %>%
          filter('World' != .data$region,
                 'Total' != .data$iso3c) %>%
          distinct(.data$region, .data$iso3c) %>%
          group_by(.data$region) %>%
          filter(1 != n()) %>%
          ungroup(),

        c('region', 'iso3c')
      ) %>%
      mutate(GDPpC = .data$GDP / .data$population)

    d_plot_projections <- projected_cement_data %>%
      filter(.data$scenario %in% names(linetype_scenarios),
             'Total' == .data$iso3c,
             between(.data$year, max(d_plot_region_totals$year), 2100)) %>%
      mutate(GDPpC = .data$GDP / .data$population) %>%
      inner_join(
        regression_data_cement %>%
          filter('Total' != .data$iso3c) %>%
          group_by(.data$region) %>%
          mutate(GDPpC = .data$GDP / .data$population) %>%
          filter(max(.data$GDPpC) == .data$GDPpC) %>%
          ungroup() %>%
          select('region', max.GDPpC = 'GDPpC'),

        'region'
      ) %>%
      filter(.data$GDPpC <= 2 * .data$max.GDPpC)

    d_plot_asymptote <- bind_cols(
      regression_parameters_cement_production %>%
        select('region', 'a') %>%
        filter('World' != .data$region),

      regression_parameters_cement_production %>%
        filter('World' == .data$region) %>%
        select(World = 'a')
    ) %>%
      mutate(a.conv = (.data$a + .data$World) / 2) %>%
      select(-'World') %>%
      pivot_longer(c('a', 'a.conv')) %>%
      full_join(
        tribble(
          ~name,      ~year,
          'a',        max(d_plot_region_totals$year),
          'a.conv',   2100),

        'name'
      ) %>%
      select('region', 'year', 'value')

    d_plot_asymptote <- full_join(
      bind_cols(
        regression_parameters_cement_production %>%
          select('region', 'a') %>%
          filter('World' != .data$region),

        regression_parameters_cement_production %>%
          filter('World' == .data$region) %>%
          select(World = 'a')
      ),

      d_plot_projections %>%
        group_by(.data$scenario, .data$region) %>%
        filter(.data$year %in% c(max(d_plot_region_totals$year),
                                 max(.data$year))) %>%
        select('scenario', 'region', 'year', 'GDPpC'),

      'region'
    ) %>%
      mutate(value = .data$a
             + ( (.data$World - .data$a)
                 / (2200 - 2000)
                 * (.data$year - 2000))) %>%
      select('scenario', 'region', 'GDPpC', 'value')

    d_plot_regression <- full_join(
      regression_parameters_cement_production,

      d_plot_region_totals %>%
        select('region', 'GDPpC') %>%
        df_populate_range('GDPpC'),

      'region'
    ) %>%
      mutate(value = .data$a * exp(.data$b / .data$GDPpC))

    d_plot_foo <- full_join(
      regression_parameters_cement_production,

      d_plot_projections %>%
        group_by(.data$scenario, .data$region) %>%
        filter(.data$year %in% c(max(d_plot_region_totals$year),
                                 max(.data$year))) %>%
        select('scenario', 'region', 'year', 'GDPpC'),

      'region'
    ) %>%
      mutate(value = .data$a * exp(.data$b / .data$GDPpC))

    y_max <- d_plot_region_totals %>%
      filter('World' == .data$region) %>%
      mutate(
        cement.production.pC = .data$cement.production / .data$population) %>%
      filter(max(.data$cement.production.pC) == .data$cement.production.pC) %>%
      pull('cement.production.pC')

    projection_points <- c(2015, 2030, 2050, 2075, 2100)

    p <- ggplot(mapping = aes(x = !!sym('GDPpC') / 1000,
                              y = !!sym('cement.production')
                              / !!sym('population'))) +
      # plot region totals
      geom_point(
        data = d_plot_region_totals,
        mapping = aes(shape = 'region totals'),
        size = 2) +
      # plot regression line
      geom_path(
        data = d_plot_regression,
        mapping = aes(y = !!sym('value'), colour = 'regression')) +
      # plot projections
      geom_path(
        data = d_plot_projections,
        mapping = aes(colour = 'projection')) +
      geom_point(
        data = d_plot_projections %>%
          filter(.data$year %in% projection_points),
        mapping = aes(shape = as.character(!!sym('year'))),
        size = 3) +
      scale_shape_manual(
        values = c('region totals' = 'o',
                   setNames(rep('x', length(projection_points)),
                            projection_points)),
        name = NULL) +
      scale_colour_manual(values = c('regression' = 'red',
                                     'projection' = 'black'),
                          name = NULL) +
      facet_wrap(vars(!!sym('region')), scales = 'free') +
      expand_limits(y = c(0, ceiling(y_max * 2) / 2)) +
      labs(x = 'per-capita GDP [1000 $/year]',
           y = 'per-capita Cement Production [tonnes/year]') +
      theme_minimal()


    ggsave(plot = p, filename = '01_Cement_regression_projection.png',
           device = 'png', path = save.plots, bg = 'white',
           width = 18, height = 14, units = 'cm', scale = 1.73)

    write_rds(x = p,
              file = file.path(save.plots,
                               '01_Cement_regression_projection.rds'))
  }

  # ========================================================================== =

  ## plot cement VA regressions ====
  if (!is.null(save.plots)) {
    d_plot_region_totals <- regression_data_cement %>%
      ungroup() %>%
      filter('Total' == .data$iso3c) %>%
      mutate(GDPpC = .data$GDP / .data$population,
             cement.VA.pt = .data$cement.VA / .data$cement.production) %>%
      # filter outliers
      # filter(2000 >= .data$cement.VA.pt) %>%
      select('region', 'year', 'GDPpC', 'cement.VA.pt')

    d_plot_regression <- full_join(
      regression_parameters_cement,

      d_plot_region_totals %>%
        select('region', 'GDPpC') %>%
        df_populate_range('GDPpC'),

      'region'
    ) %>%
      mutate(cement.VA.pt = .data$a * exp(.data$b / .data$GDPpC)) %>%
      select('region', 'GDPpC', 'cement.VA.pt')

    d_plot_projections <- projected_cement_data %>%
      filter(.data$scenario %in% names(linetype_scenarios),
             'Total' == .data$iso3c) %>%
      mutate(GDPpC = .data$GDP / .data$population,
             cement.VA.pt = .data$cement.VA / .data$cement.production) %>%
      select('scenario', 'region', 'year', 'GDPpC', 'cement.VA.pt')

    d_plot_projections <- left_join(
      d_plot_projections,

      d_plot_projections %>%
        select('region', 'year', 'GDPpC') %>%
        filter(max(.data$year) == .data$year) %>%
        group_by(.data$region) %>%
        filter(min(.data$GDPpC) == .data$GDPpC) %>%
        select('region', max.GDPpC = 'GDPpC'),

      'region'
    ) %>%
      filter(.data$GDPpC <= .data$max.GDPpC) %>%
      select(-'max.GDPpC')

    p <- ggplot(mapping = aes(x = !!sym('GDPpC') / 1000,
                              y = !!sym('cement.VA.pt'))) +
      geom_point(data = d_plot_region_totals,
                 mapping = aes(shape = 'region totals')) +
      scale_shape_manual(values = c('region totals' = 'cross'),
                         name = NULL) +
      geom_line(data = d_plot_regression,
                mapping = aes(linetype = 'regression')) +
      geom_line(data = d_plot_projections,
                mapping = aes(linetype = !!sym('scenario'))) +
      scale_linetype_manual(values = linetype_scenarios, name = NULL,
                            guide = guide_legend(direction = 'horizontal')) +
      facet_wrap(vars(!!sym('region')), scales = 'free') +
      expand_limits(x = 0, y = 0) +
      labs(x = 'per-capita GDP [1000$/yr]',
           y = 'specific Cement Value Added [$/t]') +
      theme_minimal() +
      theme(legend.justification = c(1, 0),
            legend.position = c(1, 0))


    ggsave(plot = p, filename = '05a_Cement_VA_regressions_projections.png',
           device = 'png', path = save.plots, bg = 'white',
           width = 18, height = 14, units = 'cm', scale = 1.73)

    write_rds(x = p,
              file = file.path(save.plots,
                               '05_Cement_VA_regressions_projections.rds'))
  }

  # ========================================================================== =

  # project chemicals VA ----
  ## compile regression data ----
  regression_data_chemicals <- INDSTAT %>%
      filter('chemicals' == .data$subsector) %>%
      select('region', 'iso3c', 'year', chemicals.VA = 'value') %>%
    inner_join(
      population %>%
        filter('SSP2' == .data$scenario) %>% # TODO: define default scenario
        select(-'scenario'),

      c('iso3c', 'year')
    ) %>%
    inner_join(
      GDP %>%
        filter('SSP2' == .data$scenario) %>% # TODO: define default scenario
        select(-'scenario'),

      c('iso3c', 'year')
    ) %>%
    duplicate(region = 'World')

  ### compute regional and World aggregates ----
  regression_data_chemicals <- bind_rows(
    regression_data_chemicals,

    regression_data_chemicals %>%
      pivot_longer(c('population', 'GDP', 'chemicals.VA')) %>%
      group_by(!!!syms(c('region', 'year', 'name'))) %>%
      summarise(value = sum(.data$value),
                iso3c = 'Total',
                .groups = 'drop') %>%
      pivot_wider()
  ) %>%
    mutate(GDPpC = .data$GDP / .data$population)

  ## compute regression parameters ----
  regression_parameters_chemicals <- tibble()
  for (r in sort(unique(regression_data_chemicals$region))) {
    regression_parameters_chemicals <- bind_rows(
      regression_parameters_chemicals,

      nls(formula = chemicals.VA / population ~ a * exp(b / GDPpC),
          data = regression_data_chemicals %>%
            filter(r == .data$region,
                   'Total' == .data$iso3c),
          start = list(a = 1000, b = -100),
          trace = FALSE) %>%
        broom::tidy() %>%
        select('term', 'estimate') %>%
        pivot_wider(names_from = 'term', values_from = 'estimate') %>%
        mutate(region = r)
    )
  }

  ## replace outliers with global parameters
  outliers_chemicals <- regression_parameters_chemicals %>%
    select('region', 'a') %>%
    filter(abs((.data$a - mean(.data$a)) / sd(.data$a)) > 3) %>%
    pull('region')

  regression_parameters_chemicals <- bind_rows(
    regression_parameters_chemicals %>%
      filter(!.data$region %in% outliers_chemicals),

    tibble(
      regression_parameters_chemicals %>%
        filter('World' == .data$region) %>%
        select(-'region'),

      region = outliers_chemicals)
  )

  ## project chemicals VA and share ----
  param_a <- regression_parameters_chemicals %>%
    filter('World' == .data$region) %>%
    pull('a')

  regression_parameters_chemicals_converging <-
    regression_parameters_chemicals %>%
    filter('World' != .data$region) %>%
    mutate(year = as.integer(2000)) %>%
    complete(nesting(!!!syms(c('region', 'a', 'b'))), year = 2000:2100) %>%
    mutate(a = .data$a
             + ( (param_a - .data$a)
               / (2200 - 2000)
               * (.data$year - 2000)
               ))

  projected_chemicals_data <- inner_join(
    regression_parameters_chemicals_converging,

    projected_data,

    c('region', 'year')
  ) %>%
    mutate(
      chemicals.VA = .data$a * exp(.data$b / (.data$GDP / .data$population))
                   * .data$population) %>%
    select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP',
           'manufacturing', 'chemicals.VA') %>%
    pivot_longer(c('population', 'GDP', 'manufacturing', 'chemicals.VA')) %>%
    duplicate(region = 'World') %>%
    sum_total_('iso3c') %>%
    pivot_wider() %>%
    mutate(GDPpC           = .data$GDP / .data$population,
           chemicals.share = .data$chemicals.VA / .data$manufacturing)

  ## plot chemicals regressions ================================================
  if (!is.null(save.plots)) {
    d_plot_region_totals <- regression_data_chemicals %>%
      filter('Total' == .data$iso3c)

    d_plot_countries <- regression_data_chemicals %>%
      semi_join(
        regression_data_chemicals %>%
          filter('World' != .data$region,
                 'Total' != .data$iso3c) %>%
          distinct(.data$region, .data$iso3c) %>%
          group_by(.data$region) %>%
          filter(1 != n()) %>%
          ungroup(),

        c('region', 'iso3c')
      ) %>%
      mutate(GDPpC = .data$GDP / .data$population)

    d_plot_regression <- full_join(
      regression_parameters_chemicals,

      d_plot_region_totals %>%
        select('region', 'GDPpC') %>%
        df_populate_range('GDPpC'),

      'region'
    ) %>%
      mutate(value = .data$a * exp(.data$b / .data$GDPpC))

    d_plot_projections <- projected_chemicals_data %>%
      filter(.data$scenario %in% names(linetype_scenarios),
             'Total' == .data$iso3c,
             between(.data$year, max(d_plot_region_totals$year), 2100)) %>%
      select('scenario', 'region', 'year', 'GDPpC', 'chemicals.VA',
             'population') %>%
      group_by(.data$region) %>%
      filter(.data$GDPpC <= .data$GDPpC[  'SSP2' == .data$scenario
                                          & 2100 == .data$year]) %>%
      ungroup()

    p <- ggplot(
      mapping = aes(x = !!sym('GDPpC') / 1000,
                    y = !!sym('chemicals.VA') / !!sym('population'))) +
      # plot region totals
      geom_point(
        data = d_plot_region_totals,
        mapping = aes(shape = 'region totals')) +
      # # plot regression line
      geom_path(
        data = d_plot_regression,
        mapping = aes(y = !!sym('value'), colour = 'regression')) +
      # # plot projections
      geom_path(
        data = d_plot_projections,
        mapping = aes(colour = 'projection')) +
      geom_point(
        data = d_plot_projections %>%
          filter(.data$year %in% projection_points),
        mapping = aes(shape = as.character(!!sym('year'))),
        size = 3) +
      scale_shape_manual(
        values = c('region totals' = 'o',
                   setNames(rep('x', length(projection_points)),
                            projection_points)),
                   name = NULL) +
      scale_colour_manual(values = c('regression' = 'red',
                                     'projection' = 'black'),
                            name = NULL) +
      facet_wrap(vars(!!sym('region')), scales = 'free') +
      expand_limits(y = c(0, ceiling(y_max * 2) / 2)) +
      labs(x = 'per-capita GDP [1000 $/year]',
           y = 'per-capita Chemicals Value Added [$/year]') +
      theme_minimal()

    ggsave(plot = p, filename = '02_Chemicals_regression_projection.png',
           device = 'png', path = save.plots, bg = 'white',
           width = 18, height = 14, units = 'cm', scale = 1.73)

    write_rds(x = p,
              file = file.path(save.plots,
                               '02_Chemicals_regression_projection.rds'))
  }

  # ======================================================================== ===

  if (!is.null(save.plots)) {
    d_plot_region_totals <- regression_data %>%
      filter('Total' == .data$iso3c)

    d_plot_countries <- regression_data %>%
      semi_join(
        regression_data %>%
          filter('World' != .data$region,
                 'Total' != .data$iso3c) %>%
          distinct(.data$region, .data$iso3c) %>%
          group_by(.data$region) %>%
          filter(1 != n()) %>%
          ungroup(),

        c('region', 'iso3c')
      ) %>%
      mutate(GDPpC = .data$GDP / .data$population)

    d_plot_regression <- full_join(
      regression_parameters,

      regression_data %>%
        select('region', 'GDPpC') %>%
        df_populate_range('GDPpC'),

      'region'
    ) %>%
      mutate(value = .data$a * exp(.data$b / .data$GDPpC))

    d_plot_projections <- projected_data %>%
      pivot_longer(c('population', 'GDP', 'manufacturing')) %>%
      sum_total_('iso3c') %>%
      pivot_wider() %>%
      filter(.data$scenario %in% names(linetype_scenarios),
             'Total' == .data$iso3c,
             between(.data$year, max(d_plot_region_totals$year), 2100)) %>%
      mutate(GDPpC = .data$GDP / .data$population) %>%
      select('scenario', 'region', 'year', 'GDPpC', 'manufacturing',
             'population') %>%
      group_by(.data$region) %>%
      filter(.data$GDPpC <= .data$GDPpC[ 'SSP2' == .data$scenario
                                         & 2100 == .data$year]) %>%
      ungroup()

    p <- ggplot(
      mapping = aes(x = !!sym('GDPpC') / 1000,
                    y = !!sym('manufacturing') / !!sym('population') / 1000)) +
      # plot region totals
      geom_point(
        data = d_plot_region_totals,
        mapping = aes(shape = 'region totals')) +
      # # plot regression line
      geom_path(
        data = d_plot_regression,
        mapping = aes(y = !!sym('value') / 1000, colour = 'regression')) +
      # # plot projections
      geom_path(
        data = d_plot_projections,
        mapping = aes(colour = 'projection')) +
      geom_point(
        data = d_plot_projections %>%
          filter(.data$year %in% projection_points),
        mapping = aes(shape = as.character(!!sym('year'))),
        size = 3) +
      scale_shape_manual(
        values = c('region totals' = 'o',
                   setNames(rep('x', length(projection_points)),
                            projection_points)),
        name = NULL) +
      scale_colour_manual(values = c('regression' = 'red',
                                     'projection' = 'black'),
                          name = NULL) +
      facet_wrap(vars(!!sym('region')), scales = 'free') +
      expand_limits(y = c(0, ceiling(y_max * 2) / 2)) +
      labs(x = 'per-capita GDP [1000 $/year]',
           y = 'per-capita Industry Value Added [1000 $/year]') +
      theme_minimal()

    ggsave(plot = p, filename = '03_Industry_regression_projection.png',
           device = 'png', path = save.plots, bg = 'white',
           width = 18, height = 14, units = 'cm', scale = 1.73)

    write_rds(x = p,
              file = file.path(save.plots,
                               '03_Industry_regression_projection.rds'))
  }

  # calculate other Industries Value Added projections ----
  projections <- bind_rows(
    projected_data %>%
      filter('Total' != .data$iso3c, 'World' != .data$region) %>%
      select('scenario', 'region', 'iso3c', 'year', 'population', 'GDP',
             manufacturing.VA = 'manufacturing') %>%
      pivot_longer(c('population', 'GDP', 'manufacturing.VA')),

    projected_cement_data %>%
      filter('Total' != .data$iso3c, 'World' != .data$region) %>%
      select('scenario', 'region', 'iso3c', 'year', 'cement.production',
             'cement.VA') %>%
      pivot_longer(c('cement.production', 'cement.VA')),

    projected_chemicals_data %>%
      filter('Total' != .data$iso3c, 'World' != .data$region) %>%
      select('scenario', 'region', 'iso3c', 'year', 'chemicals.VA') %>%
      pivot_longer('chemicals.VA'),

    projected_steel_data %>%
      filter('Total' != .data$iso3c, 'World' != .data$region) %>%
      select('scenario', 'region', 'iso3c', 'year', 'steel.production',
             'steel.VA') %>%
      pivot_longer(c('steel.production', 'steel.VA'))
  ) %>%
    pivot_wider() %>%
    mutate(otherInd.VA = .data$manufacturing.VA
                       - .data$cement.VA
                       - .data$chemicals.VA
                       - .data$steel.VA)

  ## filter projections with negative VA for otherInd ----
  tmp_negative_otherInd <- projections %>%
    select('scenario', 'region', 'iso3c', 'year', 'otherInd.VA') %>%
    filter(0 > .data$otherInd.VA) %>%
    select(-'otherInd.VA')

  # scenario/region/year combinations with negative values for all countries
  tmp_all_negative_otherInd <- projections %>%
    select('scenario', 'region', 'iso3c', 'year', 'otherInd.VA') %>%
    group_by(!!!syms(c('scenario', 'region', 'year'))) %>%
    filter(sum(0 > .data$otherInd.VA) == n()) %>%
    ungroup() %>%
    select('scenario', 'region', 'year')

  ## calculate regional shares  ----
  # disregarding negative VA for otherInd
  projections_regional_shares <- projections %>%
    select('scenario', 'region', 'iso3c', 'year', matches('VA$')) %>%
    anti_join(
      tmp_negative_otherInd %>%
        select(-'region'),

      c('scenario', 'iso3c', 'year')
    ) %>%
    pivot_longer(matches('VA$')) %>%
    assert(within_bounds(0, Inf), .data$value) %>%
    group_by(!!!syms(c('scenario', 'region', 'year', 'name'))) %>%
    summarise(value = sum(.data$value), .groups = 'drop') %>%
    pivot_wider() %>%
    pivot_longer(matches('^(cement|chemicals|steel|otherInd)\\.VA$'),
                 names_to = 'variable') %>%
    mutate(share = .data$value / .data$manufacturing.VA) %>%
    select('scenario', 'region', 'year', 'variable', 'share') %>%
    # fill temporal gaps arising when for some combination of
    # scenario/region/year the otherInd.VA for all countries are negative
    interpolate_missing_periods_(
      periods = list('year' = unique(projections$year)), value = 'share',
      expand.values = TRUE)

  ## replace VA projections with negative otherInd via regional shares ----
  projections <- bind_rows(
    projections %>%
      select('scenario', 'region', 'iso3c', 'year', 'manufacturing.VA') %>%
      semi_join(
        tmp_negative_otherInd,

        c('scenario', 'region', 'iso3c', 'year')
      ) %>%
      inner_join(
        projections_regional_shares,

        c('scenario', 'region', 'year')
      ) %>%
      mutate(value = .data$manufacturing.VA * .data$share) %>%
      select(-'manufacturing.VA', -'share'),

    projections %>%
      pivot_longer(c('population', 'GDP', 'manufacturing.VA',
                     'cement.production', 'cement.VA', 'chemicals.VA',
                     'steel.production', 'steel.VA', 'otherInd.VA'),
                   names_to = 'variable') %>%
      anti_join(
        tmp_negative_otherInd %>%
          mutate(variable = '') %>%
          complete(nesting(!!!syms(c('scenario', 'region', 'iso3c', 'year'))),
                   variable = c('cement.VA', 'chemicals.VA', 'steel.VA',
                                       'otherInd.VA')),

        c('scenario', 'region', 'iso3c', 'year', 'variable')
      )
  ) %>%
    duplicate(region = 'World') %>%
    sum_total_('iso3c') %>%
    filter(!('World' == .data$region & 'Total' != .data$iso3c)) %>%
    pivot_wider(names_from = 'variable')

  # plot otherInd VA ===========================================================

  if (!is.null(save.plots)) {
    p <- ggplot() +
      geom_area(
        data = projections %>%
          select('scenario', 'region', 'iso3c', 'year', 'cement.VA',
                 'chemicals.VA', 'steel.VA', 'otherInd.VA') %>%
          filter('SSP2' == .data$scenario,
                 2000 <= .data$year,
                 'Total' == .data$iso3c) %>%
          select(-'scenario', -'iso3c') %>%
          pivot_longer(matches('\\.VA$')) %>%
          mutate(name = sub('\\.VA$', '', .data$name)) %>%
          order.levels(
            name = c('cement', 'chemicals', 'steel', 'otherInd')),
        mapping = aes(x = !!sym('year'), y = !!sym('value') / 1e12,
                      fill = !!sym('name'))) +
      scale_fill_discrete(name = NULL,
                          guide = guide_legend(direction = 'horizontal')) +
      facet_wrap(vars(!!sym('region')), scales = 'free_y') +
      labs(x = NULL, y = 'Value Added [$tn/year]') +
      theme_minimal() +
      theme(legend.justification = c(1, 0), legend.position = c(1, 0))

    ggsave(plot = p, filename = '05b_Value_Added_projection.png',
           device = 'png', path = save.plots, bg = 'white',
           width = 18, height = 14, units = 'cm', scale = 1.73)

    write_rds(x = p,
              file = file.path(save.plots,
                               '05_Value_Added_projection.rds'))
  }

  # ========================================================================== =

  # construct output ----
  if ('physical' == subtype) {
    x <- projections %>%
      filter(1993 <= .data$year,
             'Total' != .data$iso3c,
             'World' != .data$region) %>%
      select('scenario', 'iso3c', 'year', ue_cement = 'cement.production',
             ue_chemicals = 'chemicals.VA', ue_otherInd = 'otherInd.VA') %>%
      pivot_longer(matches('^ue_'), names_to = 'pf') %>%
      verify(!(is.na(.data$value) & between(.data$year, 2000, 2100))) %>%
      # t/year * 1e-9 Gt/t = Gt/year      | cement
      # $/year * 1e-12 $tn/$ = $tn/year   | chemicals and other industry
      mutate(
        value = .data$value * case_when(
          'ue_cement'    == pf ~ 1e-9,
          'ue_chemicals' == pf ~ 1e-12,
          'ue_otherInd'  == pf ~ 1e-12)) %>%
      interpolate_missing_periods_(periods = list(year = 1993:2150),
                                   expand.values = TRUE) %>%
      select('scenario', 'iso3c', 'pf', 'year', 'value')
  }

  if ('economic' == subtype) {
    x <- projections %>%
      filter(1993 <= .data$year,
             'Total' != .data$iso3c,
             'World' != .data$region) %>%
      select('scenario', 'iso3c', 'year', 'cement.VA', 'chemicals.VA',
             'steel.VA', 'otherInd.VA') %>%
      pivot_longer(matches('\\.VA$')) %>%
      # $/yr * 1e12 $tn/$ = $tn/yr
      mutate(value = .data$value * 1e-12) %>%
      select('scenario', 'iso3c', 'name', 'year', 'value')
  }

  # return statement ----
  return(list(x = x %>%
                as.magpie(spatial = 2, temporal = 4, datacol = 5),
              weight = NULL,
              unit = 'trillion US$2017/year',
              description = 'chemicals and other industry value added'))

}

#' convergence year and level (relative to global average) to which per-capita
#' cement demand converges (Michaja Pehl)
toolGetCementConvergenceParameters <- function() {

  out <- readr::read_csv(
    file = toolGetMapping("cement_production_convergence_parameters.csv",
                          where = "mrindustry", returnPathOnly = TRUE),
    col_types = "cdi",
    comment = "#"
  )

  out <- bind_rows(
    out %>%
      filter(!is.na(.data$region)),
    out %>%
      utils::head(n = 1) %>%
      filter(is.na(.data$region)) %>%
      select(-"region") %>%
      tidyr::expand_grid(region = toolGetMapping(
        name = "regionmapping_21_EU11.csv",
        type = "regional", where = "mappingfolder"
      ) %>%
        pull("RegionCode") %>%
        unique() %>%
        sort() %>%
        setdiff(out$region))
  )
  return(out)
}
