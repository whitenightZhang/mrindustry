#' Function for calculating industry activity trajectories.
#'
#' @md
#' @param subtype One of
#'   - `production` Returns trajectories of primary and secondary steel
#'     production.
#'   - `secondary.steel.max.share` Returns the maximum share of secondary steel
#'     in total steel production.
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
#' @param China_Production A data frame with columns `period` and
#'     `total.production` prescribing total production for China to have,
#'     disregarding results from the stock saturation model.
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
#' @importFrom stats nls SSlogis sd lm
#' @importFrom tibble as_tibble tibble tribble
#' @importFrom tidyr expand_grid pivot_longer pivot_wider replace_na
#' @importFrom utils head
#' @importFrom dplyr bind_cols
#' @importFrom magclass setNames
#' @export
calcSteel_Projections <- function(subtype = 'production',
                                  scenarios,
                                  match.steel.historic.values = TRUE,
                                  match.steel.estimates = 'none',
                                  save.plots = NULL,
                                  China_Production = NULL) {


  if (!is.null(save.plots)) {
    if (!all(isTRUE(file.info(save.plots)$isdir),
             448L == bitwAnd(file.info(save.plots)$mode, 448L))) {
      stop('No writable directory `save.plots`: ', save.plots)
    }
  }

  produce_plots_and_tables <- TRUE

  . <- NULL

  # get EDGE-Industry switches ----
  # FIXME: remove before deploying
  EDGE_scenario_switches <- bind_rows(
    tribble(
      ~scenario,   ~`EDGE-Industry_steel.stock.estimate`,
      'SDP',       'low',
      'SDP_EI',    'low',
      'SDP_MC',    'low',
      'SDP_RC',    'low',
      'SSP1',      'low',
      'SSP2',      'med',
      'SSP3',      'med',
      'SSP4',      'med',
      'SSP5',      'high',
      'SSP2IndiaHigh',   'med',
      'SSP2IndiaMedium', 'med') %>%
      pivot_longer(-'scenario', names_to = 'switch'),

    tribble(
      ~scenario,   ~`EDGE-Industry_scenario.mask.OECD`,
      'SSP4',      'SSP2') %>%
      pivot_longer(-'scenario', names_to = 'switch'),

    tribble(
      ~scenario,   ~`EDGE-Industry_scenario.mask.non-OECD`,
      'SSP4',      'SSP1') %>%
      pivot_longer(-'scenario', names_to = 'switch'),

    # steel stock lifetime convergence ----
    tribble(
      ~scenario,   ~`EDGE-Industry_steel.stock.lifetime.base.scenario`,
      'SDP',       'SSP2',
      'SDP_EI',    'SSP2',
      'SDP_MC',    'SSP2',
      'SDP_RC',    'SSP2',
      'SSP1',      'SSP2',
      'SSP2',      'SSP2',
      'SSP3',      'SSP2',
      'SSP4',      'SSP4',
      'SSP5',      'SSP2',
      'SSP2IndiaHigh',   'SSP2',
      'SSP2IndiaMedium', 'SSP2') %>%
      pivot_longer(-'scenario', names_to = 'switch'),

    tribble(
      ~scenario,   ~`EDGE-Industry_steel.stock.lifetime.convergence.year`,
      'SDP',       '2100',
      'SDP_EI',    '2100',
      'SDP_MC',    '2100',
      'SDP_RC',    '2100',
      'SSP1',      '2100',
      'SSP2',      '2100',
      'SSP3',      '2100',
      'SSP4',      '2010',
      'SSP5',      '2100',
      'SSP2IndiaHigh',   '2100',
      'SSP2IndiaMedium', '2100') %>%
      pivot_longer(-'scenario', names_to = 'switch'),

    tribble(
      ~scenario,   ~`EDGE-Industry_steel.stock.lifetime.convergence.factor`,
      'SDP',       '1.25',
      'SDP_EI',    '1.25',
      'SDP_MC',    '1.25',
      'SDP_RC',    '1.25',
      'SSP1',      '1.25',
      'SSP2',      '1',
      'SSP3',      '1',
      'SSP4',      '1',
      'SSP5',      '0.75',
      'SSP2IndiaHigh',   '1',
      'SSP2IndiaMedium', '1') %>%
      pivot_longer(-'scenario', names_to = 'switch'),

    NULL) %>%
    pivot_wider(names_from = 'switch') %>%
    dplyr::filter(.data$scenario %in% .env$scenarios)

  `EDGE-Industry_scenario_switches` <- EDGE_scenario_switches %>%
      select(
      'scenario',
      `steel.stock.estimate` = 'EDGE-Industry_steel.stock.estimate',
      `scenario.mask.OECD` =
        'EDGE-Industry_scenario.mask.OECD',
      `scenario.mask.non-OECD` =
        'EDGE-Industry_scenario.mask.non-OECD',
      `steel.stock.lifetime.base.scenario` =
        'EDGE-Industry_steel.stock.lifetime.base.scenario',
      `steel.stock.lifetime.convergence.year` =
        'EDGE-Industry_steel.stock.lifetime.convergence.year',
      `steel.stock.lifetime.convergence.factor` =
        'EDGE-Industry_steel.stock.lifetime.convergence.factor')

  # load required data ----
  ## region mapping for aggregation ----
  region_mapping <- toolGetMapping(name = 'regionmapping_21_EU11.csv',
                                   type = 'regional',
                                   where = 'mappingfolder') %>%
    as_tibble() %>%
    select(region = 'RegionCode', iso3c = 'CountryCode')

  ### extra region mapping for Belgium-Luxembourg ----
  region_mapping__Belgium_Luxembourg <- region_mapping %>%
    filter(.data$iso3c %in% c('BEL', 'LUX')) %>%
    distinct(.data$region) %>%
    verify(1 == length(.data$region)) %>%
    mutate(iso3c = 'blx')

  ## country mapping for Müller data ----
  country_mapping <- readSource(type = 'Mueller', subtype = 'countries',
                                convert = FALSE) %>%
    madrat_mule()

  ## steel stock lifetimes ----
  lifetime <- readSource(type = 'Pauliuk', subtype = 'lifetime',
                         convert = FALSE) %>%
    madrat_mule()

  ### add iso3c codes ----
  lifetime <- inner_join(
    lifetime,

    country_mapping %>%
      mutate(country = ifelse(.data$iso3c %in% c('BEL', 'FRA', 'LUX', 'NLD'),
                              'France+Benelux', .data$country)),

    'country'
  )

  ## set of OECD countries ----
  OECD_iso3c <- toolGetMapping(name = 'regionmappingOECD.csv',
                               type = 'regional',
                               where = 'mappingfolder') %>%
    as_tibble() %>%
    select(iso3c = 'CountryCode', region = 'RegionCode') %>%
    filter('OECD' == .data$region) %>%
    pull('iso3c')



  ## historic per-capita steel stock estimates ----
  steel_stock_per_capita <- readSource(type = 'Mueller', subtype = 'stocks', convert = FALSE) %>%
    madrat_mule() %>%
    # remove Netherlands Antilles, only use Bonaire, Sint Eustatius and Saba;
    # Curaçao; and Sint Maarten (Dutch part)
    filter('ANT' != .data$iso3c)

  ## historic per-capita GDP ----
  GDPpC_history <- readSource(type = 'James', subtype = 'IHME_USD05_PPP_pc', convert = FALSE) %>%
    as_tibble() %>%
    select('iso3c' = 'ISO3', 'year' = 'Year', 'value') %>%
    GDPuc::toolConvertGDP(unit_in = 'constant 2005 US$MER',
                          unit_out = mrdrivers::toolGetUnitDollar(),
                          replace_NAs = 'with_USA') %>%
    rename(GDPpC = 'value') %>%
    character.data.frame()

  ## historic population ----
  population_history <- calcOutput("PopulationPast", pastData = "UN_PopDiv", aggregate = FALSE) %>%
    tibble::as_tibble() %>%
    dplyr::select("iso3c", "year", "population" = "value") %>%
    quitte::character.data.frame() %>%
    # million people * 1e6/million = people
    dplyr::mutate(population = .data$population * 1e6)

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

  # estimate steel stock distribution ----
  regression_data <- steel_stock_per_capita %>%
    inner_join(GDPpC_history, c('iso3c', 'year')) %>%
    inner_join(population_history, c('iso3c', 'year'))

  regression_parameters <- tibble()
  for (.estimate in unique(regression_data$estimate)) {

    Asym <- regression_data %>%
      filter(.estimate == .data$estimate) %>%
      group_by(.data$year) %>%
      summarise(Asym = 1.1 * Hmisc::wtd.quantile(x = .data$steel.stock.per.capita,
                                                 weights = .data$population,
                                                 probs = 0.99),
                .groups = 'drop') %>%
      pull('Asym') %>%
      max()

    coefficients <- lm(
      formula = car::logit(x, adjust = 0.025) ~ y,
      data = regression_data %>%
        filter(.estimate == .data$estimate,
               between(.data$steel.stock.per.capita, 0, Asym)) %>%
        mutate(x = .data$steel.stock.per.capita  / Asym) %>%
        select(.data$x, y = .data$GDPpC)
    ) %>%
      getElement('coefficients') %>%
      setNames(NULL)

    xmid <- -coefficients[1] / coefficients[2]
    scal <- 1 / coefficients[2]

    regression_parameters <- bind_rows(
      regression_parameters,

      nls(formula = steel.stock.per.capita
                  ~ Asym / (1 + exp((xmid - GDPpC) / scal)),
          weights = population,
          data = regression_data %>%
            filter(.estimate == .data$estimate),
          start = list(Asym = Asym, xmid = xmid, scal = scal),
          algorithm = 'port',
          trace = FALSE) %>%
        broom::tidy() %>%
        select('term', 'estimate') %>%
        pivot_wider(names_from = 'term', values_from = 'estimate') %>%
        mutate(estimate = .estimate)
    )
  }

  # estimate future steel stocks ----
  steel_stock_estimates <- full_join(
    # GDP, population to calculate per-capita GDP
    full_join(GDP, population, c('scenario', 'iso3c', 'year')) %>%
      assert(not_na, everything()),

    # regression parameters mapped to GDP and population scenarios
    full_join(
      regression_parameters,

      `EDGE-Industry_scenario_switches` %>%
        select('scenario', estimate = 'steel.stock.estimate'),

      'estimate'
    ) %>%
      dplyr::filter(!is.na(.data$scenario)),

    'scenario'
  ) %>%
    # make sure all scenarios have associated regression parameters
    assert(
      not_na, .data$Asym, .data$scal, .data$xmid,
      error_fun = function(errors, data) {
        rows <- lapply(errors, function(x) { x$error_df$index }) %>%
          unlist() %>%
          unique()
        message <- paste0('Unmatched estimates for steel projection regression',
                          'parameters')
        stop(paste(c(message, format(head(as.data.frame(data[rows,])))),
                   collapse = '\n'),
             call. = FALSE)
      }) %>%
    # calculate steel stock estimates using logistic function
    mutate(
      value = SSlogis(input = .data$GDP / .data$population,
                      Asym = .data$Asym, xmid = .data$xmid, scal = .data$scal),
      source = 'computation') %>%
    select('scenario', 'iso3c', 'year', 'value', 'source') %>%
    assert(not_na, everything())

  steel_stock_estimates <- bind_rows(
    steel_stock_estimates,

    steel_stock_per_capita %>%
      filter(.data$year >= min(steel_stock_estimates$year)) %>%
      inner_join(
        `EDGE-Industry_scenario_switches` %>%
          select('scenario', estimate = 'steel.stock.estimate'),

        'estimate'
      ) %>%
      select(.data$scenario, .data$iso3c, .data$year,
             value = .data$steel.stock.per.capita) %>%
      mutate(source = 'Pauliuk')
  ) %>%
    full_join(region_mapping, 'iso3c') %>%
    pivot_wider(names_from = 'source') %>%
    assert(not_na, .data$computation,
           error_fun = function(errors, data) {
             rows <- lapply(errors, function(x) { x$error_df$index }) %>%
               unlist() %>%
               unique()
             message <- paste('Mismatch between Pauliuk and estimation',
                              'regions')
             stop(paste(c(message, format(head(as.data.frame(data[rows,])))),
                        collapse = '\n'),
                  call. = FALSE)
           })

  # TODO: harmonise estimates for historic time steps between scenarios, so as
  # to having identical estimates between SSP1/2/5/... up to 2020

  ## smooth transition ----
  # from Pauliuk data to per-capita GDP-based estimates over 30 years
  fade_end   <- max(steel_stock_per_capita$year)
  fade_start <- fade_end - 30

  steel_stock_estimates <- steel_stock_estimates %>%
    mutate(
      l = pmax(0, pmin(1, (.data$year - fade_start) / (fade_end - fade_start))),
      mix = pmax(0,  .data$l       * .data$computation
                   + (1 - .data$l) * .data$Pauliuk),
      steel.stock.per.capita = ifelse(is.na(.data$mix),
                                      .data$computation, .data$mix)) %>%
    select('scenario', 'iso3c', 'region', 'year', 'steel.stock.per.capita') %>%
    assert(not_na, everything())

  rm(list = c('fade_start', 'fade_end'))

  ## update SSP4 ----
  # SSP4 uses SSP2 estimates for OECD countries and SSP1 estimates for non-OECD
  # countries
  if ("SSP4" %in% scenarios) {
  steel_stock_estimates <- bind_rows(
    # non-masked scenarios
    steel_stock_estimates %>%
      anti_join(
        `EDGE-Industry_scenario_switches` %>%
          select(.data$scenario,
                 .data$scenario.mask.OECD, .data$`scenario.mask.non-OECD`) %>%
          filter(  !is.na(.data$scenario.mask.OECD)
                 & !is.na(.data$`scenario.mask.non-OECD`)) %>%
          select(.data$scenario),

        'scenario'
      ) %>%
      assert(not_na, everything()),

    # masked scenarios, OECD countries
    left_join(
      `EDGE-Industry_scenario_switches` %>%
        select('scenario', 'scenario.mask.OECD') %>%
        filter(!is.na(.data$scenario.mask.OECD)) %>%
        rename(scenario.mask = 'scenario',
               scenario = 'scenario.mask.OECD'),

      steel_stock_estimates %>%
        filter(.data$iso3c %in% OECD_iso3c),

      'scenario'
    ) %>%
      select(-'scenario', 'scenario' = 'scenario.mask') %>%
      assert(not_na, everything()),

    # masked scenarios, non-OECD countries
    left_join(
      `EDGE-Industry_scenario_switches` %>%
        select('scenario', 'scenario.mask.non-OECD') %>%
        filter(!is.na(.data$`scenario.mask.non-OECD`)) %>%
        rename(scenario.mask = 'scenario',
               scenario = 'scenario.mask.non-OECD'),

      steel_stock_estimates %>%
        filter(!.data$iso3c %in% OECD_iso3c),

      'scenario'
    ) %>%
      select(-'scenario', 'scenario' = 'scenario.mask') %>%
      assert(not_na, everything())
  ) %>%
    assert(not_na, everything())
  }

  ## calculate regional and global totals, as well as absolute stocks ----
  steel_stock_estimates <- steel_stock_estimates %>%
    assert(not_na, everything()) %>%
    full_join(population, c('scenario', 'iso3c', 'year')) %>%
    group_by(.data$scenario, .data$year, .data$region) %>%
    sum_total_(group = 'iso3c', value = 'steel.stock.per.capita',
               weight = 'population') %>%
    ungroup(.data$region) %>%
    sum_total_(group = 'iso3c', value = 'steel.stock.per.capita',
               weight = 'population') %>%
    ungroup() %>%
    filter(!(.data$region == 'World' & .data$iso3c != 'Total')) %>%
    # absolute stocks
    mutate(steel.stock = .data$steel.stock.per.capita * .data$population) %>%
    assert(not_na, everything())

  if ('steel_stock_estimates' == subtype) {
    return(list(
      x = steel_stock_estimates %>%
        madrat_mule(),
      weight = NULL))
  }

  # calculate lifetime projections ----

  # steel stock lifetimes are projected to converge from regional averages in
  # 2010 towards the global average in 2100
  lifetime_regions <- lifetime %>%
    select(.data$iso3c, .data$lifetime) %>%
    full_join(filter(GDP, 2010 == .data$year), 'iso3c') %>%
    inner_join(region_mapping, 'iso3c') %>%
    filter(!is.na(.data$lifetime)) %>%
    group_by(.data$scenario, .data$region) %>%
    summarise(
      lifetime = round(sum(.data$lifetime * .data$GDP) / sum(.data$GDP)),
      .groups = 'drop')

  lifetime_global <- lifetime %>%
    select(.data$iso3c, .data$lifetime) %>%
    full_join(filter(GDP, 2010 == .data$year), 'iso3c') %>%
    inner_join(region_mapping, 'iso3c') %>%
    filter(!is.na(lifetime)) %>%
    group_by(.data$scenario) %>%
    summarise(
      lifetime = round(sum(.data$lifetime * .data$GDP) / sum(.data$GDP)),
      .groups = 'drop')

  lifetime_projections <- inner_join(
    lifetime_regions %>%
      rename(`2010` = .data$lifetime),

    lifetime_global %>%
      mutate(region = 'World') %>%
      complete(nesting(!!sym('scenario'), !!sym('lifetime')),
               region = unique(region_mapping$region)) %>%
      rename(`2100` = .data$lifetime),

    c('scenario', 'region')
  ) %>%
    pivot_longer(c(.data$`2010`, .data$`2100`),
                 names_to = 'year', names_transform = list(year = as.integer),
                 values_to = 'lifetime',
                 values_transform = list(lifetime = as.numeric))

  # steel stock lifetimes for specific scenarios in 2100 can be defined relative
  # to the lifetime of a <base.scenario> in a <convergence.year>, times a
  # <convergence.factor>
  lifetime_projections <- bind_rows(
    lifetime_projections %>%
      filter(2010 == .data$year),

    inner_join(
      `EDGE-Industry_scenario_switches` %>%
        select(
          .data$scenario,
          base.scenario      = .data$steel.stock.lifetime.base.scenario,
          convergence.year   = .data$steel.stock.lifetime.convergence.year,
          convergence.factor = .data$steel.stock.lifetime.convergence.factor
        ) %>%
        mutate(convergence.factor = as.numeric(.data$convergence.factor),
               convergence.year   = as.integer(.data$convergence.year)),

      lifetime_projections,

      c('base.scenario' = 'scenario', 'convergence.year' = 'year')
    ) %>%
      mutate(lifetime = round(.data$convergence.factor * .data$lifetime),
             year = 2100) %>%
      select('scenario', 'region', 'year', 'lifetime')
  ) %>%
    interpolate_missing_periods_(periods = list(year = 1950:2150),
                                 value = 'lifetime', expand.values = TRUE)

  # calculate steel trade ----
  steel_yearbook_data <- madrat_mule(readSource('worldsteel', convert = FALSE))

  ## compute historic steel values ----
  steel_historic <- bind_rows(
    # combine Belgium and Luxembourg, because apparent steel use is reported
    # for both together
    steel_yearbook_data %>%
      filter(!.data$iso3c %in% c('BEL', 'LUX')),

    steel_yearbook_data %>%
      filter(.data$iso3c %in% c('BEL', 'LUX')) %>%
      group_by(.data$name, .data$year) %>%
      summarise(value = sum(.data$value, na.rm = TRUE),
                iso3c = 'blx',
                .groups = 'drop')
  ) %>%
    # rename to shorter variable names
    inner_join(
      tribble(
        ~name,                                           ~variable,
        'Apparent Steel Use (Crude Steel Equivalent)',   'use',
        'Total Production of Crude Steel',               'production',
        'Production in Oxygen-Blown Converters',         'prod.BOF',
        'Production in Open Hearth Furnaces',            'prod.OHF',
        'Production in Electric Arc Furnaces',           'prod.EAF',
        'Pig Iron Production',                           'prod.pig',
        'DRI Production',                                'prod.DRI'),

      'name'
    ) %>%
    select('iso3c', 'variable', 'year', 'value') %>%
    # kt/year * 1e-3 Mt/kt = Mt/year
    mutate(value = .data$value * 1e-3) %>%
    pivot_wider(names_from = 'variable') %>%

    mutate(imports = pmax(0, .data$use - .data$production),
           exports = pmin(0, .data$use - .data$production)) %>%
    pivot_longer(cols = c(-'iso3c', -'year'), names_to = 'variable',
                 values_drop_na = TRUE) %>%
    # add region mapping
    inner_join(
      bind_rows(
        region_mapping,
        region_mapping__Belgium_Luxembourg),

      'iso3c')

  ## compute regional/global aggregates ----
  steel_historic <- bind_rows(
    steel_historic,

    steel_historic %>%
      group_by(.data$region, .data$year, .data$variable) %>%
      summarise(value = sum(.data$value, na.rm = TRUE),
                iso3c = 'Total',
                .groups = 'drop'),

    steel_historic %>%
      group_by(.data$year, .data$variable) %>%
      summarise(value = sum(.data$value, na.rm = TRUE),
                region = 'World',
                .groups = 'drop')
  )

  ## compute trade shares ----
  # calculate regional trade shares
  steel_trade_shares_regional <- steel_historic %>%
    filter('Total' != .data$iso3c,
           .data$variable %in% c('use', 'imports', 'exports')) %>%
    # exclude regions that don't have valid import/export data
    group_by(.data$iso3c, .data$year) %>%
    filter(3 == n()) %>%
    group_by(.data$region, .data$year, .data$variable) %>%
    summarise(value = sum(.data$value), .groups = 'drop') %>%
    pivot_wider(names_from = 'variable') %>%
    mutate(import.share = .data$imports / .data$use,
           export.share = .data$exports / .data$use) %>%
    select('region', 'year', 'import.share', 'export.share') %>%
    pivot_longer(c('import.share', 'export.share'), names_to = 'variable')

  # calculate country trade shares, defaulting to regional shares
  steel_trade_shares <- steel_historic %>%
    filter(.data$variable %in% c('imports', 'exports', 'use'),
           !('Total' == .data$iso3c & 'use' != .data$variable)) %>%
    pivot_wider(names_from = 'variable') %>%
    full_join(
      steel_trade_shares_regional %>%
        mutate(variable = paste0(.data$variable, '.regional')) %>%
        pivot_wider(names_from = 'variable') %>%
        inner_join(region_mapping, 'region'),

      c('region', 'iso3c', 'year')
    ) %>%
    mutate(
      import.share = ifelse(!is.na(.data$imports),
                            .data$imports / .data$use,
                            .data$import.share.regional),
      export.share = ifelse(!is.na(.data$exports),
                            .data$exports / .data$use,
                            .data$export.share.regional),
      imports      = ifelse(!is.na(.data$imports),
                            .data$imports,
                            .data$use * .data$import.share),
      exports      = ifelse(!is.na(.data$exports),
                            .data$exports,
                            .data$use * .data$export.share),
      trade        = .data$imports + .data$exports,
      trade.share  = ifelse(!is.na(.data$use),
                            .data$trade / .data$use,
                            .data$import.share + .data$export.share)) %>%
    select('iso3c', 'region', 'year', 'import.share', 'export.share',
           'trade.share')

  # calculate steel production ----
  steel_trade_share_2015 <- steel_trade_shares %>%
    filter('Total' != .data$iso3c,
           2015 == .data$year) %>%
    select('region', 'iso3c', 'trade.share')

  # duplicate Belgium and Luxembourg from Belgium-Luxembourg
  steel_trade_share_2015 <- bind_rows(
    steel_trade_share_2015 %>%
      filter(!.data$iso3c %in% c('blx', 'BEL', 'LUX')),

    steel_trade_share_2015 %>%
      filter('blx' == .data$iso3c) %>%
      pivot_wider(names_from = 'iso3c', values_from = 'trade.share') %>%
      mutate(LUX = .data$blx) %>%
      rename(BEL = .data$blx) %>%
      pivot_longer(-'region', names_to = 'iso3c', values_to = 'trade.share')
  )

  ## aggregate primary and secondary production ----
  steel_historic_prod <- steel_historic %>%
    filter(!is.na(.data$iso3c),
           .data$variable %in% c('production', 'prod.BOF', 'prod.OHF',
                                 'prod.EAF', 'prod.DRI')) %>%
    pivot_wider(names_from = 'variable', values_fill = 0) %>%
    mutate(
      primary.production   = .data$prod.BOF + .data$prod.OHF + .data$prod.DRI,
      # TODO: for VEN & IRN DRI > EAF -- figure out what is going on
      secondary.production = pmax(0, .data$prod.EAF - .data$prod.DRI),
      primary.production   = .data$primary.production
                           * .data$production
                           / ( .data$primary.production
                             + .data$secondary.production),
      secondary.production = .data$secondary.production
                          * .data$production
                          / ( .data$primary.production
                            + .data$secondary.production)) %>%
    select('iso3c', 'region', 'year', 'primary.production',
           'secondary.production') %>%
    pivot_longer(cols = c('primary.production', 'secondary.production'),
                 names_to = 'variable') %>%
    filter(0 != .data$value)

  ### split Belgium and Luxembourg by population ----
  steel_historic_prod <- bind_rows(
    steel_historic_prod %>%
      filter('blx' != .data$iso3c),

    steel_historic_prod %>%
      filter('blx' == .data$iso3c) %>%
      select(-'region') %>%
      left_join(
        population %>%
          filter(.data$iso3c %in% c('BEL', 'LUX'),
                 .data$year %in% unique(steel_historic_prod$year)) %>%
          group_by(.data$year, .data$scenario) %>%
          summarise(population = sum(.data$population),
                    .groups = 'drop_last') %>%
          summarise(population = mean(.data$population), .groups = 'drop'),

        'year'
      ) %>%
      mutate(value = .data$value / .data$population) %>%
      select('year', 'variable', 'value') %>%
      left_join(
        population %>%
          filter(.data$iso3c %in% c('BEL', 'LUX'),
                 .data$year %in% unique(steel_historic_prod$year)) %>%
          inner_join(region_mapping, 'iso3c') %>%
          group_by(.data$region, .data$iso3c, .data$year) %>%
          summarise(population = mean(.data$population), .groups = 'drop'),

        'year'
      ) %>%
      mutate(value = .data$value * .data$population) %>%
      select('region', 'iso3c', 'year', 'variable', 'value')
  ) %>%
    assert(not_na, everything())

  ## calculate secondary steel max share ----
  secondary.steel.max.switches <- calcOutput(
    type = 'industry_max_secondary_steel_share',
    scenarios = scenarios,
    regions = unique(region_mapping$region),
    aggregate = FALSE) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(scenario = 'Data1', region = 'Data2', name = 'Data3',
           value = 'Value') %>%
    mutate(name = paste0('secondary.steel.max.share.', .data$name)) %>%
    pivot_wider() %>%
    character.data.frame()

  tmp <- full_join(
    steel_historic_prod %>%
      filter('Total' != .data$iso3c) %>%
      mutate(match = TRUE),

    secondary.steel.max.switches %>%
      select('scenario', 'secondary.steel.max.share.from') %>%
      mutate(match = TRUE,
             secondary.steel.max.share.from =
               as.integer(.data$secondary.steel.max.share.from)),

    'match'
  ) %>%
    select(-'match') %>%
    group_by(!!!syms(c('scenario', 'region', 'iso3c', 'variable'))) %>%
    filter(.data$year <= .data$secondary.steel.max.share.from) %>%
    group_by(!!!syms(c('scenario', 'region', 'iso3c', 'year', 'variable'))) %>%
    summarise(value = mean(.data$value), .groups = 'drop') %>%
    sum_total_('iso3c') %>%
    pivot_wider(names_from = 'variable', values_fill = list(value = 0)) %>%
    mutate(share = .data$secondary.production
                 / (.data$primary.production + .data$secondary.production)) %>%
    select('scenario', 'region', 'iso3c', 'year', 'share')

  secondary.steel.max.share <- bind_rows(
      tmp,

      tmp %>%
        distinct(.data$scenario, .data$region, .data$iso3c) %>%
        full_join(
          secondary.steel.max.switches %>%
            select('scenario', 'region', year = 'secondary.steel.max.share.by',
                   share = 'secondary.steel.max.share.target') %>%
            mutate(year = as.integer(.data$year),
                   share = as.numeric(.data$share)),

          c('scenario', 'region')
        )
    ) %>%
    interpolate_missing_periods_(
      periods = list('year' = seq_range(range(steel_stock_estimates$year))),
      value = 'share', expand.values = TRUE)

  # expand regional values to missing countries
  secondary.steel.max.share <- bind_rows(
    secondary.steel.max.share %>%
      filter('Total' != .data$iso3c),

    secondary.steel.max.share %>%
      filter('Total' == .data$iso3c) %>%
      select(-'iso3c') %>%
      right_join(
        region_mapping %>%
          anti_join(secondary.steel.max.share, c('region', 'iso3c')),

        'region'
      ) %>%
      assert(not_na, everything())
  )

  ## calculate primary and secondary production ----

  # Imports (> 0 and exports (< 0) are scaled by factors m such that they
  # balance globally.  If imports are twice as large as exports, the imbalance
  # is solved by scaling imports down by a factor twice as large as the factor
  # with which exports are scaled up.  E.g.:
  # trade <- c(1, 2, -7)
  # m <- (1 + sum(trade) / sum(abs(trade)) * -sign(trade))
  # adjusted.trade <- trade * m
  # sum(adjusted.trade) == 0

  production_estimates <- steel_stock_estimates %>%
    filter('Total' != .data$iso3c) %>%
    inner_join(steel_trade_share_2015 %>% select(-'region'), 'iso3c') %>%
    left_join(lifetime_projections, c('scenario', 'region', 'year')) %>%
    select(-'steel.stock.per.capita', -'population') %>%
    assert(not_na, everything()) %>%
    pivot_longer(c(-'scenario', -'iso3c', -'region', -'year')) %>%
    interpolate_missing_periods_(
      periods = list('year' = seq_range(range(.$year)))) %>%
    pivot_wider() %>%
    full_join(
      secondary.steel.max.share %>%
        rename(secondary.steel.max.share = 'share'),

      c('scenario', 'region', 'iso3c', 'year')
    ) %>%
    group_by(!!!syms(c('scenario', 'region', 'iso3c'))) %>%
    mutate(
      # stock additions: rolling average of stock changes (stocks might decrease
      # with decreasing population, but still become obsolete and need
      # replacement) over five years
      stock.additions = zoo::rollmean(
        pmax(0,
             .data$steel.stock - lag(.data$steel.stock, order_by = .data$year,
                                     default = first(.data$steel.stock))),
        k = 5, fill = 'extend', na.rm = TRUE),
      # depreciation: last years steel stock deprecated by 1/lifetime
      depreciation    = lag(x = .data$steel.stock, order_by = .data$year,
                            default = first(.data$steel.stock))
                      / lag(.data$lifetime, order_by = .data$year,
                            default = first(.data$lifetime)),
      # new stock: stock increases and replacements for deprecated old stock
      new.stock       = .data$stock.additions + .data$depreciation,
      # recycable: 90 % of deprecated steel stock are assumed to be recycled
      recyclable      = 0.9 * .data$depreciation, # FIXME: pull parameter out
      # trade: share of new stock serviced by trade
      trade           = .data$new.stock * .data$trade.share) %>%
    group_by(.data$scenario, .data$year) %>%
    mutate(m.factor = ( sum(.data$trade, na.rm = TRUE)
                      / sum(abs(.data$trade), na.rm = TRUE)
                      )) %>%
    group_by(.data$scenario, .data$region, .data$iso3c) %>%
    mutate(
      adj.trade            = ( .data$trade
                             * ifelse(0 < .data$trade, 1 - .data$m.factor,
                                       1 + .data$m.factor)
                             ),
      adj.trade.share      = .data$trade / .data$new.stock,
      production           = .data$new.stock - .data$adj.trade) %>%
      ungroup() %>%
    select('scenario', 'region', 'iso3c', 'year', 'production', 'recyclable',
           'steel.stock', 'secondary.steel.max.share', 'depreciation',
           'adj.trade')

  production_estimates <- production_estimates %>%
    mutate(
      secondary.production = pmin(
        .data$secondary.steel.max.share * .data$production,
        .data$recyclable),
      primary.production   = .data$production - .data$secondary.production) %>%
    select('scenario', 'region', 'iso3c', 'year', 'steel.stock', 'depreciation',
           'primary.production', 'secondary.production',
           trade = 'adj.trade') %>%
    filter(min(.data$year) < .data$year) %>%
    pivot_longer(c('steel.stock', 'depreciation', 'primary.production',
                   'secondary.production', 'trade'),
                 names_to = 'variable') %>%
    group_by(.data$scenario, .data$region, .data$year, .data$variable) %>%
    sum_total_('iso3c') %>%
    ungroup()

  ## calculate production limits of secondary steel----
  # FIXME: move to separate function
  production_limits <- production_estimates %>%
    filter('depreciation' == .data$variable) %>%
    select(-'variable')

  ## construct output ----
  x <- production_estimates %>%
    semi_join(region_mapping, c('region', 'iso3c')) %>%
    filter(min(steel_historic$year) <= .data$year) %>%
    right_join(
      tribble(
        ~variable,                ~pf,
        'primary.production',     'ue_steel_primary',
        'secondary.production',   'ue_steel_secondary'),

      'variable'
    ) %>%
    assert(not_na, everything()) %>%
    # t/year * 1e-6 Gt/t = Gt/year
    mutate(value = .data$value * 1e-9) %>%
    select('scenario', 'iso3c', 'pf', 'year', 'value') %>%
    as.magpie(spatial = 2, temporal = 4, data = 5)

  # match historic values ----
  if (match.steel.historic.values) {
    tmp <- full_join(
      production_estimates %>%
        filter(.data$variable %in% c('primary.production',
                                     'secondary.production')),

      steel_historic_prod %>%
        filter(.data$variable %in% c('primary.production',
                                     'secondary.production')) %>%
        rename(historic = 'value') %>%
        # Mt/year * 1e6 t/Mt = t/year
        mutate(historic = .data$historic * 1e6) %>%
        expand_grid(scenario = unique(production_estimates$scenario)),

      c('scenario', 'region', 'iso3c', 'year', 'variable')
    )

    tmp_factor <- tmp %>%
      group_by(.data$scenario, .data$region, .data$iso3c, .data$variable) %>%
      arrange(.data$year) %>%
      mutate(
        factor = .data$historic / .data$value,
        factor = case_when(
          # countries w/o historic production fade production in over 20 years
          all(is.na(.data$historic)) ~
            pmin(1, pmax(0, (.data$year - max(steel_historic_prod$year)) / 20)),
          # shift country production to meet historic production in the
          # first/last year for which historic data is available
          .data$year < first(.data$year * as.integer(Inf != .data$historic),
                             order_by = .data$year, na_rm = TRUE) ~
            first(.data$factor, order_by = .data$year, na_rm = TRUE),
          .data$year > last(.data$year * as.integer(Inf != .data$historic),
                            order_by = .data$year, na_rm = TRUE) ~
            last(.data$factor, order_by = .data$year, na_rm = TRUE),
          TRUE ~ .data$factor),
        # if value is 0, x/0 is Inf, and 0 * (x/0) is NaN
        factor = ifelse(is.infinite(.data$factor), 0, .data$factor)) %>%
      ungroup() %>%
      select(-'value', -'historic') %>%
      interpolate_missing_periods_(periods = list(year = unique(.$year)),
                                   value = 'factor',
                                   expand.values = TRUE)

    tmp <- full_join(
      tmp,
      tmp_factor,

      c('scenario', 'region', 'iso3c', 'year', 'variable')
    ) %>%
      mutate(value = .data$value * .data$factor) %>%
      ungroup() %>%
      select(-'historic', -'factor') %>%
      assert(not_na, everything())

    ## make zero values explicit ----
    tmp <- tmp %>%
      semi_join(region_mapping, c('region', 'iso3c')) %>%
      complete(.data$scenario, .data$variable,
               nesting(!!sym('region'), !!sym('iso3c')),
               year   = unique(!!sym('year')),
               fill = list(value = 0)) %>%
      assert(not_na, everything())

    ## update max secondary steel shares ----
    update.secondary.steel.max.share <- function(production,
                                                 secondary.steel.max.share) {
      full_join(
        secondary.steel.max.share %>%
          rename(max.share = 'share'),

        production %>%
          pivot_wider(names_from = 'variable') %>%
          mutate(share = .data$secondary.production
                       / ( .data$primary.production
                         + .data$secondary.production)) %>%
          replace_na(list(share = 0)),

        c('scenario', 'region', 'iso3c', 'year')
      ) %>%
        mutate(share = pmax(.data$share, .data$max.share, na.rm = TRUE)) %>%
        select(all_of(colnames(secondary.steel.max.share))) %>%
        assert(not_na, everything())
    }

    secondary.steel.max.share <- update.secondary.steel.max.share(
      tmp, secondary.steel.max.share)

    ## construct output ----
    x <- tmp %>%
      filter(min(steel_historic_prod$year) <= .data$year) %>%
      semi_join(region_mapping, c('region', 'iso3c')) %>%
      right_join(
        tribble(
          ~variable,                ~pf,
          'primary.production',     'ue_steel_primary',
          'secondary.production',   'ue_steel_secondary'),

        'variable'
      ) %>%
      assert(not_na, everything()) %>%
      # t/year * 1e-9 Gt/t = Gt/year
      mutate(value = .data$value * 1e-9) %>%
      select('scenario', 'iso3c', 'pf', 'year', 'value') %>%
      as.magpie(spatial = 2, temporal = 4, data = 5)
  }

  # match exogenous data for China ----
  if (is.data.frame(China_Production)) {
    China_Production <- China_Production %>%
      interpolate_missing_periods(period = seq_range(range(.$period)),
                                  value = 'total.production',
                                  method = 'spline') %>%
      mutate(total.production = .data$total.production * 1e6)

    tmp <- tmp %>%
      filter('SSP2' == .data$scenario,
             'CHN' == .data$iso3c,
             max(steel_historic_prod$year) < .data$year,
             .data$variable %in% c('primary.production',
                                   'secondary.production')) %>%
      group_by(.data$scenario, .data$iso3c, .data$year) %>%
      summarise(production = sum(.data$value), .groups = 'drop') %>%
      left_join(China_Production, c('year' = 'period')) %>%
      mutate(factor = .data$total.production / .data$production) %>%
      select('iso3c', 'year', 'factor') %>%
      expand_grid(scenario = unique(production_estimates$scenario)) %>%
      complete(nesting(!!sym('scenario')),
               iso3c = setdiff(unique(production_estimates$iso3c), 'Total'),
               year = unique(production_estimates$year)) %>%
      group_by(.data$scenario, .data$iso3c) %>%
      mutate(
        factor = case_when(
          max(steel_historic_prod$year) >= .data$year ~ 1,
          TRUE ~ .data$factor),
        factor = case_when(
          is.na(.data$factor) ~ last(na.omit(.data$factor)),
          TRUE ~ .data$factor)) %>%
      ungroup() %>%
      left_join(tmp, c('scenario', 'iso3c', 'year')) %>%
      mutate(value = .data$value * .data$factor) %>%
      select(-'factor') %>%
      assert(not_na, everything())

    ## construct output ----
    x <- tmp %>%
      filter(min(steel_historic_prod$year) <= .data$year) %>%
      semi_join(region_mapping, c('region', 'iso3c')) %>%
      right_join(
        tribble(
          ~variable,                ~pf,
          'primary.production',     'ue_steel_primary',
          'secondary.production',   'ue_steel_secondary'),

        'variable'
      ) %>%
      assert(not_na, everything()) %>%
      # t/year * 1e-9 Gt/t = Gt/year
      mutate(value = .data$value * 1e-9) %>%
      select('scenario', 'iso3c', 'pf', 'year', 'value') %>%
      as.magpie(spatial = 2, temporal = 4, data = 5)
  }

  # match exogenous estimates ----
  ## IEA ETP 2017 ----
  if ('IEA_ETP' == match.steel.estimates) {
    # projected SSP2 production aggregated into OECD/Non-OECD regions
    # w/o Chinese production if that is exogenously prescribed
    if (!is.data.frame(China_Production)) {
      projected_production <- tmp %>%
        filter('SSP2' == .data$scenario) %>%
        mutate(
          region = ifelse(.data$iso3c %in% OECD_iso3c, 'OECD', 'Non-OECD')) %>%
        group_by(.data$region, .data$year) %>%
        summarise(value = sum(.data$value) * 1e-6, .groups = 'drop')
    } else  {
      projected_production <- tmp %>%
        filter('SSP2' == .data$scenario,
               'CHN' != .data$iso3c) %>%
        mutate(
          region = ifelse(.data$iso3c %in% OECD_iso3c, 'OECD', 'Non-OECD')) %>%
        group_by(.data$region, .data$year) %>%
        summarise(value = sum(.data$value) * 1e-6, .groups = 'drop')
    }

    # IEA ETP RTS production, minus Chinese production if exogenously prescribed
    ETP_production <- readSource('IEA_ETP', 'industry', convert = FALSE) %>%
      `[`(,,'RTS.Industry|Materials production|Crude steel.Mt') %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(region = 'Region', year = 'Year', ETP = 'Value') %>%
      filter(.data$region %in% c('OECD', 'Non-OECD')) %>%
      character.data.frame() %>%
      mutate(year = as.integer(.data$year))

    if (is.data.frame(China_Production)) {
      ETP_production <- bind_rows(
        ETP_production %>%
          filter('OECD' == .data$region),

        ETP_production %>%
          filter('Non-OECD' == .data$region) %>%
          left_join(
            China_Production %>%
              mutate(total.production = .data$total.production * 1e-6),

            c('year' = 'period')
          ) %>%
          mutate(total.production = ifelse(!is.na(.data$total.production),
                                           .data$total.production,
                                           last(na.omit(.data$total.production))),
                 ETP = .data$ETP - .data$total.production) %>%
          select(-'total.production')
      ) %>%
        verify(expr = .data$ETP > 0,
               description = paste('exogenous Chinese production does not exceed',
                                   'IEA ETP Non-OECD production'))
    }

    scaling_factor <- inner_join(
      projected_production,
      ETP_production,

      c('region', 'year')
    ) %>%
      group_by(.data$region) %>%
      mutate(
        factor = .data$ETP / .data$value,
        factor = .data$factor / first(.data$factor, order_by = .data$year))

    # If exogenous Chinese production trajectories gobble up all Non-OECD
    # production, temper the scaling factor to only meet 2060 production exactly
    if (is.data.frame(China_Production)) {
      scaling_factor <- scaling_factor %>%
      mutate(factor = ( .data$factor
                      + ( first(.data$factor, order_by = .data$year)
                        + ( ( last(.data$factor, order_by = .data$year)
                            - first(.data$factor, order_by = .data$year)
                            )
                          / (max(.data$year) - min(.data$year))
                          * (.data$year - min(.data$year))
                          )
                        )
                      )
                    / 2)
      }

      scaling_factor <- scaling_factor %>%
      select(-'value', -'ETP') %>%
      bind_rows(
        tibble(
          year = c(max(steel_historic_prod$year), 2100), factor = 1)) %>%
      complete(year = unique(tmp$year)) %>%
      filter(!is.na(.data$region)) %>%
        mutate(factor = zoo::na.approx(object = .data$factor,
                                       x = .data$year,
                                       yleft = first(na.omit(.data$factor)),
                                       yright = last(na.omit(.data$factor)))) %>%
      ungroup() %>%
      assert(not_na, everything())

    if (!is.data.frame(China_Production)) {
      IEA_ETP_matched <- tmp %>%
        mutate(OECD.region = ifelse(.data$iso3c %in% OECD_iso3c,
                                    'OECD', 'Non-OECD')) %>%
        full_join(scaling_factor, c('year', 'OECD.region' = 'region')) %>%
        mutate(value = .data$value * .data$factor) %>%
        select(-'factor', -'OECD.region') %>%
        complete(nesting(!!!syms(c('scenario', 'region', 'iso3c', 'variable'))),
                 year = unique(.$year),
                 fill = list(value = 0)) %>%
        arrange('scenario', 'region', 'iso3c', 'year', 'variable')
    } else {
      IEA_ETP_matched <- tmp %>%
        filter('CHN' != .data$iso3c) %>%
        mutate(OECD.region = ifelse(.data$iso3c %in% OECD_iso3c,
                                    'OECD', 'Non-OECD')) %>%
        full_join(scaling_factor, c('year', 'OECD.region' = 'region')) %>%
        mutate(value = .data$value * .data$factor) %>%
        select(-'factor', -'OECD.region') %>%
        bind_rows(
          tmp %>%
            filter('CHN' == .data$iso3c)
        ) %>%
        complete(nesting(!!!syms(c('scenario', 'region', 'iso3c', 'variable'))),
                 year = unique(.$year),
                 fill = list(value = 0)) %>%
        arrange('scenario', 'region', 'iso3c', 'year', 'variable')
    }

    ## update max secondary steel share ----
    secondary.steel.max.share <- update.secondary.steel.max.share(
      IEA_ETP_matched, secondary.steel.max.share)

    ## construct output ----
    x <- IEA_ETP_matched %>%
      filter(min(steel_historic_prod$year) <= .data$year) %>%
      semi_join(region_mapping, c('region', 'iso3c')) %>%
      right_join(
        tribble(
          ~variable,                ~pf,
          'primary.production',     'ue_steel_primary',
          'secondary.production',   'ue_steel_secondary'),

        'variable'
      ) %>%
      assert(not_na, everything()) %>%
      # t/year * 1e-9 Gt/t = Gt/year
      mutate(value = .data$value * 1e-9) %>%
      select('scenario', 'iso3c', 'pf', 'year', 'value') %>%
      as.magpie(spatial = 2, temporal = 4, data = 5)
  } else if ('none' != match.steel.estimates) {
    stop('Unknown setting \'', match.steel.estimates,
         '\' for match.steel.estimates')
  }

  ### return secondary steel max share ----
  if ('secondary.steel.max.share' == subtype) {
    return(
      list(x = secondary.steel.max.share %>%
             filter(.data$year %in% unique(quitte::remind_timesteps$period),
                    'Total' != .data$iso3c) %>%
             select('scenario', 'iso3c', 'year', 'share') %>%
             as.magpie(spatial = 2, temporal = 3, data = 4),
           weight = calcOutput(
             type = 'Steel_Projections',
             scenarios = scenarios,
             match.steel.historic.values = match.steel.historic.values,
             match.steel.estimates = match.steel.estimates,
             aggregate = FALSE, years = unique(quitte::remind_timesteps$period),
             supplementary = FALSE) %>%
             dimSums(dim = 3.2),
           unit = 'fraction',
           description = 'maximum secondary steel production share'
      )
    )
  }

  if (!is.null(save.plots)) {

    p <- ggplot() +
      geom_area(
        data = x %>%
          as_tibble() %>%
          filter('SSP2' == .data$scenario) %>%
          left_join(region_mapping, 'iso3c') %>%
          full_join(
            tibble(
              pf = c('ue_steel_primary', 'ue_steel_secondary'),
              production = factor(c('Primary Production',
                                    'Secondary Production'),
                                  rev(c('Primary Production',
                                        'Secondary Production')))),

            'pf'
          ) %>%
          group_by(.data$region, .data$year, .data$production) %>%
          summarise(value = sum(.data$value), .groups = 'drop') %>%
          sum_total_('region', name = 'World'),
        mapping = aes(x = !!sym('year'), y = !!sym('value') * 1e3,
                      fill = !!sym('production'))) +
      facet_wrap(~ region, scales = 'free_y') +
      labs(x = NULL, y = 'Mt Steel/year') +
      scale_fill_manual(values = c('Primary Production' = 'orange',
                                   'Secondary Production' = 'yellow'),
                        name = NULL) +
      coord_cartesian(xlim = c(NA, 2100), expand = FALSE) +
      theme_minimal() +
      theme(legend.position = c(1, 0),
            legend.justification = c(1, 0))

    ggsave(plot = p, filename = '6_Steel_production.png',
           device = 'png', path = save.plots, bg = 'white',
           width = 18, height = 14, units = 'cm', scale = 1.73)

    write_rds(x = p,
              file = file.path(save.plots,
                               '6_Steel_production.rds'))
  }

  # return statement ----
  return(list(x = x,
              weight = NULL,
              unit = 'Gt steel/year',
              description = 'primary and secondary steel production'))
}
