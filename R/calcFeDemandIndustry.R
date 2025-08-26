#' Calculates FE demand in industry as REMIND variables
#'
#' @md
#' @param use_ODYM_RECC per-capita pathways for `SDP_xx` scenarios?  (Defaults
#'   to `FALSE`.)
#' @param last_empirical_year Last year for which empirical data is available.
#'   Defaults to 2020.
#' @param scenarios Vector of strings designating the scenario. !!Currently it acts as a filter only, with the actual
#'  scenarios computed within the function hard-coded into remind_scenarios.
#' @importFrom assertr assert not_na verify
#' @importFrom dplyr anti_join arrange as_tibble between bind_rows case_when
#'   distinct filter first full_join group_by inner_join left_join
#'   matches mutate n pull rename right_join select semi_join summarise ungroup
#' @importFrom magrittr %>%
#' @importFrom quitte as.quitte cartesian character.data.frame
#'   interpolate_missing_periods interpolate_missing_periods_ madrat_mule
#'   magclass_to_tibble overwrite seq_range
#' @importFrom rlang .data sym syms !!! !!
#' @importFrom tibble tribble
#' @importFrom tidyr complete expand_grid extract nesting pivot_longer
#'   pivot_wider replace_na separate unite
#' @importFrom magclass getNames<- getItems getSets mselect add_dimension
#' @importFrom tidyselect all_of
#' @author Michaja Pehl
#'
calcFeDemandIndustry <- function(scenarios, use_ODYM_RECC = FALSE, last_empirical_year = 2020) {

  # The scenarios argument is currently only used to filter at the end.
  ## Replace any calls to scenario groups such as "SSPs" and "SSP2IndiaDEAs", with calls to the individual scenarios.
  scenarios <- mrdrivers::toolReplaceShortcuts(scenarios)

  # Will ideally be replaced by direct usage of the "scenario" argument in the future. SSP2_NAV_all not included here
  # on purpose (is only created by duplication of SSP2 at the end).
  remind_scenarios <- c(
    paste0("SSP", c(1:5, "2_lowEn", "2_highDemDEU", "2IndiaHigh", "2IndiaMedium")),
    paste0("SDP", c("", "_EI", "_RC", "_MC"))
  )

  # Check if all the scenarios are available.
  if (!all(scenarios %in% c(remind_scenarios, "SSP2_NAV_all"))) {
    stop(paste("The",
               paste(scenarios[! scenarios %in% c(remind_scenarios, "SSP2_NAV_all")], collapse = ", "),
               "scenario(s) are not available."))
  }

  gdpPopScen <- remind_scenarios[
    remind_scenarios %in% mrdrivers::toolGetScenarioDefinition(driver = "GDPpc", aslist = TRUE)$scenario
  ]

  remind_years <- seq(1995, 2150, 5)

  # ---- Industry subsectors data and FE stubs ----
  region_mapping_21 <- toolGetMapping('regionmapping_21_EU11.csv', 'regional',
                                      where = 'mappingfolder') %>%
    as_tibble() %>%
    select(iso3c = 'CountryCode', region = 'RegionCode')

  fixing_year <- calcOutput(
    type = 'industry_subsectors_specific', subtype = 'fixing_year',
    scenarios = remind_scenarios,
    regions = unique(region_mapping_21$region),
    aggregate = FALSE
  ) %>%
    as_tibble() %>%
    select('scenario', region = 'region.1', fixing_year = 'value')

  ## subsector activity projections ----
  industry_subsectors_ue <- mbind(
    calcOutput(
      type = "Industry_Value_Added",
      scenarios = gdpPopScen,
      match.steel.historic.values = TRUE,
      match.steel.estimates = "IEA_ETP",
      aggregate = FALSE,
      years = sort(union(remind_years, last_empirical_year:max(fixing_year$fixing_year))),
      warnNA = FALSE
    ),
    calcOutput(
      type = "Steel_Projections",
      subtype = "production",
      scenarios = gdpPopScen,
      match.steel.historic.values = TRUE,
      match.steel.estimates = "IEA_ETP",
      aggregate = FALSE,
      years = sort(union(remind_years,
                         last_empirical_year:max(fixing_year$fixing_year))),
      supplementary = FALSE)
  )

  ## re-curve specific industry activity per unit GDP ----
  GDP <- calcOutput("GDP",
                    scenario = gdpPopScen,
                    average2020 = FALSE,
                    years = sort(union(remind_years, last_empirical_year:max(fixing_year$fixing_year))),
                    aggregate = FALSE) %>%
    as_tibble() %>%
    dplyr::rename("scenario" = "variable", "GDP" = "value") %>%
    dplyr::mutate(dplyr::across(tidyselect::where(is.factor), as.character))

  ### calculate specific material demand factors ----
  foo <- full_join(
    industry_subsectors_ue %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(iso3c = "Region", year = "Year", scenario = "Data1", subsector = "Data2", value = "Value") %>%
      character.data.frame() %>%
      mutate(year = as.integer(.data$year)) %>%
      # remove zero activity from historic data, to be extended backwards by
      # first projection below
      filter(!(  0 == .data$value
               & .data$year <= max(fixing_year$fixing_year))) %>%
      verify(expr = .data$value != 0,
             description = 'No zero subsector activity after fixing_year'),

    GDP,

    by = c("iso3c", "year", "scenario")
  ) %>%
    # expand time series of per-GDP production back into time for
    # non-existent production in historic periods (e.g. primary steel in
    # NEN)
    complete(nesting(!!!syms(c("iso3c", "year", "scenario", "GDP"))),
             subsector = unique(.data$subsector),
             fill = list(value = NA_real_)) %>%
    group_by(.data$iso3c, .data$scenario, .data$subsector) %>%
    mutate(
      value = .data$GDP
      * zoo::na.fill(object = .data$value / .data$GDP,
                     fill = first(x = .data$value[!is.na(.data$value)]
                                  / .data$GDP[!is.na(.data$value)],
                                  order_by = .data$year[!is.na(.data$value)]))
    ) %>%
    ungroup()

  industry_subsectors_material_alpha <- calcOutput(
    type = "industry_subsectors_specific", subtype = "material_alpha",
    scenarios = c(getNames(x = industry_subsectors_ue, dim = 1),
                  "SSP2_lowEn"),
    regions = unique(region_mapping_21$region),
    aggregate = FALSE
  ) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(scenario = "Data1", region = "Data2", subsector = "Data3",
           name = "Data4", value = "Value") %>%
    character.data.frame() %>%
    pivot_wider() %>%
    mutate(subsector = paste0("ue_", .data$subsector))

  industry_subsectors_material_relative <- calcOutput(
    type = "industry_subsectors_specific", subtype = "material_relative",
    scenarios = c(getNames(x = industry_subsectors_ue, dim = 1),
                  "SSP2_lowEn", "SSP2_highDemDEU"),
    regions = unique(region_mapping_21$region),
    aggregate = FALSE
  ) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(scenario = "Data1", base.scenario = "Data2", region = "Data3",
           subsector = "Data4", name = "Data5", value = "Value") %>%
    character.data.frame() %>%
    pivot_wider() %>%
    mutate(subsector = paste0("ue_", .data$subsector))

  if (use_ODYM_RECC) {
    industry_subsectors_material_relative <- industry_subsectors_material_relative %>%
      filter(!.data$scenario %in% c("SDP_EI", "SDP_MC", "SDP_RC"))
  }


  industry_subsectors_material_relative_change <- calcOutput(
    type = "industry_subsectors_specific",
    subtype = "material_relative_change",
    scenarios = c(getNames(x = industry_subsectors_ue, dim = 1), "SSP2_lowEn"),
    regions = unique(region_mapping_21$region),
    aggregate = FALSE
  ) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(scenario = "Data1", base.scenario = "Data2", region = "Data3",
           subsector = "Data4", name = "Data5", value = "Value") %>%
    character.data.frame() %>%
    pivot_wider() %>%
    mutate(subsector = paste0("ue_", .data$subsector))

  if (use_ODYM_RECC) {
    industry_subsectors_material_percapita <- calcOutput(
      type = "ODYM_RECC",
      subtype = "REMIND_industry_trends",
      aggregate = FALSE) %>%
      magclass_to_tibble() %>%
      filter(!.data$scenario %in% c(
        unique(industry_subsectors_material_alpha$scenario),
        unique(industry_subsectors_material_relative$scenario),
        unique(industry_subsectors_material_relative_change$scenario))) %>%
      interpolate_missing_periods_(
        periods = list(
          year = unique(pmax(remind_years,
                             min(.$year)))),
        expand.values = TRUE)
  }

  bind_rows(
    industry_subsectors_material_alpha %>% select("scenario", "region", "subsector"),
    industry_subsectors_material_relative %>% select("scenario", "region", "subsector"),
    industry_subsectors_material_relative_change %>% select("scenario", "region", "subsector")
  ) %>%
    group_by(!!!syms(c("scenario", "region", "subsector"))) %>%
    summarise(count = n(), .groups = "drop") %>%
    verify(expr = 1 == .data$count,
           error_fun = function(errors, data) {
             stop("Industry specific material is over-specified for:\n",
                  paste(
                    format(data[errors[[1]]$error_df$index, ],
                           width = 80,
                           n = errors[[1]]$num.violations),
                    collapse = "\n"))
           }) %>%
    invisible()

  foo2 <- bind_rows(
    ### SSP2 default scenario ----
    foo %>% filter("SSP2" == .data$scenario),

    ### alpha relative to SSP2 ----
    industry_subsectors_material_alpha %>%
      full_join(
        full_join(
          foo %>%
            filter("SSP2" == .data$scenario) %>%
            # keep specific production constant for historic years without
            # production
            group_by(!!!syms(c("iso3c", "subsector"))) %>%
            mutate(specific.production = .data$value / .data$GDP,
                   specific.production = ifelse(
                     0 != .data$value, .data$specific.production,
                     first(.data$specific.production[0 != .data$value],
                           order_by = .data$year))) %>%
            ungroup() %>%
            select("iso3c", "year", "subsector", "specific.production") %>%
            interpolate_missing_periods_(
              periods = list(year = seq_range(range(.$year))),
              value = "specific.production",
              method = "linear"),

          # mark years which have no historic production so that we can set
          # them to 0 once we calculated future production for the new
          # scenarios
          foo %>%
            filter("SSP2" == .data$scenario) %>%
            mutate(fake.value = 0 == .data$value) %>%
            select("iso3c", "year", "subsector", "fake.value") %>%
            interpolate_missing_periods_(
              periods = list(year = seq_range(range(.$year))),
              value = "fake.value",
              method = "linear") %>%
            mutate(fake.value = 0 != .data$fake.value),

          c("iso3c", "year", "subsector")
        ) %>%
          full_join(region_mapping_21, "iso3c"),

        by = c("region", "subsector"),
        relationship = "many-to-many"
      ) %>%
      left_join(fixing_year, by = c('scenario', 'region')) %>%
      assert(not_na, 'fixing_year',
             description = paste('missing fixing_year for scenario in',
                                 'material_alpha')) %>%
      group_by(!!!syms(c("scenario", "subsector", "iso3c"))) %>%
      mutate(
        # alpha factors converge linearly towards zero over the convergence
        # time
        conv.fctr = 1 - pmin(1, ( (.data$year - .data$fixing_year)
                                / .data$convergence_time
                                )),
        alpha.conv = ifelse(.data$year <= .data$fixing_year, 1,
                            1 - .data$alpha * .data$conv.fctr),
        # specific production is scaled with the cumulated converged alpha
        # factors
        cum.fctr = cumprod(.data$alpha.conv),
        specific.production = ifelse(
          .data$year <= .data$fixing_year, .data$specific.production,
          ( .data$specific.production[.data$fixing_year == .data$year]
          * .data$cum.fctr
          ))
          # ensure that years without historic production are 0
        * (1 - .data$fake.value)
      ) %>%
      ungroup() %>%
      select("region", "iso3c", "year", "scenario", "subsector",
             "specific.production") %>%
      filter(.data$year %in% unique(foo$year)) %>%
      left_join(
        bind_rows(
          foo,

          foo %>%
            filter("SSP2" == .data$scenario) %>%
            mutate(scenario = "SSP2_lowEn")

        ),

        c("scenario", "subsector", "iso3c", "year")
      ) %>%
      mutate(value = .data$specific.production * .data$GDP) %>%
      select(all_of(colnames(foo))) %>%
      assert(not_na, everything())
  )

  foo3 <- bind_rows(
    foo2,

    ### specific production relative baseline ----
    industry_subsectors_material_relative %>%
      left_join(
        foo2 %>%
          mutate(specific.production = .data$value / .data$GDP) %>%
          select(base.scenario = "scenario", "subsector", "iso3c", "year",
                 "specific.production") %>%
          full_join(region_mapping_21, "iso3c"),

        by = c("base.scenario", "region", "subsector"),
        relationship = "many-to-many"
      ) %>%
      left_join(
        bind_rows(
          foo,

          foo %>%
            filter("SSP2" == .data$scenario) %>%
            mutate(scenario = "SSP2_highDemDEU")

        )  %>%
          select("scenario", "subsector", "iso3c", "year", "GDP", "value"),

        c("scenario", "subsector", "iso3c", "year")
      ) %>%
      assert(not_na, everything()) %>%
      left_join(fixing_year, by = c('scenario', 'region')) %>%
      assert(not_na, 'fixing_year',
             description = paste('missing fixing_year for scenario in',
                                 'material_relative')) %>%
      # scale factor in over 15 years
      mutate(l = pmin(1, pmax(0, (.data$year - .data$fixing_year) / .data$phasein)),
             value = .data$specific.production
                   * .data$GDP
                   * (.data$factor * .data$l + 1 * (1 - .data$l))) %>%
      select(all_of(colnames(foo))),

    ### specific production change relative baseline ----
    full_join(
      # base scenario data
      foo %>%
        rename(base.scenario = "scenario") %>%
        semi_join(
          industry_subsectors_material_relative_change,

          c("base.scenario", "subsector")
        ) %>%
        pivot_longer(c('GDP', 'value')) %>%
        interpolate_missing_periods_(
          periods = list(
            'year' = sort(union(
              unique(foo$year),
              seq.int(last_empirical_year, max(fixing_year$fixing_year)))))) %>%
        pivot_wider() %>%
        full_join(region_mapping_21, "iso3c"),

      # change parameters
      industry_subsectors_material_relative_change,

      c("base.scenario", "region", "subsector")
    ) %>%
      select("scenario", "iso3c", "region", "subsector", "year",
             base.value = "value", base.GDP = "GDP", "factor") %>%
      # GDP trajectories of target scenarios
      left_join(
        foo %>%
          select("scenario", "iso3c", "subsector", "year", "GDP") %>%
            interpolate_missing_periods_(
                periods = list(
                    'year' = sort(union(
                      unique(foo$year),
                      seq.int(last_empirical_year,
                              max(fixing_year$fixing_year))))),
                value = 'GDP'),

        c("scenario", "iso3c", "subsector", "year")
      ) %>%
      left_join(fixing_year, by = c('scenario', 'region')) %>%
      assert(not_na, 'fixing_year',
             description = paste('missing fixing_year for scenario in',
                                 'material_relative_change')) %>%
      group_by(!!!syms(c("scenario", "iso3c", "subsector"))) %>%
      mutate(
        # specific production of base scenarios
        base.specific.production = .data$base.value / .data$base.GDP,
        # change in specific production of base scenarios relative to 2020
        base.change = ( .data$base.specific.production
                      / .data$base.specific.production[   .data$fixing_year
                                                       == .data$year]),
        # modified change of target scenarios
        # If base change is below (above) 1, i.e. material efficiency is
        # improving (deteriorating), efficiency gains (losses) are halved
        # (doubled).  Changes of historic values (i.e. before 2015) are
        # identical to base scenario.  Not finite changes (e.g. division by
        # zero) lead to constant values.
        change = case_when(
          !is.finite(.data$base.change) ~ 1,
          .data$year <= .data$fixing_year        ~ .data$base.change,
          TRUE ~  ( ( (.data$base.change - 1)
                    * .data$factor ^ sign(1 - .data$base.change)
                    )
                  + 1)),
        specific.production =
          ( .data$base.specific.production[.data$fixing_year == .data$year]
          * .data$change
          ),
        value = ifelse(  !is.finite(.data$base.change)
                       | .data$year <= .data$fixing_year,
                       .data$base.value,
                       .data$specific.production * .data$GDP)) %>%
      ungroup() %>%
      select("scenario", "iso3c", "subsector", "year", "value", "GDP")
  )

  ### per-capita projections ----
  . <- NULL

  if (use_ODYM_RECC) {
    foo4 <- bind_rows(
      foo3 %>% select(-"GDP"),

      foo3 %>%
        filter(
          "SSP2" == .data$scenario,
          min(industry_subsectors_material_percapita$year) > .data$year) %>%
        select(-"GDP") %>%
        complete(
          nesting(!!!syms(c("iso3c", "year", "subsector", "value"))),
          scenario = unique(industry_subsectors_material_percapita$scenario)
        ) %>%
        filter("SSP2" != .data$scenario),

      foo3 %>%
        filter(
          "SSP2" == .data$scenario,
          min(industry_subsectors_material_percapita$year) == .data$year
        ) %>%
        select(-"scenario", -"GDP", -"year") %>%
        inner_join(
          calcOutput("Population", scenario = gdpPopScen, aggregate = FALSE, years = remind_years) %>%
            magclass_to_tibble() %>%
            select("iso3c", "scenario" = "variable", "year", "population" = "value") %>%
            filter(
              min(industry_subsectors_material_percapita$year) <= .data$year,
              .data$scenario %in%
                unique(industry_subsectors_material_percapita$scenario)
            ) %>%
            group_by(.data$iso3c, .data$scenario) %>%
            mutate(
              population = .data$population
              / first(.data$population, order_by = .data$year)) %>%
            ungroup(),

          "iso3c") %>%
        inner_join(
          industry_subsectors_material_percapita %>%
            mutate(subsector = paste0("ue_", .data$subsector)) %>%
            interpolate_missing_periods_(
              periods = list(year = seq_range(range(.$year)))) %>%
            rename(activity = "value"),

          c("iso3c", "subsector", "year", "scenario")
        ) %>%
        mutate(value = .data$value * .data$population * .data$activity) %>%
        select("iso3c", "scenario", "subsector", "year", "value")
    )

    industry_subsectors_ue <- foo4 %>%
      select("iso3c", "year", "scenario", pf = "subsector", "value") %>%
      as.magpie(spatial = 1, temporal = 2, datacol = ncol(.))
  } else {
    industry_subsectors_ue <- foo3 %>%
      select("iso3c", "year", "scenario", pf = "subsector", "value") %>%
      as.magpie(spatial = 1, temporal = 2, datacol = ncol(.))
  }

  ## subsector FE shares ----
  ### get 1993-2020 industry FE ----
  industry_subsectors_en <- calcOutput("IndustrySubsectorsOutput", aggregate = FALSE) %>%
    # convert to data frame
    as.data.frame() %>%
    as_tibble() %>%
    select(iso3c = "Region", year = "Year", pf = "Data2",
           value = "Value") %>%
    character.data.frame() %>%
    mutate(year = as.integer(as.character(.data$year))) %>%
    # get 1993-2020 industry FE data
    filter(grepl("^fe.*_(cement|chemicals|steel|otherInd)", .data$pf),
           between(.data$year, 1993, last_empirical_year)) %>%
    # sum up fossil and bio SE (which produce the same FE), aggregate
    # regions
    full_join(region_mapping_21, "iso3c") %>%
    group_by(!!!syms(c("year", "region", "pf"))) %>%
    summarise(value = sum(.data$value), .groups = "drop") %>%
    # split feel steel into primary and secondary production
    left_join(
      industry_subsectors_ue[, , "ue_steel", pmatch = TRUE] %>%
        as.data.frame() %>%
        as_tibble() %>%
        select(iso3c = "Region", scenario = "Data1", year = "Year",
               subsector = "Data2", production = "Value") %>%
        filter("SSP2" == .data$scenario) %>%
        select(-"scenario") %>%
        # aggregate regions
        full_join(region_mapping_21, "iso3c") %>%
        group_by(!!!syms(c("year", "region", "subsector"))) %>%
        summarise(production = sum(.data$production), .groups = "drop") %>%
        mutate(year      = as.integer(as.character(.data$year)),
               pf        = "feel_steel",
               subsector = sub("_production$", "", .data$subsector)) %>%
        pivot_wider(names_from = "subsector", values_from = "production"),

      c("region", "year", "pf")
    ) %>%
    group_by(!!!syms(c("year", "region", "pf"))) %>%
    # if there was no historic steel production, but feel_steel was
    # allocated, assume primary and secondary production to be identical
    mutate(
      foo = .data$ue_steel_primary + .data$ue_steel_secondary,
      ue_steel_primary   = ifelse(0 == .data$foo, 1,
                                  .data$ue_steel_primary),
      ue_steel_secondary = ifelse(0 == .data$foo, 1,
                                  .data$ue_steel_secondary)
    ) %>%
    select(-"foo") %>%
    # assume that secondary steel production is nine times as electricity
    # intensive (not energy intensive!) as primary production, since
    # detailed data is missing so far
    mutate(feel_steel_secondary =
             (9 * .data$ue_steel_secondary * .data$value)
           / (9 * .data$ue_steel_secondary + .data$ue_steel_primary),
           feel_steel_primary = .data$value - .data$feel_steel_secondary
    ) %>%
    ungroup() %>%
    select(-"ue_steel_primary", -"ue_steel_secondary") %>%
    pivot_wider(names_from = "pf") %>%
    select(-"feel_steel") %>%
    pivot_longer(matches("^fe.*"), names_to = "pf", values_drop_na = TRUE)

  # can be removed once feelwlth is replaced by feel in all mappings for
  # calcIO()
  industry_subsectors_en <- industry_subsectors_en %>%
    mutate(pf = sub("^feelwlth_", "feel_", .data$pf))


  FE_alpha_mod <- 1

  ### calculate 1993-2020 industry subsector FE shares ----
  industry_subsectors_en_shares <- industry_subsectors_en %>%
    mutate(subsector = sub("^[^_]+_", "", .data$pf),
           subsector = ifelse("steel" == .data$subsector, "steel_primary",
                              .data$subsector)) %>%
    group_by(!!!syms(c("year", "region", "subsector"))) %>%
    mutate(share = .data$value / sum(.data$value),
           share = ifelse(is.finite(.data$share), .data$share, 0)) %>%
    ungroup() %>%
    select(-"value")

  failed_share_sum <- industry_subsectors_en_shares %>%
    group_by(!!!syms(c("year", "region", "subsector"))) %>%
    summarise(failed = abs(sum(.data$share) - 1) > 1e-15, .groups = "drop")

  if (any(failed_share_sum$failed)) {
    stop("industry_subsectors_en_shares don't add up to 1.")
  }

  ### future subsector FE shares from IEA ETP 2017 ----
  IEA_ETP_Ind_FE_shares <- readSource("IEA_ETP", "industry",
                                      convert = FALSE) %>%
    # filter for OECD and Non-OECD regions and RTS scenario
    `[`(c("OECD", "Non-OECD"), , "RTS", pmatch = "left") %>%
    # convert to data frame
    as.data.frame() %>%
    as_tibble() %>%
    select(region = "Region", year = "Year", variable = "Data2",
           value = "Value") %>%
    character.data.frame() %>%
    mutate(year = as.integer(as.character(.data$year))) %>%
    # filter for future data
    filter(max(industry_subsectors_en$year) < .data$year) %>%
    # rename variables
    right_join(
      tribble(
        ~subsector,    ~fety,    ~variable,
        "cement",      "feso",   "Industry|Cement - final energy consumption|Coal",
        "cement",      "feso",   "Industry|Cement - final energy consumption|Biomass",
        "cement",      "feso",   "Industry|Cement - final energy consumption|Waste",
        "cement",      "feso",   "Industry|Cement - final energy consumption|Other renewables",
        "cement",      "feli",   "Industry|Cement - final energy consumption|Oil",
        "cement",      "fega",   "Industry|Cement - final energy consumption|Natural gas",
        "cement",      "feel",   "Industry|Cement - final energy consumption|Electricity",

        "chemicals",   "feso",   "Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Coal",
        "chemicals",   "feso",   "Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Biomass",
        "chemicals",   "feso",   "Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Waste",
        "chemicals",   "feso",   "Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Other renewables",
        "chemicals",   "feli",   "Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Oil",
        "chemicals",   "fega",   "Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Natural gas",
        "chemicals",   "feel",   "Industry|Chemicals and petrochemicals - final energy consumption and chemical feedstock|Electricity",

        "steel",       "feso",   "Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Coal",
        "steel",       "feso",   "Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Biomass",
        "steel",       "feso",   "Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Waste",
        "steel",       "feso",   "Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Other renewables",
        "steel",       "feli",   "Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Oil",
        "steel",       "fega",   "Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Natural gas",
        "steel",       "feel",   "Industry|Iron and steel - final energy consumption incl_ blast furnaces and coke ovens|Electricity",

        "total",       "feso",   "Industry|Total industry final energy consumption|Coal",
        "total",       "feso",   "Industry|Total industry final energy consumption|Biomass",
        "total",       "feso",   "Industry|Total industry final energy consumption|Waste",
        "total",       "feso",   "Industry|Total industry final energy consumption|Other renewables",
        "total",       "feli",   "Industry|Total industry final energy consumption|Oil",
        "total",       "fega",   "Industry|Total industry final energy consumption|Natural gas",
        "total",       "fehe",   "Industry|Total industry final energy consumption|Heat",
        "total",       "feel",   "Industry|Total industry final energy consumption|Electricity"),

      "variable"
    ) %>%
    # drop faulty data
    # - 2055/OECD/Chemicals (all zero)
    anti_join(
      tribble(
        ~region,   ~year,   ~subsector,
        'OECD',    2055,    'chemicals'),
      c('region', 'year', 'subsector')) %>%
    # - 2055/Non-OECD/Chemicals/Oil (exceeds total Oil)
    anti_join(
      tribble(
        ~region,      ~year,   ~subsector,    ~fety,
        'Non-OECD',   2055,    'chemicals',   'feli'),
      c('region', 'year', 'subsector', 'fety')) %>%
    # aggregate by subsector and fety
    group_by(!!!syms(c("region", "year", "subsector", "fety"))) %>%
    summarise(value = sum(.data$value), .groups = "drop") %>%
    # fill gaps (e.g. 2055/Chemicals) with interpolated data
    complete(nesting(!!!syms(c("region", "subsector", "fety"))),
             year = unique(.data$year)) %>%
    interpolate_missing_periods_(periods = list(year = unique(.$year))) %>%
    # calculate otherInd as total - cement - chemicals - steel
    pivot_wider(names_from = "subsector", values_fill = 0) %>%
    mutate(otherInd = .data$total
                    - (.data$cement + .data$chemicals + .data$steel)) %>%
    select(-"total") %>%
    pivot_longer(c("cement", "chemicals", "steel", "otherInd"),
                 names_to = "subsector") %>%
    filter(0 != .data$value) %>%
    # calculate share
    group_by(!!!syms(c("region", "year", "subsector"))) %>%
    mutate(share = .data$value / sum(.data$value)) %>%
    ungroup() %>%
    select(-"value")

  ### modify subsector FE share targets ----
  # Default data with no modification will reduce the solids share of other
  # industry in the Non-OECD region by ten percentage points, and increase the
  # electricity share by as much.  The changes are phased in over the time
  # horizon of IEA ETP data (2025–60).
  IEA_ETP_Ind_FE_shares_delta <- tribble(
    ~region,      ~subsector,   ~fety,   ~share.delta,
    'Non-OECD',   'otherInd',   'feel',    0.1,
    'Non-OECD',   'otherInd',   'feso',   -0.1)

  # verify modification sums
  IEA_ETP_Ind_FE_shares_delta %>%
    group_by(.data$region, .data$subsector) %>%
    summarise(share.delta.sum = sum(.data$share.delta), .groups = 'drop') %>%
    verify(0 == .data$share.delta.sum) %>%
    invisible()

  IEA_ETP_Ind_FE_shares <- IEA_ETP_Ind_FE_shares %>%
    left_join(
      # expand modifications to phase in from 0 to full over the time horizon of
      # IEA_ETP_Ind_FE_shares
      IEA_ETP_Ind_FE_shares_delta %>%
        left_join(
          IEA_ETP_Ind_FE_shares %>%
            select(-'share') %>%
            filter(.data$year %in% range(.data$year)),

          c('region', 'subsector', 'fety')
        ) %>%
        mutate(share.delta = ifelse(min(.data$year) == .data$year,
                                    0,
                                    .data$share.delta)) %>%
        interpolate_missing_periods(year = unique(IEA_ETP_Ind_FE_shares$year),
                                    value = 'share.delta'),

      c('region', 'subsector', 'fety', 'year'),

    ) %>%
    replace_na(list(share.delta = 0)) %>%
    # add share modifications
    mutate(share = .data$share + .data$share.delta) %>%
    select(-'share.delta')

  ### split feel shares and extend to SSP scenarios ----
  IEA_ETP_Ind_FE_shares <- bind_rows(
    # all pf that don't need splitting
    IEA_ETP_Ind_FE_shares %>%
      semi_join(
        industry_subsectors_en %>%
          distinct(.data$pf) %>%
          separate("pf", c("fety", "subsector"), sep = "_",
                   extra = "merge"),

        c("fety", "subsector")
      ) %>%
      unite("pf", c("fety", "subsector"), sep = "_") %>%
      # extend to SSP scenarios
      mutate(scenario = "SSP1") %>%
      complete(nesting(!!sym("year"), !!sym("region"), !!sym("pf"),
                       !!sym("share")),
               scenario = unique(sub("\\..*$", "",
                                     getNames(industry_subsectors_ue)))),

    # split feel_steel into primary and secondary steel
    IEA_ETP_Ind_FE_shares %>%
      filter("feel" == .data$fety, "steel" == .data$subsector) %>%
      select(-"fety", -"subsector") %>%
      inner_join(
        industry_subsectors_ue %>%
          `[`(, , "ue_steel_", pmatch = TRUE) %>%
          as.data.frame() %>%
          as_tibble() %>%
          select(iso3c = "Region", year = "Year", scenario = "Data1",
                 pf = "Data2", value = "Value") %>%
          character.data.frame() %>%
          mutate(year = as.integer(as.character(.data$year))) %>%
          inner_join(
            toolGetMapping(name = "regionmappingOECD.csv",
                           type = "regional", where = "mappingfolder") %>%
              as_tibble() %>%
              select(iso3c = "CountryCode", region = "RegionCode"),

            "iso3c"
          ) %>%
          group_by(!!!syms(c("region", "year", "scenario", "pf"))) %>%
          summarise(value = sum(.data$value), .groups = "drop"),

        c("region", "year")
      ) %>%
      pivot_wider(names_from = "pf") %>%
      # as above, assume that secondary steel production is nine times as
      # electricity intensive (not energy intensive!) as primary production,
      # since detailed data is missing so far
      # reduce primary steel electricity share accordingly; secondary steel
      # electricity share is 1 since only electricity is assumed to be used
      mutate(
        feel_steel_primary        = .data$share
        - ((9 * .data$ue_steel_secondary * .data$share)
            / (9 * .data$ue_steel_secondary + .data$ue_steel_primary)
        ),
        feel_steel_secondary = 1) %>%
      select(-"share", -"ue_steel_primary", -"ue_steel_secondary") %>%
      pivot_longer(c("feel_steel_primary", "feel_steel_secondary"),
                   names_to = "pf", values_to = "share")
  )

  ### extend time horizon and convert regions ----
  IEA_ETP_Ind_FE_shares <- IEA_ETP_Ind_FE_shares %>%
    mutate(subsector = sub("^[^_]+_", "", .data$pf),
           subsector = ifelse("steel" == .data$subsector, "steel_primary",
                              .data$subsector)) %>%
    interpolate_missing_periods_(
      periods = list(year = unique(pmax(min(IEA_ETP_Ind_FE_shares$year),
                                        remind_years))),
      value = "share",
      expand.values = TRUE) %>%
    inner_join( # TODO: check many-to-many
      toolGetMapping(name = "regionmappingOECD.csv",
                     type = "regional", where = "mappingfolder") %>%
        as_tibble() %>%
        select(iso3c = "CountryCode", region = "RegionCode"),

      by = "region",
      relationship = 'many-to-many'
    ) %>%
    select("scenario", "iso3c", period = "year", "pf", "subsector",
           "share") %>%
    # weight by subsector activity
    left_join(
      industry_subsectors_ue %>%
        as.data.frame() %>%
        as_tibble() %>%
        select(iso3c = "Region", period = "Year", scenario = "Data1",
               subsector = "Data2", activity = "Value") %>%
        character.data.frame() %>%
        mutate(period = as.integer(as.character(.data$period)),
               subsector = sub("^ue_", "", .data$subsector)),

      c("scenario", "iso3c", "period", "subsector")
    ) %>%
    full_join(region_mapping_21, "iso3c") %>%
    group_by(
      !!!syms(c("scenario", "region", "period", "subsector", "pf"))) %>%
    summarise(
      share = ifelse(0 == sum(.data$activity), 0,
                     sum(.data$share * .data$activity)
                     / sum(.data$activity)),
      .groups = "drop_last") %>%
    # re-normalise shares
    mutate(share = case_when(
      0 == sum(.data$share) ~ 1 / n(),
      TRUE                  ~ .data$share / sum(.data$share))) %>%
    verify(expr = is.finite(.data$share),
           description = paste("Finite IEA ETP industry FE shares after",
                               "time horizon extension.")) %>%
    ungroup()

  ### combine historic and future industry FE shares ----
  industry_subsectors_en_shares <- inner_join(
    industry_subsectors_en_shares %>%
      mutate(scenario = first(IEA_ETP_Ind_FE_shares$scenario)) %>%
      complete(nesting(!!!syms(setdiff(colnames(.), "scenario"))),
               scenario = unique(IEA_ETP_Ind_FE_shares$scenario)) %>%
      interpolate_missing_periods_(
        periods = list(year = unique(c(industry_subsectors_en_shares$year,
                                       remind_years))),
        value = "share",
        expand.values = TRUE) %>%
      select("scenario", "year", "region", "pf", "subsector",
             share.hist = "share") %>%
      # re-normalise steel_primary to extended feel_steel_primary
      group_by(!!!syms(c("scenario", "year", "region", "subsector"))) %>%
      mutate(share.hist = .data$share.hist / sum(.data$share.hist)) %>%
      ungroup(),

    IEA_ETP_Ind_FE_shares %>%
      interpolate_missing_periods_(
        periods = list(period = unique(c(industry_subsectors_en_shares$year,
                                         remind_years))),
        value = "share",
        expand.values = TRUE) %>%
      select("scenario", year = "period", "region", "pf", "subsector",
             share.future  = "share"),

    c("scenario", "year", "region", "pf", "subsector")
  ) %>%
    mutate(foo = pmin(1, pmax(0, (.data$year - 2015) / (2100 - 2015))),
           share = .data$share.hist * (1 - .data$foo)
           + .data$share.future * .data$foo,
           subsector = ifelse("steel" == .data$subsector,
                              "steel_primary", .data$subsector)) %>%
    verify(expr = is.finite(.data$share),
           description = paste("Finite industry FE shares after combining",
                               "historic and future values")) %>%
    select(-"foo", -"share.hist", -"share.future")

  failed_share_sum <- industry_subsectors_en_shares %>%
    group_by(!!!syms(c("scenario", "year", "region", "subsector"))) %>%
    summarise(failed = abs(sum(.data$share) - 1) > 1e-15, .groups = "drop")

  if (any(failed_share_sum$failed)) {
    stop("industry_subsectors_en_shares don't add up to 1.")
  }

  ### extend to H2 and HTH_el shares ----
  # H2 shares grow linearly from 0.1 % to feh2_share_in_fega of fega from
  # 2025 to 2060 and are constant afterwards
  industry_subsectors_en_shares <- bind_rows(
    industry_subsectors_en_shares %>%
      filter(!grepl("^fega", .data$pf)),

    industry_subsectors_en_shares %>%
      filter(grepl("^fega", .data$pf)) %>%
      extract("pf", c("pf.fety", "pf.subsector"), "^([^_]*)_(.*)") %>%
      complete(nesting(!!!syms(setdiff(colnames(.), "pf.fety"))),
               pf.fety = c("fega", "feh2")) %>%
      pivot_wider(names_from = "pf.fety", values_from = "share") %>%
      full_join(
        bind_rows(
          expand_grid(
            region = c('CAZ', 'CHA', 'DEU', 'ECE', 'ECS', 'ENC', 'ESC', 'ESW',
                       'EWN', 'FRA', 'JPN', 'UKI', 'USA'),
            year.start = 2025,
            year.end   = 2060,
            feh2_share_in_fega = 0.35),

          expand_grid(
            region = c('IND', 'LAM', 'MEA', 'NEN', 'NES', 'OAS', 'REF', 'SSA'),
            year.start = 2030,
            year.end   = 2060,
            feh2_share_in_fega = 0.35)
        ),

        'region') %>%
      assert(not_na, everything()) %>%
      mutate(
        feh2 = pmin(.data$feh2_share_in_fega,
                    pmax(0.01,
                           .data$feh2_share_in_fega
                         * (.data$year - .data$year.start)
                         / (.data$year.end - .data$year.start)))
             * .data$fega,
        fega = .data$fega - .data$feh2) %>%
      select(-'year.start', -'year.end', -'feh2_share_in_fega') %>%
      pivot_longer(c("fega", "feh2"), names_to = "pf.fety",
                   values_to = "share") %>%
      unite("pf", c("pf.fety", "pf.subsector"), sep = "_")
  )

  feelhth_share_in_fuel <- 0.08
  # HTH_el shares grow linearly from 0.1 % to feelhth_share_in_fuel of all
  # FE but feel from 2020 to 2050 and are constant afterwards
  industry_subsectors_en_shares <- bind_rows(
    # subsectors w/o HTH_el
    industry_subsectors_en_shares %>%
      filter(!.data$subsector %in% c("chemicals", "otherInd")),

    # WLTH_el
    industry_subsectors_en_shares %>%
      filter(.data$subsector %in% c("chemicals", "otherInd"),
             grepl("^feel", .data$pf)) %>%
      mutate(pf = sub("^feel_", "feelwlth_", .data$pf)),

    # HTH_el based on share of fuels
    industry_subsectors_en_shares %>%
      filter(.data$subsector %in% c("chemicals", "otherInd"),
             !grepl("^feel", .data$pf)) %>%
      mutate(pf = sub("(_[^_]+)$", "", .data$pf)) %>%
      group_by(!!!syms(setdiff(colnames(.), c("pf", "share")))) %>%
      mutate(feelhth = pmin(feelhth_share_in_fuel,
                            pmax(0.01,
                                 feelhth_share_in_fuel
                                 * (.data$year - 2020) / (2050 - 2020)))
             * sum(.data$share),
             share = .data$share
             / sum(.data$share)
             * (sum(.data$share) - .data$feelhth)) %>%
      ungroup() %>%
      pivot_wider(names_from = "pf", values_from = "share") %>%
      pivot_longer(c(-"scenario", -"year", -"region", -"subsector"),
                   names_to = "pf", values_to = "share",
                   values_drop_na = TRUE) %>%
      mutate(pf = paste(.data$pf, .data$subsector, sep = "_"))
  )

  failed_share_sum <- industry_subsectors_en_shares %>%
    group_by(!!!syms(c("scenario", "year", "region", "subsector"))) %>%
    summarise(failed = abs(sum(.data$share) - 1) > 1e-15, .groups = "drop")

  if (any(failed_share_sum$failed)) {
    stop("industry_subsectors_en_shares don't add up to 1.")
  }

  ### calculate industry total FE level ----
  # scale industry subsector total FE by subsector activity and exogenous
  # energy efficiency gains

  specific_FE_limits <- readSource(type = "industry_subsectors_specific",
                                   subtype = "industry_specific_FE_limits",
                                   convert = FALSE) %>%
    madrat_mule()

  industry_subsectors_specific_FE <- calcOutput(
    type = "industry_subsectors_specific", subtype = "FE",
    scenarios = c(getNames(x = industry_subsectors_ue, dim = 1),
                  "SSP2_lowEn"),
    regions = unique(region_mapping_21$region),
    aggregate = FALSE
  ) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(scenario = "Data1", region = "Data2", subsector = "Data3",
           name = "Data4", value = "Value") %>%
    character.data.frame() %>%
    pivot_wider() %>%
    mutate(alpha = .data$alpha * FE_alpha_mod)

  industry_subsectors_specific_energy <- inner_join(
    industry_subsectors_en %>%
      mutate(subsector = sub("^[^_]+_", "", .data$pf),
             subsector = ifelse("steel" == .data$subsector, "steel_primary",
                                .data$subsector),
             scenario = first(IEA_ETP_Ind_FE_shares$scenario)) %>%
      # extend to SSP scenarios
      # TODO: remove this extension, see TODO below
      complete(nesting(!!!syms(setdiff(colnames(.), "scenario"))),
               scenario = unique(industry_subsectors_en_shares$scenario)
      ) %>%
      group_by(!!!syms(c("scenario", "region", "year", "subsector"))) %>%
      summarise(value = sum(.data$value), .groups = "drop"),

    # TODO: track down differences between SDP and SSP scenarios on historical
    # data
    industry_subsectors_ue %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(scenario = "Data1", iso3c = "Region", year = "Year",
             pf = "Data2", level = "Value") %>%
      character.data.frame() %>%
      mutate(year = as.integer(as.character(.data$year))) %>%
      filter(.data$year %in% unique(industry_subsectors_en$year)) %>%
      # aggregate regions
      full_join(region_mapping_21, "iso3c") %>%
      group_by(!!!syms(c("scenario", "region", "year", "pf"))) %>%
      summarise(level = sum(.data$level), .groups = "drop") %>%
      extract("pf", "subsector", "^ue_(.*)$"),

    c("scenario", "region", "year", "subsector")
  ) %>%
    mutate(specific.energy = .data$value / .data$level,
           specific.energy = ifelse(is.finite(.data$specific.energy),
                                    .data$specific.energy, 0)) %>%
    select("scenario", "region", "year", "subsector", "specific.energy")

  # replace 0 specific energy (e.g. primary steel NEN) with global averages
  industry_subsectors_specific_energy <-
    industry_subsectors_specific_energy %>%
    anti_join(
      industry_subsectors_specific_energy %>%
        filter(0 == .data$specific.energy),

      c("scenario", "region", "year", "subsector")
    ) %>%
    bind_rows(
      left_join(
        industry_subsectors_specific_energy %>%
          filter(0 == .data$specific.energy) %>%
          select(-"specific.energy"),

        industry_subsectors_specific_energy %>%
          filter(0 != .data$specific.energy) %>%
          group_by(!!!syms(c("scenario", "year", "subsector"))) %>%
          summarise(specific.energy = mean(.data$specific.energy),
                    .groups = "drop"),

        c("scenario", "year", "subsector")
      )
    ) %>%
    verify(expr = 0 < .data$specific.energy,
           description = "All specific energy factors above 0")

  # replace absurdly high specific energy (e.g. primary steel NEN after IEA
  # 2021 data update) with EUR averages (considered peer-countries to NEN –
  # CHE, GRL, ISL, LIE, NOR, SJM).
  industry_subsectors_specific_energy <- industry_subsectors_specific_energy %>%
    filter("steel_primary" == .data$subsector, 100 < .data$specific.energy) %>%
    select(-"specific.energy") %>%
    left_join(
      industry_subsectors_specific_energy %>%
        filter("steel_primary" == .data$subsector,
               .data$region %in% c("DEU", "ECE", "ECS", "ENC", "ESC", "ESW", "EWN", "FRA", "UKI")) %>%
        group_by(.data$scenario, .data$year, .data$subsector) %>%
        summarise(specific.energy = mean(.data$specific.energy), .groups = "drop"),

      c("scenario", "year", "subsector")
    ) %>%
    overwrite(industry_subsectors_specific_energy, except = "specific.energy")

  # extend time horizon
  industry_subsectors_specific_energy <-
    industry_subsectors_specific_energy %>%
    interpolate_missing_periods_(
      periods = list(year = unique(industry_subsectors_en_shares$year)),
      value = "specific.energy", expand.values = TRUE)

  # correct lower-then-thermodynamic limit projections
  too_low_projections <- industry_subsectors_specific_energy %>%
    left_join(
      specific_FE_limits %>%
        filter("absolute" == .data$type) %>%
        select(-"type"),

      "subsector"
    ) %>%
    filter(2020 < .data$year,
           !is.na(.data$limit),
           .data$specific.energy < .data$limit) %>%
    select("scenario", "region", "subsector", "year")

  if (0 != nrow(too_low_projections)) {
    industry_subsectors_specific_energy <- bind_rows(
      industry_subsectors_specific_energy %>%
        anti_join(
          too_low_projections,

          c("scenario", "region", "subsector", "year")
        ),

      industry_subsectors_specific_energy %>%
        semi_join(
          too_low_projections %>%
            select(-"region"),

          c("scenario", "subsector", "year")
        ) %>%
        anti_join(
          too_low_projections,

          c("scenario", "region", "subsector", "year")
        ) %>%
        group_by(!!!syms(c("scenario", "subsector", "year"))) %>%
        summarise(specific.energy = mean(.data$specific.energy),
                  .groups = "drop") %>%
        full_join(
          too_low_projections,

          c("scenario", "subsector", "year")
        ) %>%
        assert(not_na, everything())
    )
  }

  #### decrease values by alpha p.a. ----
  industry_subsectors_specific_energy <- industry_subsectors_specific_energy %>%
    # calculate default scenario values first
    filter('SSP2' == .data$scenario) %>% # TODO: define default scenario
    left_join(fixing_year, c('scenario', 'region')) %>%
    inner_join(
      industry_subsectors_specific_FE,

      c('scenario', 'region', 'subsector')
    ) %>%
    inner_join(specific_FE_limits, 'subsector') %>%
    # allow for year-specific decreases
    interpolate_missing_periods_(
      periods = list(
        'year' = union(unique(.$year),
                       seq.int(last_empirical_year,
                               max(fixing_year$fixing_year)))),
      value = 'specific.energy',
      method = 'linear') %>%
    mutate(
      specific.energy = case_when(
        'absolute' == .data$type ~
            ( (.data$specific.energy - .data$limit)
            * pmin(1, (1 - .data$alpha) ^ (.data$year - .env$last_empirical_year))
            )
          + .data$limit,

        'relative' == .data$type ~
            ( .data$specific.energy * (1 - .data$limit)
            * pmin(1, (1 - .data$alpha) ^ (.data$year - .env$last_empirical_year))
            )
          + (.data$specific.energy * .data$limit),
        TRUE ~ NA)) %>%
    assert(not_na, everything()) %>%
    ungroup() %>%
    # extend to non-standard scenarios
    select(-'fixing_year', -'alpha', -'scenario') %>%
    left_join(fixing_year, 'region', relationship = 'many-to-many') %>%
    inner_join(
      industry_subsectors_specific_FE,

      c('scenario', 'region', 'subsector')
    ) %>%
    group_by(.data$region, .data$subsector) %>%
    mutate(
      # continue default scenario data until fixing year
      specific.energy = ifelse(
        # TODO: define default scenario
        'SSP2' != .data$scenario & .data$fixing_year < .data$year,
        .data$specific.energy[.data$fixing_year == .data$year],
        .data$specific.energy),
      specific.energy = case_when(
        # TODO: define default scenario
        'SSP2' == .data$scenario ~ .data$specific.energy,

        'absolute' == .data$type ~
          ( (.data$specific.energy - .data$limit)
            * pmin(1,  (1 - .data$alpha) ^ (.data$year - .data$fixing_year))
          )
        + .data$limit,

        'relative' == .data$type ~
          ( .data$specific.energy * (1 - .data$limit)
            * pmin(1, (1 - .data$alpha) ^ (.data$year - .data$fixing_year))
          )
        + (.data$specific.energy * .data$limit),
        TRUE ~ NA)) %>%
    assert(not_na, everything()) %>%
    ungroup() %>%
    select('scenario', 'region', 'year', 'subsector', 'specific.energy')

  ### converge subsector en shares to global value ----
  # calculate global shares, weighted by subsector activity
  industry_subsectors_en_shares_global <- industry_subsectors_en_shares %>%
    inner_join(
      industry_subsectors_ue %>%
        as.data.frame() %>%
        as_tibble() %>%
        select(scenario = "Data1", iso3c = "Region", year = "Year",
               pf = "Data2", level = "Value") %>%
        character.data.frame() %>%
        mutate(year = as.integer(as.character(.data$year))) %>%
        extract("pf", "subsector", "^ue_(.*)$") %>%
        inner_join(region_mapping_21, "iso3c") %>%
        group_by(!!!syms(c("scenario", "region", "year", "subsector"))) %>%
        summarise(level = sum(.data$level), .groups = "drop"),

      c("scenario", "region", "year", "subsector")
    ) %>%
    group_by(!!!syms(c("scenario", "year", "subsector", "pf"))) %>%
    summarise(
      share.global = sum(.data$share * .data$level) / sum(.data$level),
      .groups = "drop_last") %>%
    mutate(share.global = .data$share.global / sum(.data$share.global)) %>%
    ungroup()

  # converge
  industry_subsectors_en_shares <- inner_join(
    industry_subsectors_en_shares,
    industry_subsectors_en_shares_global,

    c("scenario", "year", "subsector", "pf")
  ) %>%
    left_join(fixing_year, by = c('scenario', 'region')) %>%
    assert(not_na, 'fixing_year',
           description = paste('missing fixing_year for scenario in industry',
                               'FE convergence')) %>%
    mutate(
      # converge from 2020 to 2100
      foo = pmin(1, pmax(0, (.data$year - .data$fixing_year) / 80)),
      share = (1 - .data$foo) * .data$share
      # use minimum of regional and global share, so regions doing
      # better than the average don't regress
      + .data$foo       * pmin(.data$share, .data$share.global)) %>%
    select(-"foo", -"share.global") %>%
    # fill possible gaps in the time steps
    interpolate_missing_periods_(
      periods = list(year = remind_years),
      value = "share",
      expand.values = TRUE) %>%
    group_by(!!!syms(c("scenario", "year", "region", "subsector"))) %>%
    mutate(share = .data$share / sum(.data$share)) %>%
    ungroup()

  failed_share_sum <- industry_subsectors_en_shares %>%
    group_by(!!!syms(c("scenario", "year", "region", "subsector"))) %>%
    summarise(failed = abs(sum(.data$share) - 1) > 1e-15, .groups = "drop")

  if (any(failed_share_sum$failed)) {
    stop("industry_subsectors_en_shares don't add up to 1.")
  }

  industry_subsectors_en <- inner_join(
    industry_subsectors_specific_energy %>%
      # expand regions to iso3c
      full_join(region_mapping_21, by = "region",
                relationship = 'many-to-many') %>%
      select(-"region"),

    industry_subsectors_ue %>%
      as.data.frame() %>%
      as_tibble() %>%
      select(scenario = "Data1", iso3c = "Region", year = "Year", pf = "Data2",
             level = "Value") %>%
      character.data.frame() %>%
      mutate(year = as.integer(as.character(.data$year))) %>%
      extract("pf", "subsector", "^ue_(.*)$") %>%
      group_by(!!!syms(c("scenario", "iso3c", "year", "subsector"))) %>%
      summarise(level = sum(.data$level), .groups = "drop"),

    c("scenario", "iso3c", "year", "subsector")
  ) %>%
    mutate(value = .data$level * .data$specific.energy) %>%
    select("scenario", "iso3c", "year", "subsector", "value") %>%
    assert(is.finite, "value") %>%
    inner_join(
      industry_subsectors_en_shares %>%
        full_join(region_mapping_21, by = "region",
                  relationship = 'many-to-many') %>%
        select(-"region"),

      c("scenario", "iso3c", "year", "subsector")
    ) %>%
    group_by(!!!syms(c("scenario", "iso3c", "year", "subsector"))) %>%
    mutate(share = .data$share / sum(.data$share)) %>%
    ungroup() %>%
    mutate(value = .data$value * .data$share) %>%
    select("scenario", region = "iso3c", "year", item = "pf", "value") %>%
    verify(expr = is.finite(.data$value),
           description = "Finite industry_subsectors_en values")

  ### bespoke CHA electricity 2025 ----
  # increase 2025 China industry electricity demand so that it matches
  # extrapolation of 2018-2022 IEA data. This increase is then kept constant
  # for all following time steps
  industry_subsectors_en <- overwrite(
      industry_subsectors_en %>%
        # select countries in CHA region
        semi_join(
          region_mapping_21 %>%
            filter('CHA' == .data$region),

          c('region' = 'iso3c')) %>%
        # select electricity
        filter(grepl('^feel', .data$item)) %>%
        # interpolate between last_empirical_year and 2025
        # (last_empirical_year might not be present in
        # industry_subsectors_en)
        interpolate_missing_periods_(
          periods = list(
            year = c(.$year,
                     seq(from = last_empirical_year, to = 2025, by = 1)) %>%
              unique())) %>%
        left_join(
          # calculate missing electricity "delta" as per Robert's napkin
          # (target: 21 EJ in 2025)
          industry_subsectors_en %>%
            semi_join(
              region_mapping_21 %>%
                filter('CHA' == .data$region),

              c('region' = 'iso3c')
            ) %>%
            filter(grepl('^feel', .data$item),
                   2025 == .data$year) %>%
            group_by(.data$scenario) %>%
            summarise(delta = max(0, 21 - sum(.data$value)),
                      .groups = 'drop'),

          'scenario'
        ) %>%
        # scale delta to 0 in last empirical year and constant after 2025
        mutate(delta = ( .data$delta
                         * (.env$last_empirical_year - .data$year)
                         / (.env$last_empirical_year - 2025)
        ) %>%
          pmax(0) %>%
          pmin(.data$delta)) %>%
        group_by(.data$scenario, .data$year) %>%
        # add (scaled) delta according to country/subsector shares
        mutate(value = .data$value * (1 + .data$delta / sum(.data$value)),
               .keep = 'unused') %>%
        ungroup(),

      industry_subsectors_en,

      except = 'value')

  industry_subsectors_en <- industry_subsectors_en %>%
    as.magpie(spatial = 2, temporal = 3, datacol = 5)


  remind <- mbind(
    industry_subsectors_en[,remind_years,],
    industry_subsectors_ue[,remind_years,])

  # Add SSP2_NAV_all as duplicate of SSP2, if required.
  if ("SSP2_NAV_all" %in% scenarios) remind <- mbind(remind, setItems(remind[, , "SSP2"], 3.1, "SSP2_NAV_all"))
  # Filter by scenario
  remind <-  mselect(remind, scenario = scenarios)

  # ---- _ prepare output ----
  list(x = remind,
       weight = NULL,
       unit = paste0("EJ, except ue_cement (Gt), ue_primary_steel and ",
                     "ue_secondary_steel (Gt) and ue_chemicals and ",
                     "ue_otherInd ($tn)"),
       description = "demand pathways for final energy demand in industry",
       structure.data = "^(SSP[1-5].*|SDP.*)\\.(fe|ue)")
}
