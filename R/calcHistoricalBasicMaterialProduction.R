#' Calculate historical basic material production data for
#' steel (Worldsteel) and cement (USGS)
#'
#' @param subtype Either 'cement' or 'steel'
#'
#' @author Michaja Pehl, Falk Benke
#'
calcHistoricalBasicMaterialProduction <- function(subtype) {

  if (subtype == "cement") {

    x <- readSource(
      type = "USGS", subtype = "cement",
      convert = FALSE
    ) %>%
      quitte::madrat_mule() %>%
      group_by(!!!syms(c("iso3c", "year"))) %>%
      filter(max(.data$reporting.year) == .data$reporting.year) %>%
      ungroup() %>%
      select(-"reporting.year") %>%
      # t/year * 1e-6 Mt/t = Mt/year
      mutate(
        value = .data$value * 1e-6,
        model = "USGS",
        variable = "Production|Industry|Cement (Mt/yr)"
      ) %>%
      select("iso3c", "year", "model", "variable", "value") %>%
      tidyr::complete(
        iso3c = unname(getISOlist()),
        year = unique(.data$year),
        fill = list(
          model = "USGS",
          variable = "Production|Industry|Cement (Mt/yr)",
          value = 0
        )
      ) %>%
      as.magpie(spatial = 1, temporal = 2, tidy = TRUE)

    description = "Historical cement production based on USGS"
    unit = "Mt/yr"
  } else if (subtype == "steel") {
    x <- readSource("worldsteel", convert = FALSE) %>%
      quitte::madrat_mule() %>%
      filter(
        .data$name %in% c(
          "Production in Oxygen-Blown Converters",
          "Production in Open Hearth Furnaces",
          "DRI Production",
          "Production in Electric Arc Furnaces"
        ),
        .data$iso3c %in% (toolGetMapping(
          name = getConfig("regionmapping"),
          type = "regional", where = "mappingfolder"
        ) %>%
          pull("CountryCode"))
      ) %>%
      # kt/year * 1e-3 Mt/kt = Mt/year
      mutate(value = .data$value * 1e-3) %>%
      tidyr::pivot_wider(values_fill = 0) %>%
      mutate(
        `Production|Industry|Steel (Mt/yr)` = .data$`Production in Oxygen-Blown Converters`
        + .data$`Production in Open Hearth Furnaces`
        + .data$`Production in Electric Arc Furnaces`,
        `Production|Industry|Steel|Secondary (Mt/yr)` =
          # Secondary steel production is production from EAF that does not use
          # inputs from DRI.  If mostly DRI is used for EAF, the difference might
          # be negative (different mass bases due to e.g. carbon content), so
          # limit to zero.
          pmax(
            0,
            .data$`Production in Electric Arc Furnaces`
            - .data$`DRI Production`
          ),
        `Production|Industry|Steel|Primary (Mt/yr)` = (.data$`Production|Industry|Steel (Mt/yr)`
                                                       - .data$`Production|Industry|Steel|Secondary (Mt/yr)`
        ),
        source = "Worldsteel"
      ) %>%
      select(
        "iso3c", "year", "source", "Production|Industry|Steel (Mt/yr)",
        "Production|Industry|Steel|Primary (Mt/yr)",
        "Production|Industry|Steel|Secondary (Mt/yr)"
      ) %>%
      tidyr::pivot_longer(c(
        "Production|Industry|Steel (Mt/yr)",
        "Production|Industry|Steel|Primary (Mt/yr)",
        "Production|Industry|Steel|Secondary (Mt/yr)"
      )) %>%
      tidyr::complete(tidyr::nesting(!!!syms(c("year", "source", "name"))),
                      iso3c = toolGetMapping(
                        name = getConfig("regionmapping"),
                        type = "regional", where = "mappingfolder"
                      ) %>%
                        pull("CountryCode"),
                      fill = list(value = 0)
      ) %>%
      as.magpie(spatial = 4, temporal = 1, datacol = ncol(.data))

    getSets(x)[3] <- "model"
    getSets(x)[4] <- "variable"
    description = "Historical steel production based on WorldSteel"
    unit = "Mt/yr"
  } else {
    stop("Invalid subype. Must be either 'cement' or 'steel'.")
  }

  list(
    x = x,
    weight = NULL,
    unit = unit,
    description = description
  )
}
