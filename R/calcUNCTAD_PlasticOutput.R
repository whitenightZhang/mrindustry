#' Calculate Country-Level Plastics Trade for Various Categories
#'
#' Reads UNCTAD plastics trade (exports or imports) data at regional level,
#' fills missing historical years, and aggregates to country level.
#'
#' @param category Character; product category:
#'   \itemize{
#'     \item "final"        - Final plastics
#'     \item "primary"      - Primary plastics
#'     \item "intermediate" - Intermediate forms of plastic
#'     \item "manufactured" - Intermediate manufactured plastic goods
#'   }
#' @param flow Character; trade flow:
#'   \itemize{
#'     \item "Exports" - Exports
#'     \item "Imports" - Imports
#'   }
#' @author Qianzhi Zhang
#' @export
calcUNCTAD_PlasticOutput <- function(
    category = c("final", "primary", "intermediate", "manufactured"),
    flow     = c("Exports", "Imports")
) {
  # ---------------------------------------------------------------------------
  # 1. Match inputs and map to UNCTAD subtype identifier
  # ---------------------------------------------------------------------------
  category <- match.arg(category)
  flow     <- match.arg(flow)
  subtype_map <- list(
    final        = "Final_Region",
    primary      = "Primary_Region",
    intermediate = "Intermediate_Region",
    manufactured = "Manufactured_Region"
  )
  subtype_region <- subtype_map[[category]]
  
  # ---------------------------------------------------------------------------
  # 2. Load regional trade data for the selected category
  # ---------------------------------------------------------------------------
  region_df <- calcOutput(
    "UNCTAD_PlasticTrade", subtype = subtype_region
  ) %>%
    as.data.frame() %>%
    dplyr::select(Region, Year, Data2, Data1, Value)
  
  # ---------------------------------------------------------------------------
  # 3. Filter by export or import flows
  # ---------------------------------------------------------------------------
  flow_label <- ifelse(flow == "Exports",
                       "Imports",
                       "Exports")
  flow_df <- region_df %>%
    dplyr::filter(Data2 == flow_label) %>%
    dplyr::select(-Data2)
  
  # ---------------------------------------------------------------------------
  # 4. Fill missing historical years (1990–2004) using 2005 values
  # ---------------------------------------------------------------------------
  base_2005 <- flow_df %>%
    dplyr::filter(Year == 2005) %>%
    dplyr::select(-Year)
  hist_years <- 1990:2004
  hist_df <- expand.grid(
    Region = unique(flow_df$Region),
    Year   = hist_years,
    Data1  = unique(flow_df$Data1),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(base_2005, by = c("Region", "Data1"))
  
  # ---------------------------------------------------------------------------
  # 5. Combine original, historical, and future data, then sort by year
  # ---------------------------------------------------------------------------
  core_df <- flow_df %>%
    dplyr::filter(!Year %in% hist_years) %>%
    dplyr::mutate(Year = as.integer(as.character(Year)))
  full_df <- dplyr::bind_rows(core_df, hist_df) %>%
    dplyr::mutate(Year = as.integer(as.character(Year))) %>%
    dplyr::arrange(Year)
  
  # ---------------------------------------------------------------------------
  # 6. Convert to MagPIE and aggregate to country level using GDP weights
  # ---------------------------------------------------------------------------
  x <- as.magpie(
    full_df %>% dplyr::select(Region, Year, Data1, Value),
    spatial = 1, temporal = 2
  )
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mrindustry"
  )
  gdp_ssp2 <- calcOutput(
    "GDP", average2020 = FALSE, naming = "scenario", aggregate = FALSE
  )[, paste0("y", 1990:2022), "SSP2"]
  x <- toolAggregate(
    x,
    rel    = region_map,
    dim    = 1,
    from   = "RegionCode",
    to     = "CountryCode",
    weight = gdp_ssp2[unique(region_map$CountryCode), , ]
  )
  
  # ---------------------------------------------------------------------------
  # 7. Return results
  # ---------------------------------------------------------------------------
  list(
    x           = x,
    weight      = NULL,
    unit        = "Mt Plastic",
    description = sprintf(
      "Country-level %s plastics %s (1990–2100)", category, flow
    )
  )
}
