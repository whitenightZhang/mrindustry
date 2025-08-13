#' Calculate Country-Level Plastic Waste Trade Shares
#'
#' Read UNCTAD regional plastic waste trade data (exports or imports),
#' fill missing historical and future years, then aggregate to country level for 1990–2100.
#'
#' @param subtype Character; flow to extract:
#'   \itemize{
#'     \item "export"  - Exports of plastic waste
#'     \item "import"  - Imports of plastic waste
#'   }
#' 
#' @author Qianzhi Zhang
#' @export
calcUNCTAD_PlasticWaste <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Load regional plastic waste trade data
  # ---------------------------------------------------------------------------
  region_df <- calcOutput(
    "UNCTAD_PlasticTrade", subtype = "Waste_Region"
  ) %>%
    as.data.frame() %>%
    dplyr::mutate(Year = as.integer(as.character(Year))) %>%
    dplyr::select(Region, Year, Data2, Data1, Value)
  
  # ---------------------------------------------------------------------------
  # Filter for exports or imports
  # ---------------------------------------------------------------------------
  if (subtype == "export") {
    flow_df <- region_df %>% dplyr::filter(Data2 == "Exports") %>% dplyr::select(-Data2)
  } else if (subtype == "import") {
    flow_df <- region_df %>% dplyr::filter(Data2 == "Imports") %>% dplyr::select(-Data2)
  } else {
    stop("Invalid subtype. Choose 'export' or 'import'.")
  }
  
  # ---------------------------------------------------------------------------
  # Fill missing historical years (1990–2004) with 2005 values
  # ---------------------------------------------------------------------------
  base_2005 <- flow_df %>% dplyr::filter(Year == 2005) %>% dplyr::select(-Year)
  hist_years <- 1990:2004
  hist_df <- tidyr::expand_grid(
    Region = unique(flow_df$Region),
    Year   = hist_years,
    Data1  = unique(flow_df$Data1)
  ) %>%
    dplyr::left_join(base_2005, by = c("Region", "Data1"))
  
  # ---------------------------------------------------------------------------
  # Fill future years (2023–2100) with 2022 values
  # ---------------------------------------------------------------------------
  base_2022 <- flow_df %>% dplyr::filter(Year == 2022) %>% dplyr::select(-Year)
  future_years <- 2023:2100
  future_df <- tidyr::expand_grid(
    Region = unique(flow_df$Region),
    Year   = future_years,
    Data1  = unique(flow_df$Data1)
  ) %>%
    dplyr::left_join(base_2022, by = c("Region", "Data1"))
  
  # ---------------------------------------------------------------------------
  # Combine core, historical, and future data, and compute share
  # ---------------------------------------------------------------------------
  core_df <- flow_df %>%
    dplyr::filter(!(Year %in% c(hist_years, future_years))) %>%
    dplyr::mutate(Year = as.integer(as.character(Year)))
  
  full_df <- dplyr::bind_rows(core_df, hist_df, future_df) %>%
    dplyr::mutate(Year = as.integer(Year)) %>%
    dplyr::arrange(Region, Year)
  
  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to country level
  # ---------------------------------------------------------------------------
  x <- as.magpie(full_df %>% dplyr::select(Region, Year, Data1, Value), spatial = 1, temporal = 2)
  region_map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  
  gdp_ssp2 <- calcOutput("GDP", scenario="SSP2", average2020 = FALSE, naming = "scenario", aggregate = FALSE)[,"y2019", "SSP2"]
  x <- toolAggregate(x, rel = region_map, dim = 1, from = "RegionCode", to = "CountryCode", weight = gdp_ssp2[unique(region_map$CountryCode), , ])
  
  # ---------------------------------------------------------------------------
  # Return results
  # ---------------------------------------------------------------------------
  return(list(
    x           = x,
    weight      = NULL,
    unit        = "Mt Plastic",
    description = "Country-level plastic waste trade flow aggregated for 1990–2100."
  ))
}
