#' Calculate Country-Level End-of-Life Plastic Fate Shares
#'
#' Compute end-of-life fate ratios of plastics by country,
#' based on OECD regional waste EOL data (1990–2019).
#'
#' @author Qianzhi Zhang
#'
#' @export
#' 
calcOECD_PlasticEoL <- function() {
  # ---------------------------------------------------------------------------
  # Load and clean regional EoL data (1990–2019)
  #    - Read OECD waste end-of-life outputs by region.
  #    - Exclude totals and not applicable categories.
  # ---------------------------------------------------------------------------
  eps <- 1e-9
  eol_df <- calcOutput(
    "OECD_Plastic", subtype = "WasteEOL_1990-2019_region", aggregate = TRUE
  ) %>%
    as.data.frame() %>%
    dplyr::filter(!Data1 %in% c("Total", "Not applicable")) %>%
    dplyr::select(-Cell, -Data2)
  
  # ---------------------------------------------------------------------------
  # Calculate per-region fate ratios
  #    - Sum values per region-year and compute ratio for each fate category.
  # ---------------------------------------------------------------------------
  eol_df <- eol_df %>%
    dplyr::group_by(Region, Year) %>%
    dplyr::mutate(
      total = sum(Value, na.rm = TRUE),
      ratio = Value / (total + eps)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-total, -Value) %>%
    dplyr::rename(EoL_Ratio = ratio)
  
  # ---------------------------------------------------------------------------
  # Aggregate ratios to country level
  #    - Convert to MagPIE and apply regional-to-country mapping.
  # ---------------------------------------------------------------------------
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mrindustry"
  )
  x <- as.magpie(eol_df, spatial = 1, temporal = 2)
  x <- toolAggregate(
    x, rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode"
  )
  
  # ---------------------------------------------------------------------------
  # Prepare weight object
  #    - Use equal weights (1) for all country-fate combinations.
  # ---------------------------------------------------------------------------
  weight <- x
  weight[,] <- 1
  
  # ---------------------------------------------------------------------------
  # Return results
  # ---------------------------------------------------------------------------
  return(list(
    x           = x,
    weight      = weight,
    unit        = "%",
    description = "End-of-life fate ratios of plastic aggregated to country level."
  ))
}


