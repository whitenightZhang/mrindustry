#' Calculate OECD Plastic Use Share by Country
#'
#' Compute the share of plastic use by sector for each country, based on regional OECD data
#' and supplemental EU shares, then aggregate to country level.
#' 
#' @author Qianzhi Zhang
#'
#' @export
calcOECD_PlasticUseShare <- function() {
  # ---------------------------------------------------------------------------
  # 1. Load and preprocess regional use data (2019)
  #    - Read OECD plastic use outputs at regional level.
  #    - Exclude total categories and compute sectoral sums and shares.
  # ---------------------------------------------------------------------------
  regional_df <- calcOutput(
    "OECD_Plastic", subtype = "Use_2019_region", aggregate = TRUE
  ) %>%
    as.data.frame() %>%
    dplyr::filter(Data1 != "Total", Data2 != "Total") %>%
    dplyr::group_by(Region, Year, Data2) %>%
    dplyr::summarise(Value_sum = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::group_by(Region, Year) %>%
    dplyr::mutate(share = Value_sum / sum(Value_sum, na.rm = TRUE)) %>%
    dplyr::ungroup()
  
  # ---------------------------------------------------------------------------
  # 2. Import EU plastic share reference data
  #    - Read CSV of PlasticEurope shares and reshape to long format.
  #    - Exclude total demand and convert Year to integer.
  # ---------------------------------------------------------------------------
  eu_share_df <- read.csv(
    "C:/Data/madrat/sources/PlasticEurope/PlasticShare_EU.csv"
  ) %>%
    tidyr::pivot_longer(
      cols = -Year,
      names_to = "Type",
      values_to = "Share"
    ) %>%
    dplyr::filter(Type != "Total.Demand..Mt.") %>%
    dplyr::mutate(Year = as.integer(as.character(Year)))
  
  # ---------------------------------------------------------------------------
  # 3. Map EU shares to MagPIE and weight by regional totals
  #    - Convert EU share dataframe to MagPIE (fraction).
  #    - Compute EU weights from regional sectoral sums.
  # ---------------------------------------------------------------------------
  sector_map <- toolGetMapping(
    "structuremappingPlasticShare.csv", type = "sectoral", where = "mrindustry"
  )
  y_eu <- as.magpie(eu_share_df, temporal = 1) / 100
  weights_eu <- as.magpie(
    regional_df[c("Region", "Year", "Data2", "Value_sum")],
    spatial = 1, temporal = 2
  )
  
  # ---------------------------------------------------------------------------
  # 4. Replace EUR region share with EU reference
  #    - Aggregate EU shares by sector mapping and assign to EUR for 2019.
  # ---------------------------------------------------------------------------
  x_share <- as.magpie(
    regional_df[c("Region", "Year", "Data2", "share")],
    spatial = 1, temporal = 2
  )
  y_agg <- toolAggregate(
    y_eu, rel = sector_map, dim = 3,
    weight = weights_eu["EUR", "y2019", ],
    from = "Source", to = "Target"
  )
  x_share["EUR", "y2019", ] <- y_agg["EUR", "y2019", ]
  
  # ---------------------------------------------------------------------------
  # 5. Aggregate shares to country level
  #    - Use regional-to-country mapping to disaggregate region shares.
  # ---------------------------------------------------------------------------
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mrindustry"
  )
  x_country <- toolAggregate(
    x_share, rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode"
  )
  
  # ---------------------------------------------------------------------------
  # 6. Prepare final weight object and return
  #    - Set all aggregation weights to 1.
  # ---------------------------------------------------------------------------
  weight <- x_country
  weight[,] <- 1
  
  return(list(
    x = x_country,
    weight = weight,
    unit = "%",
    description = "Sectoral plastic use shares aggregated to country level for 2019."
  ))
}


