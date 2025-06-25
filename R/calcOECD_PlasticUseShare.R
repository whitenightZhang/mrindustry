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
  # Calculate share of plastic use by sector (plastics application) from OECD data
  # - Read OECD plastic use outputs at regional level.
  # - Exclude total categories and compute sectoral sums (summarise over all polymers) and shares.
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
  # Replace shares for EU with EU plastic share reference data
  # - Read CSV of PlasticEurope shares 
  # - Map sectors of EU shares to sectors of OECD shares, for mapping "Agriculture" and "Others" to "Others" and "Textiles" use weights from OECD data
  # ---------------------------------------------------------------------------
  eu_share_df <- read.csv(
    "C:/Users/leoniesc/madrat/sources/PlasticEurope/PlasticShare_EU.csv" #C:/Data/madrat/sources/PlasticEurope/PlasticShare_EU.csv
  ) %>%
    tidyr::pivot_longer(
      cols = -Year,
      names_to = "Type",
      values_to = "Share"
    ) %>%
    dplyr::filter(Type != "Total.Demand..Mt.") %>%
    dplyr::mutate(Year = as.integer(as.character(Year)))

  sector_map <- toolGetMapping(
    "structuremappingPlasticShare.csv", type = "sectoral", where = "mrindustry"
  )
  y_eu <- as.magpie(eu_share_df, temporal = 1) / 100
  weights_eu <- as.magpie(
    regional_df[c("Region", "Year", "Data2", "Value_sum")],
    spatial = 1, temporal = 2
  )
  y_agg <- toolAggregate(
    y_eu, rel = sector_map, dim = 3,
    weight = weights_eu["EUR", "y2019", ],
    from = "Source", to = "Target"
  )
  
  x_share <- as.magpie(
    regional_df[c("Region", "Year", "Data2", "share")],
    spatial = 1, temporal = 2
  )
  x_share["EUR", "y2019", ] <- y_agg["EUR", "y2019", ]
  
  # ---------------------------------------------------------------------------
  # Aggregate shares to country level
  # ---------------------------------------------------------------------------
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mrindustry"
  )
  x_country <- toolAggregate(
    x_share, rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode"
  )
  
  # ---------------------------------------------------------------------------
  # Prepare final weight object and return
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


