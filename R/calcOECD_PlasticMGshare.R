#' Calculate Country-Level Material Good Shares
#'
#' Compute material share of different goods from 2019 use data,
#' aggregate regional shares to country level for 2019.
#'
#' @author Qianzhi Zhang
#'
#' @export
#' 
calcOECD_PlasticMGshare <- function() {
  # ---------------------------------------------------------------------------
  # Load and filter regional use data (2019)
  #    - Read regional use output, exclude 'Total' categories.
  # ---------------------------------------------------------------------------
  use_df <- calcOutput(
    "OECD_Plastic", subtype = "Use_2019_region", aggregate = TRUE
  ) %>%
    as.data.frame() %>%
    dplyr::select(-Cell) %>%
    dplyr::filter(Data1 != "Total", Data2 != "Total")
  
  # ---------------------------------------------------------------------------
  # Compute material shares by good
  #    - Group by region, year, and good (Data2), then calculate share of each subcategory (Data1).
  # ---------------------------------------------------------------------------
  ratio_df <- use_df %>%
    dplyr::group_by(Region, Year, Data2) %>%
    dplyr::mutate(
      total_by_good = sum(Value, na.rm = TRUE),
      MaterialShare = Value / total_by_good
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(Region, Year, Data2, Data1, MaterialShare)
  
  # ---------------------------------------------------------------------------
  # Aggregate shares to country level
  #    - Convert to MagPIE and map regional codes to country codes.
  # ---------------------------------------------------------------------------
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mrindustry"
  )
  x <- as.magpie(ratio_df, spatial = 1, temporal = 2)
  x <- toolAggregate(
    x, rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode"
  )
  
  # ---------------------------------------------------------------------------
  # Prepare weight object and return
  #    - Use equal weights for aggregation.
  # ---------------------------------------------------------------------------
  weight <- x
  weight[,] <- 1
  
  return(list(
    x           = x,
    weight      = weight,
    unit        = "fraction",
    description = "Material share of plastics in different goods aggregated to country level for 2019."
  ))
}


