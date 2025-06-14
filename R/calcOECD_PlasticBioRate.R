#' Calculate Country-Level Bio-based Plastic Share Trajectories
#'
#' Generate time series of bio-based plastic share trajectories by sector
#' and aggregate from regions to countries for 1990–2100.
#' 
#' @author Qianzhi Zhang
#'
#' @export
#' 
calcOECD_PlasticBioRate <- function() {
  # ---------------------------------------------------------------------------
  # 1. Load sectoral mapping and define sectors
  #    - Retrieve sectoral targets from manufacturing mapping, excluding totals.
  # ---------------------------------------------------------------------------
  sector_map <- toolGetMapping(
    "structuremappingPlasticManu.csv", type = "sectoral", where = "mrindustry"
  )
  targets <- unique(sector_map$Target)
  targets <- setdiff(targets, "Total")
  
  # ---------------------------------------------------------------------------
  # 2. Load regional mapping and define regions
  #    - Retrieve regional codes from mapping.
  # ---------------------------------------------------------------------------
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mrindustry"
  )
  regions <- unique(region_map$RegionCode)
  
  # ---------------------------------------------------------------------------
  # 3. Define time horizon and bio-based share bounds
  #    - Years: 1990–2100
  #    - Starting share (2020): 0
  #    - End share (2050): 20%
  # ---------------------------------------------------------------------------
  years <- 1990:2100
  share_bounds <- data.frame(
    Target = targets,
    start  = 0,
    end    = 0.2
  )
  
  # ---------------------------------------------------------------------------
  # 4. Build full data frame of trajectories
  #    - Expand grid of Year × Sector × Region
  #    - Merge with share bounds and interpolate linearly
  # ---------------------------------------------------------------------------
  traj_df <- expand.grid(
    Year   = years,
    Target = targets,
    Region = regions,
    stringsAsFactors = FALSE
  )
  traj_df <- merge(traj_df, share_bounds, by = "Target")
  traj_df$value <- with(traj_df, ifelse(
    Year <= 2020, start,
    ifelse(
      Year <= 2050,
      start + (Year - 2020) * (end - start) / (2050 - 2020),
      end
    )
  ))
  traj_df <- dplyr::select(traj_df, Region, Year, Target, value)
  
  # ---------------------------------------------------------------------------
  # 5. Convert to MagPIE and aggregate to country level
  #    - Map regional trajectories to countries (equal weights)
  # ---------------------------------------------------------------------------
  x <- as.magpie(traj_df, spatial = 1, temporal = 2)
  x <- toolAggregate(
    x, rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode"
  )
  
  # ---------------------------------------------------------------------------
  # 6. Prepare weight object and return
  #    - Equal weights (1) for all entries
  # ---------------------------------------------------------------------------
  weight <- x
  weight[,] <- 1
  
  return(list(
    x           = x,
    weight      = weight,
    unit        = "% Bio-based Plastic",
    description = "Projected bio-based plastic share by sector, aggregated to country level for 1990–2100."
  ))
}

