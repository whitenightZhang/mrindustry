#' Calculate Country-Level Mechanical Recycling Loss Trajectories
#'
#' Generate time series of mechanical recycling loss trajectories by sector and region,
#' then aggregate to countries for 1990–2100.
#'
#' @author Qianzhi Zhang
#'
#' @export
#' 
calcOECD_PlasticMechLoss <- function() {
  # ---------------------------------------------------------------------------
  # 1. Define sectors and regions
  #    - Retrieve manufacturing sectors (excluding 'Total') and regional codes.
  # ---------------------------------------------------------------------------
  sector_map <- toolGetMapping(
    "structuremappingPlasticManu.csv", type = "sectoral", where = "mrindustry"
  )
  targets <- setdiff(unique(sector_map$Target), "Total")
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mrindustry"
  )
  regions <- unique(region_map$RegionCode)
  
  # ---------------------------------------------------------------------------
  # 2. Define time horizon and loss bounds
  #    - Years: 1990–2100
  #    - Start loss (pre-2020): 0%
  #    - End loss (2100): 5%
  # ---------------------------------------------------------------------------
  years <- 1990:2100
  bounds <- data.frame(
    Target = targets,
    start  = 0.05,
    end    = 0.05,
    stringsAsFactors = FALSE
  )
  
  # ---------------------------------------------------------------------------
  # 3. Construct full dataset and compute loss trajectories
  #    - Expand grid Year × Target × Region
  #    - Merge bounds and interpolate values beyond 2020 to 2100
  # ---------------------------------------------------------------------------
  traj_df <- expand.grid(
    Year   = years,
    Target = targets,
    Region = regions,
    stringsAsFactors = FALSE
  )
  traj_df <- merge(traj_df, bounds, by = "Target")
  traj_df$value <- with(traj_df, ifelse(
    Year <= 2020,
    start,
    start + (Year - 2020) * (end - start) / (2100 - 2020)
  ))
  traj_df <- dplyr::select(traj_df, Region, Year, Target, value)
  
  # ---------------------------------------------------------------------------
  # 4. Convert to MagPIE and aggregate to countries
  # ---------------------------------------------------------------------------
  x <- as.magpie(traj_df, spatial = 1, temporal = 2)
  x <- toolAggregate(
    x, rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode"
  )
  
  # ---------------------------------------------------------------------------
  # 5. Prepare weight object and return
  # ---------------------------------------------------------------------------
  weight <- x
  weight[,] <- 1
  
  return(list(
    x           = x,
    weight      = weight,
    unit        = "% Mechanical Recycling Loss",
    description = "Mechanical recycling loss trajectories aggregated to country level for 1990–2100."
  ))
}


