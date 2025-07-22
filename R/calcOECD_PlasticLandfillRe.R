#' Calculate Country-Level Plastic Landfill Rate Trajectories
#'
#' Build time series of plastic landfill rates by sector and region
#' using OECD end-of-life (EoL) data, then aggregate to countries for 1990–2100.
#'
#' @author Qianzhi Zhang
#'
#' @export
#' 
calcOECD_PlasticLandfillRe <- function() {
  # ---------------------------------------------------------------------------
  # Define sectors and regions
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
  # Load OECD landfill data and extend to 2020
  #    - Filter 'Landfilled' fate and replicate 2019 to 2020.
  # ---------------------------------------------------------------------------
  landfill_df <- calcOutput("OECD_PlasticEoL", aggregate = TRUE) %>%
    as.data.frame() %>%
    dplyr::select(-Cell) %>%
    dplyr::filter(Data1 == "Landfilled") %>%
    dplyr::select(-Data1) %>%
    dplyr::mutate(Year = as.integer(as.character(Year)))
  landfill_ext <- dplyr::bind_rows(
    landfill_df,
    dplyr::filter(landfill_df, Year == 2019) %>% dplyr::mutate(Year = 2020)
  )
  
  # ---------------------------------------------------------------------------
  # Define target bounds for interpolation
  #    - End share for 2100 set to 20% for all sectors.
  # ---------------------------------------------------------------------------
  bounds <- data.frame(
    Target = targets,
    end    = rep(0.2, length(targets)),
    stringsAsFactors = FALSE
  )
  landfill_ext <- merge(landfill_ext, bounds, by = NULL)
  
  # ---------------------------------------------------------------------------
  # Build final trajectory dataset (1990–2100)
  #    - Keep 1990–2019 values, then interpolate 2020–2100 to 'end'.
  # ---------------------------------------------------------------------------
  pre2020 <- landfill_ext %>% dplyr::filter(Year < 2020)
  post2020 <- landfill_ext %>%
    dplyr::distinct(Region, Target) %>%
    tidyr::crossing(Year = 2020:2100) %>%
    dplyr::left_join(
      landfill_ext %>%
        dplyr::filter(Year == 2020) %>%
        dplyr::select(Region, Target, start_value = Value),
      by = c("Region", "Target")
    ) %>%
    dplyr::left_join(bounds, by = "Target") %>%
    dplyr::mutate(
      Value = ifelse(
        Year == 2020,
        start_value,
        start_value + (Year - 2020) * (end - start_value) / (2100 - 2020)
      )
    ) %>%
    dplyr::select(Region, Target, Year, Value)
  final_df <- dplyr::bind_rows(pre2020, post2020) %>%
    dplyr::select(Region, Year, Target, Value)
  
  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to countries
  # ---------------------------------------------------------------------------
  x <- as.magpie(final_df, spatial = 1, temporal = 2)
  x <- toolAggregate(
    x, rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode"
  )
  
  # ---------------------------------------------------------------------------
  # Prepare weight object and return
  # ---------------------------------------------------------------------------
  weight <- x
  weight[,] <- 1
  return(list(
    x           = x,
    weight      = weight,
    unit        = "% Landfill rate",
    description = "Plastic landfill rate trajectories aggregated to country level for 1990–2100."
  ))
}


