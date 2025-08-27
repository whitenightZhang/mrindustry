#' Calculate Country-Level Mechanical Recycling Rate Trajectories
#'
#' Generate time series of mechanical recycling rates by sector and region
#' using OECD end-of-life (EoL) data and external sources, then aggregate to countries for 1990–2100.
#'
#' @author Qianzhi Zhang
#'
#' @export
#' 
calcOECD_PlasticMechReRate <- function() {
  # ---------------------------------------------------------------------------
  # Define sectors and regions
  #    - Retrieve manufacturing sectors (excluding 'Total') and regional codes.
  # ---------------------------------------------------------------------------
  sector_map <- toolGetMapping("structuremappingPlasticManu.csv", type = "sectoral", where = "mrindustry")
  targets    <- setdiff(unique(sector_map$Target), "Total")
  region_map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  regions    <- unique(region_map$RegionCode)
  
  # ---------------------------------------------------------------------------
  # Load OECD mechanical recycling data and extend to 2020
  #    - Filter 'Recycled' fate and replicate 2019 to 2020.
  # ---------------------------------------------------------------------------
  mech_df <- calcOutput("OECD_PlasticEoL", aggregate = TRUE) %>%
    as.data.frame() %>%
    dplyr::select(-Cell) %>%
    dplyr::filter(Data1 == "Recycled") %>%
    dplyr::select(-Data1) %>%
    dplyr::mutate(Year = as.integer(as.character(Year)))
  
  mech_ext <- dplyr::bind_rows(
    mech_df,
    dplyr::filter(mech_df, Year == 2019) %>% dplyr::mutate(Year = 2020)
  )
  
  # ---------------------------------------------------------------------------
  # Incorporate external EoL share data (2005–2020)
  #    - Load EU, CNBS, US EPA datasets, compute mechanical recycling share.
  # ---------------------------------------------------------------------------
  eu <- read.csv("C:/Users/leoniesc/madrat/sources/PlasticEurope/PlasticEol.csv") %>%
    dplyr::slice(1:15) %>%
    tidyr::pivot_longer(-Year, names_to = "Treatment", values_to = "Value") %>%
    dplyr::mutate(Region = "EUR", Year = as.integer(Year))
  cn <- read.csv("C:/Users/leoniesc/madrat/sources/China_CNBS/PlasticEol.csv") %>%
    dplyr::select(-Source.) %>%
    tidyr::pivot_longer(-Year, names_to = "Treatment", values_to = "Value") %>%
    dplyr::mutate(Value = Value/100, Region="CHA", Year=as.integer(Year))
  us <- read.csv("C:/Users/leoniesc/madrat/sources/US_EPA/PlasticEol.csv") %>%
    dplyr::slice(1:10) %>%
    tidyr::pivot_longer(-Year, names_to = "Treatment", values_to = "Value") %>%
    dplyr::mutate(Value = Value/1000, Region="USA", Year=as.integer(Year))
  
  ext_combined <- dplyr::bind_rows(eu, cn, us) %>%
    dplyr::filter(Year >= 2005, Year <= 2020) %>%
    dplyr::group_by(Region, Year) %>%
    dplyr::summarise(
      total     = sum(Value, na.rm=TRUE),
      recycled  = sum(Value[Treatment=="Recycled"], na.rm=TRUE),
      .groups   = "drop"
    ) %>%
    dplyr::mutate(share = recycled/total) %>%
    dplyr::select(Region, Year, share)
  
  # ---------------------------------------------------------------------------
  # Merge external shares into mech_ext, replacing where available
  # ---------------------------------------------------------------------------
  mech_hist <- mech_ext %>%
    dplyr::left_join(ext_combined, by=c("Region","Year")) %>%
    dplyr::mutate(Value = dplyr::if_else(!is.na(share), share, Value)) %>%
    dplyr::select(Region, Year, Value)
  
  # ---------------------------------------------------------------------------
  # Fill 1990–1999 with Year-2000 values and extend to 2100
  #    - Copy 2000 value back to 1990–1999; interpolate 2021–2100 to target 40%.
  # ---------------------------------------------------------------------------
  base2000 <- mech_hist %>% dplyr::filter(Year==2000) %>% dplyr::select(Region, v2000=Value)
  hist_ext <- mech_hist %>%
    dplyr::left_join(base2000, by="Region") %>%
    dplyr::mutate(Value=dplyr::if_else(Year<2000, v2000, Value)) %>%
    dplyr::select(-v2000)
  
  future <- expand.grid(Region=regions, Year=2021:2100, stringsAsFactors=FALSE) %>%
    dplyr::left_join(
      dplyr::filter(hist_ext, Year==2020) %>% dplyr::select(Region, start=Value),
      by="Region"
    ) %>%
    dplyr::mutate(Value = start + (Year-2020)*(0.4-start)/(2050-2020)) %>%
    dplyr::select(Region, Year, Value)
  
  final_df <- dplyr::bind_rows(
    hist_ext %>% dplyr::filter(Year<=2020),
    future
  )
  
  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to countries
  # ---------------------------------------------------------------------------
  x <- as.magpie(final_df, spatial=1, temporal=2)
  x <- toolAggregate(x, rel=region_map, dim=1, from="RegionCode", to="CountryCode")
  
  # ---------------------------------------------------------------------------
  # Prepare weight object and return
  # ---------------------------------------------------------------------------
  weight <- x; weight[,] <- 1
  return(list(
    x           = x,
    weight      = weight,
    unit        = "% Mechanical Recycling",
    description = "Mechanical recycling rate trajectories aggregated to country level for 1990–2100."
  ))
}


