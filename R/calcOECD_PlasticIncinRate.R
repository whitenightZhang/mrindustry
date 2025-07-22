#' Calculate Country-Level Plastic Incineration Rate Trajectories
#'
#' Build time series of plastic incineration rates by sector and region
#' using OECD EoL data and external sources, then aggregate to countries for 1990–2100.
#'
#' @author Qianzhi Zhang
#'
#' @export
#' 
calcOECD_PlasticIncinRate <- function() {
  # ---------------------------------------------------------------------------
  # Define sectors and regions
  #    - Retrieve manufacturing sectors (excluding 'Total') and regional codes.
  # ---------------------------------------------------------------------------
  sector_map <- toolGetMapping("structuremappingPlasticManu.csv", type = "sectoral", where = "mrindustry")
  targets <- setdiff(unique(sector_map$Target), "Total")
  
  region_map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  regions <- unique(region_map$RegionCode)
  
  # ---------------------------------------------------------------------------
  # Load OECD incineration data and extend to 2020
  #    - Filter for 'Incinerated' fate and replicate 2019 to 2020.
  # ---------------------------------------------------------------------------
  incin_df <- calcOutput("OECD_PlasticEoL", aggregate = TRUE) %>%
    as.data.frame() %>%
    dplyr::select(-Cell) %>%
    dplyr::filter(Data1 == "Incinerated") %>%
    dplyr::select(-Data1) %>%
    dplyr::mutate(Year = as.integer(as.character(Year)))
  
  incin_ext <- dplyr::bind_rows(
    incin_df,
    dplyr::filter(incin_df, Year == 2019) %>% dplyr::mutate(Year = 2020)
  )
  
  # ---------------------------------------------------------------------------
  # Compute historical share from external datasets (2005–2020)
  #    - Load EU, China, and US EoL CSVs, compute incineration share per region-year.
  # ---------------------------------------------------------------------------
  # (paths and unit conversions may need adjustment)
  eu <- read.csv("C:/Users/leoniesc/madrat/sources/PlasticEurope/PlasticEol.csv") %>% #C:/Data/madrat/sources/PlasticEurope/PlasticEol.csv
    dplyr::slice(1:15) %>%
    tidyr::pivot_longer(-Year, names_to = "Treatment", values_to = "Value") %>%
    dplyr::mutate(Region = "EUR", Year = as.integer(Year))
  
  cn <- read.csv("C:/Users/leoniesc/madrat/sources/China_CNBS/PlasticEol.csv") %>%
    dplyr::select(-Source.) %>%
    tidyr::pivot_longer(-Year, names_to = "Treatment", values_to = "Value") %>%
    dplyr::mutate(Value = Value / 100, Region = "CHA", Year = as.integer(Year))
  
  us <- read.csv("C:/Users/leoniesc/madrat/sources/US_EPA/PlasticEol.csv") %>%
    dplyr::slice(1:10) %>%
    tidyr::pivot_longer(-Year, names_to = "Treatment", values_to = "Value") %>%
    dplyr::mutate(Value = Value / 1000, Region = "USA", Year = as.integer(Year)) # in thousands of U.S. tons, to convert to metric tons *0.90718
  
  ext_all <- dplyr::bind_rows(eu, cn, us) %>%
    dplyr::filter(Year >= 2005, Year <= 2020) %>%
    dplyr::group_by(Region, Year) %>%
    dplyr::summarise(
      total = sum(Value, na.rm = TRUE),
      inc = sum(Value[Treatment == "Incinerated"], na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(share = inc / total) %>%
    dplyr::select(Region, Year, share)
  
  # ---------------------------------------------------------------------------
  # Merge ext shares into incin_ext, replacing where available
  # ---------------------------------------------------------------------------
  # TODO: interpolate non-OECD data, otherwise only specific years get replaced
  incin_hist <- incin_ext %>%
    dplyr::left_join(ext_all, by = c("Region", "Year")) %>%
    dplyr::mutate(Value = if_else(!is.na(share), share, Value)) %>%
    dplyr::select(Region, Year, Value)
  
  # ---------------------------------------------------------------------------
  # Fill 1990–2000 for non-CHA regions and extend to 2100
  #    - Copy Year 2000 value to 1990–1999; linearly interpolate from 2020 to 2100 to reach target 30%.
  # ---------------------------------------------------------------------------
  # Base 2000
  base2000 <- incin_hist %>%
    dplyr::filter(Year == 2000) %>%
    dplyr::select(Region, val2000 = Value)
  
  hist_ext <- incin_hist %>%
    dplyr::left_join(base2000, by = "Region") %>%
    dplyr::mutate(
      Value = if_else(Region != "CHA" & Year >= 1990 & Year < 2000, val2000, Value)
    ) %>%
    dplyr::select(-val2000)
  
  # Future 2021–2100
  target_share <- 0.30
  fut <- expand.grid(Region = regions, Year = 2021:2100, stringsAsFactors = FALSE) %>%
    dplyr::left_join(
      hist_ext %>% dplyr::filter(Year == 2020) %>% dplyr::select(Region, start = Value),
      by = "Region"
    ) %>%
    dplyr::mutate(
      Value = start + (Year - 2020) * (target_share - start) / (2100 - 2020)
    ) %>%
    dplyr::select(Region, Year, Value)
  
  final_df <- dplyr::bind_rows(
    hist_ext %>% dplyr::filter(Year < 2021),
    fut
  )
  
  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to countries
  # ---------------------------------------------------------------------------
  x <- as.magpie(final_df, spatial = 1, temporal = 2)
  x <- toolAggregate(x, rel = region_map, dim = 1, from = "RegionCode", to = "CountryCode")
  
  # ---------------------------------------------------------------------------
  # Prepare weight object and return
  # ---------------------------------------------------------------------------
  weight <- x
  weight[,] <- 1
  
  return(list(
    x           = x,
    weight      = weight,
    unit        = "% Plastic incineration",
    description = "Plastic incineration rate trajectories aggregated to country level for 1990–2100."
  ))
}

