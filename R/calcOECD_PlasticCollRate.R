#'Calculate Country-Level Plastic Collection Rate Trajectories
#'
#' Build time series of plastic collection rates by sector and region,
#' apply fixed and interpolated values, then aggregate to countries for 1990–2100.
#'
#' @author Qianzhi Zhang
#'
#' @export
#' 
calcOECD_PlasticCollRate <- function() {
  # ---------------------------------------------------------------------------
  # Load and clean regional EoL data
  #    - Read end-of-life outputs and exclude non-collection categories.
  # ---------------------------------------------------------------------------
  eol_df <- calcOutput(
    "OECD_PlasticEoL", aggregate = TRUE
  ) %>%
    as.data.frame() %>%
    dplyr::select(-Cell) %>%
    dplyr::filter(!Data1 %in% c("Littered", "Mismanaged")) %>%
    dplyr::mutate(
      Year = as.integer(as.character(Year))
    ) %>%
    dplyr::group_by(Region, Year) %>%
    dplyr::summarise(
      collected = sum(Value, na.rm = TRUE),
      .groups = "drop"
    )
  
  # ---------------------------------------------------------------------------
  # Apply fixed collection rates for China
  #    - Use reported rates for specific years and interpolate.
  # ---------------------------------------------------------------------------
  # source: Assessment of Plastic Stocks and Flows in China: 1978-2017; 1-(Untreatment share)
  fixed_years <- c(1990, 2005, 2010, 2015, 2017)
  fixed_vals  <- c(0.65, 0.68, 0.84, 0.96, 0.98)
  
  # Interpolate for China across full timeline
  china_idx <- eol_df$Region == "CHA"
  interp_china <- approx(
    x = fixed_years, y = fixed_vals,
    xout = eol_df$Year[china_idx], rule = 2
  )$y
  eol_df$collected[china_idx] <- interp_china
  
  # ---------------------------------------------------------------------------
  # Fill 1990–2000 for other regions with 2000 level
  #    - For non-CHA regions, assign 2000 value to 1990–2000 period.
  # ---------------------------------------------------------------------------
  non_cha <- dplyr::filter(eol_df, Region != "CHA")
  value2000 <- non_cha %>%
    dplyr::filter(Year == 2000) %>%
    dplyr::select(Region, val2000 = collected)
  
  eol_df <- eol_df %>%
    dplyr::left_join(value2000, by = "Region") %>%
    dplyr::mutate(
      collected = if_else(
        Region != "CHA" & Year >= 1990 & Year <= 2000,
        val2000, collected
      )
    ) %>%
    dplyr::select(-val2000)
  
  # ---------------------------------------------------------------------------
  # Extend series to 2100 with linear growth to 100%
  #    - Duplicate 2019 as 2020, then interpolate to reach 1.00 by 2100.
  # ---------------------------------------------------------------------------
  base2019 <- eol_df %>% dplyr::filter(Year == 2019)
  ext_df <- dplyr::bind_rows(
    eol_df,
    dplyr::mutate(base2019, Year = 2020)
  )
  target_final <- 1.0
  future_df <- expand.grid(
    Region = unique(ext_df$Region),
    Year   = 2021:2100,
    stringsAsFactors = FALSE
  ) %>%
    dplyr::left_join(
      dplyr::filter(ext_df, Year == 2020) %>%
        dplyr::select(Region, start = collected),
      by = "Region"
    ) %>%
    dplyr::mutate(
      collected = start +
        (Year - 2020) * (target_final - start) / (2100 - 2020)
    ) %>%
    dplyr::select(Region, Year, collected)
  
  final_df <- dplyr::bind_rows(
    dplyr::filter(ext_df, Year <= 2020) %>%
      dplyr::select(Region, Year, collected),
    future_df
  )
  
  # ---------------------------------------------------------------------------
  # Convert to MagPIE and aggregate to countries
  #    - Map regions to countries with equal weights.
  # ---------------------------------------------------------------------------
  x <- as.magpie(final_df, spatial = 1, temporal = 2)
  region_map <- toolGetMapping(
    "regionmappingH12.csv", type = "regional", where = "mrindustry"
  )
  x <- toolAggregate(
    x, rel = region_map, dim = 1,
    from = "RegionCode", to = "CountryCode"
  )
  
  # ---------------------------------------------------------------------------
  # Prepare weight object and return
  #    - Equal weights (1) for all entries
  # ---------------------------------------------------------------------------
  weight <- x
  weight[,] <- 1
  
  return(list(
    x           = x,
    weight      = weight,
    unit        = "% Collection rate",
    description = "Plastic collection rate trajectories aggregated to country level for 1990–2100."
  ))
}
