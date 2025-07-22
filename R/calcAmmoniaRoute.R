#'
#' Calculates ammonia production volumes per production route in Mt for 2015-2020
#' based on IFA total ammonia production volumes (2015-2020) and
#' shares for different ammonia production routes from IEA (2020 and 2017) and China specific data (2020)
#'
#' @author Qianzhi Zhang
#'
#' @export
calcAmmoniaRoute <- function() {
  # ============================================================================
  # Get IFA ammonia production volumes in Kt, convert to Mt and select the years 2015-2020.
  # ============================================================================
  ammonia_production <- (calcOutput("IFA_Chem", subtype = "ammonia_statistics_production", aggregate = TRUE) / 1000) %>%
    .[, c("y2015", "y2016", "y2017", "y2018", "y2019", "y2020"), ] %>%
    as.data.frame()

  # ============================================================================
  # Get IEA shares for different ammonia production routes for the Base Year 2020 (IEA_Ammonia)
  # ============================================================================
  ammonia_share_iea <- calcOutput("IEA_Ammonia", subtype = "BaseYear_2020", aggregate = TRUE)[, "y2020", ] %>%
    as.data.frame() %>%
    # Exclude regions: China (CHA), Japan (JPN), and OAS
    # There is no data on the Asia Pacific Region in IEA_Ammonia, therefore Japan and OAS are taken from IEA_Petrochem (see below); for China there is more specific data (RMI_China)
    filter(!(.[[2]] %in% c("CHA", "JPN", "OAS")))

  # Map Data1 values to REMIND categories
  ammonia_share_iea$Category <- case_when(
    ammonia_share_iea$Data1 %in% c("Coal", "CCU") ~ "amSyCoal",
    ammonia_share_iea$Data1 %in% c("Coal_with_CCS") ~ "amSyCoal_cc", # TODO: Decide if CCS tech is needed in the base year
    ammonia_share_iea$Data1 %in% c("Gas", "Gas_with_CCU", "Pyrolysis") ~ "amSyNG", # Methane pyrolysis: H2 is produced for ammonia production and black carbon for other potential uses
    ammonia_share_iea$Data1 %in% c("Gas_with_CCS") ~ "amSyNG_cc",
    ammonia_share_iea$Data1 %in% c("Oil") ~ "amSyLiq",
    ammonia_share_iea$Data1 %in% c("Electrolysis") ~ "amSyH2"
  )

  # ---------------------------------------------------------------------------
  # Gas with CCS route in 2020 is only employed in the US. The aggregation performed in convertIEA_Ammonia assigned it 
  # to all North American countries. To fix this, the total of "amSyNG_cc" category shares are reassigned to USA and of all other regions set to zero.
  # ---------------------------------------------------------------------------
  am_syngcc_total <- ammonia_share_iea %>%
    filter(Category == "amSyNG_cc") %>%
    summarise(total_value = sum(Value, na.rm = TRUE)) %>%
    pull(total_value)

  ammonia_share_iea <- ammonia_share_iea %>%
    mutate(Value = ifelse(Category == "amSyNG_cc" & Region == "USA", am_syngcc_total,
      ifelse(Category == "amSyNG_cc", 0, Value)
    ))

  # ----------------------------------------------------------------------------
  # Group by Region, Year, and Category, then calculate normalized share (%)
  #   - the convertIEA_Ammonia function currently disaggregates the shares to the country level in a way that they don't sum up to 1
  #   - consequently, also for the H12 regions, they don't sum up to 1, this is corrected here
  # ----------------------------------------------------------------------------
  ammonia_share_iea <- ammonia_share_iea %>%
    group_by(Region, Year, Category) %>%
    summarise(Value = sum(Value, na.rm = TRUE)) %>%
    group_by(Region, Year) %>%
    mutate(normalized_value = (Value / sum(Value, na.rm = TRUE)) * 100) %>%
    ungroup() %>%
    select(-Value, -Year)

  # ============================================================================
  # Get IEA shares for different ammonia production routes for Japan and OAS in 2017 (IEA_Petrochem)
  #   - the output of IEA_Petrochem (subtype="RouteRTS_Ammonia") is Mt of ammonia output per production route
  #   - set amSyCoal to 0 for Japan and OAS because all ammonia production from coal comes from China, by the disaggregation it is falsely allocated to Japan&OAS
  # ============================================================================
  ammonia_share_ieapetro <- calcOutput("IEA_Petrochem", subtype = "RouteRTS_Ammonia", aggregate = TRUE)[c("JPN", "OAS"), "y2017", ] %>%
    as.data.frame()

  # Map Data1 values to REMIND categories for petrochemical routes
  ammonia_share_ieapetro$Category <- case_when(
    ammonia_share_ieapetro$Data1 %in% c("Coal_GS", "CCU") ~ "amSyCoal",
    ammonia_share_ieapetro$Data1 %in% c("NG_SR", "COG_SR", "Bio_GS", "Pyrolysis") ~ "amSyNG",
    ammonia_share_ieapetro$Data1 %in% c("Oil_SR") ~ "amSyLiq"
  )

  # Group by Region, Year, and Category:
  #    - Sum the values, then set amSyCoal values to 0.
  #    - calculate the share per production route for each region-year group.
  ammonia_share_ieapetro <- ammonia_share_ieapetro %>%
    group_by(Region, Year, Category) %>%
    summarise(Value = sum(Value, na.rm = TRUE)) %>% # Sum values by group
    mutate(Value = ifelse(Category == "amSyCoal", 0, Value)) %>% # Set amSyCoal values to 0
    group_by(Region, Year) %>%
    mutate(normalized_value = (Value / sum(Value, na.rm = TRUE)) * 100) %>% # Normalize
    ungroup() %>%
    select(-Value, -Year)

  # ============================================================================
  # Get China-specific ammonia data (Base Year 2020)
  # ============================================================================
  ammonia_share_china <- calcOutput("RMI_China", subtype = "ChemStructure_Ammonia", aggregate = TRUE)["CHA", "y2020", ] %>%
    as.data.frame()

  # Map Data1 values to REMIND categories for China data
  ammonia_share_china$Category <- case_when(
    ammonia_share_china$Data1 %in% c("Coal") ~ "amSyCoal",
    ammonia_share_china$Data1 %in% c("Coal + CCS") ~ "amSyCoal_cc",
    ammonia_share_china$Data1 %in% c("Gas", "Others") ~ "amSyNG",
    ammonia_share_china$Data1 %in% c("Gas + CCS") ~ "amSyNG_cc",
    ammonia_share_china$Data1 %in% c("PtX") ~ "amSyH2"
  )

  # Group by Region, Year, and Category, then normalize the share values
  ammonia_share_china <- ammonia_share_china %>%
    group_by(Region, Year, Category) %>%
    summarise(Value = sum(Value, na.rm = TRUE)) %>%
    group_by(Region, Year) %>%
    mutate(normalized_value = (Value / sum(Value, na.rm = TRUE)) * 100) %>%
    ungroup() %>%
    select(-Value, -Year)

  # ============================================================================
  # Combine all ammonia share data (IEA, IEA Petrochem, and China)
  # ============================================================================
  ammonia_share_all <- rbind(ammonia_share_iea, ammonia_share_ieapetro, ammonia_share_china)

  # ============================================================================
  # Calculate actual ammonia route values
  #   - Join the combined share data with production data.
  #   - Compute the actual value using the normalized share.
  # ============================================================================
  ammonia_route_value <- ammonia_share_all %>%
    left_join(ammonia_production, by = "Region") %>%
    mutate(actual_value = normalized_value * Value / 100) %>%
    select(-Value, -Cell, -Data1, -normalized_value)

  # ============================================================================
  # Load non-aggregated ammonia production data (for weighting purposes)
  # ============================================================================
  ammonia_production_all <- calcOutput("IFA_Chem", subtype = "ammonia_statistics_production", aggregate = FALSE) %>%
    .[, c("y2015", "y2016", "y2017", "y2018", "y2019", "y2020"), ]

  # ============================================================================
  # Load regional mapping for aggregation from regions to countries
  # ============================================================================
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")

  # ============================================================================
  # Convert ammonia route data to a magpie object for spatial and temporal analysis
  # ============================================================================
  x <- as.magpie(ammonia_route_value, spatial = 1, temporal = 3)

  # ============================================================================
  # Aggregate regional data to the country level using the mapping and production weights
  # ============================================================================
  x <- toolAggregate(
    x,
    rel = map,
    dim = 1,
    from = "RegionCode",
    to = "CountryCode",
    weight = ammonia_production_all[unique(map$CountryCode), , ]
  )
  x[is.na(x)] <- 0 # Replace any NA values with 0

  # ============================================================================
  # Return the final object along with metadata
  # ============================================================================
  return(list(
    x = x,
    weight = NULL,
    unit = "MtNH3",
    description = "Ammonia production distribution by production route, derived from IEA, petrochemical, and China-specific data. 
    The data is regionally mapped, weighted by production values, and formatted as a magpie object for energy system modeling."
  ))
}
