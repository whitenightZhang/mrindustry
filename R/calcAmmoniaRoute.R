#'
#' @author Qianzhi Zhang
#'
#' @export
calcAmmoniaRoute <- function() {
  # ============================================================================
  # 1. Load and preprocess ammonia production data
  #    - Convert production values to thousands and select the years 2015-2020.
  # ============================================================================
  ammonia_production <- (calcOutput("IFA_Chem", subtype = "ammonia_statistics_production", aggregate = TRUE) / 1000) %>%
    .[, c("y2015", "y2016", "y2017", "y2018", "y2019", "y2020"), ] %>%
    as.data.frame()

  # ============================================================================
  # 2. Process IEA Ammonia data (Base Year 2020)
  #    - Exclude certain regions and map technology types to custom categories.
  # ============================================================================
  ammonia_share_iea <- calcOutput("IEA_Ammonia", subtype = "BaseYear_2020", aggregate = TRUE)[, "y2020", ] %>%
    as.data.frame() %>%
    # Exclude regions: China (CHA), Japan (JPN), and OAS
    filter(!(.[[2]] %in% c("CHA", "JPN", "OAS")))

  # Map Data1 values to custom categories
  ammonia_share_iea$Category <- case_when(
    ammonia_share_iea$Data1 %in% c("Coal", "Pyrolysis", "CCU") ~ "amSyCoal",
    ammonia_share_iea$Data1 %in% c("Coal_with_CCS") ~ "amSyCoal_cc", # TODO: Decide if CCS tech is needed in the base year
    ammonia_share_iea$Data1 %in% c("Gas", "Gas_with_CCU") ~ "amSyNG",
    ammonia_share_iea$Data1 %in% c("Gas_with_CCS") ~ "amSyNG_cc",
    ammonia_share_iea$Data1 %in% c("Oil") ~ "amSyLiq",
    ammonia_share_iea$Data1 %in% c("Electrolysis") ~ "amSyH2"
  )

  # ---------------------------------------------------------------------------
  # 2.1 Special handling for the "amSyNG_cc" category:
  #      - Sum the total amSyNG_cc value across all regions.
  #      - Reassign the entire total to USA and set the values for other regions to zero.
  # ---------------------------------------------------------------------------
  am_syngcc_total <- ammonia_share_iea %>%
    filter(Category == "amSyNG_cc") %>%
    summarise(total_value = sum(Value, na.rm = TRUE)) %>%
    pull(total_value)

  ammonia_share_iea <- ammonia_share_iea %>%
    mutate(Value = ifelse(Category == "amSyNG_cc" & Region == "USA", am_syngcc_total,
      ifelse(Category == "amSyNG_cc", 0, Value)
    ))

  # Group by Region, Year, and Category, then calculate normalized share (%)
  ammonia_share_iea <- ammonia_share_iea %>%
    group_by(Region, Year, Category) %>%
    summarise(Value = sum(Value, na.rm = TRUE)) %>%
    group_by(Region, Year) %>%
    mutate(normalized_value = (Value / sum(Value, na.rm = TRUE)) * 100) %>%
    ungroup() %>%
    select(-Value, -Year)

  # ============================================================================
  # 3. Process IEA Petrochem data (for Japan and OAS in 2017)
  #    - Map technology types to categories and adjust values.
  # ============================================================================
  ammonia_share_ieapetro <- calcOutput("IEA_Petrochem", subtype = "RouteRTS_Ammonia", aggregate = TRUE)[c("JPN", "OAS"), "y2017", ] %>%
    as.data.frame()

  # Map Data1 values to custom categories for petrochemical routes
  ammonia_share_ieapetro$Category <- case_when(
    ammonia_share_ieapetro$Data1 %in% c("Coal_GS", "Pyrolysis", "CCU") ~ "amSyCoal",
    ammonia_share_ieapetro$Data1 %in% c("NG_SR", "COG_SR", "Bio_GS") ~ "amSyNG",
    ammonia_share_ieapetro$Data1 %in% c("Oil_SR") ~ "amSyLiq"
  )

  # Group by Region, Year, and Category:
  #    - Sum the values, then set amSyCoal values to 0.
  #    - Normalize the values within each region-year group.
  ammonia_share_ieapetro <- ammonia_share_ieapetro %>%
    group_by(Region, Year, Category) %>%
    summarise(Value = sum(Value, na.rm = TRUE)) %>% # Sum values by group
    mutate(Value = ifelse(Category == "amSyCoal", 0, Value)) %>% # Set amSyCoal values to 0
    group_by(Region, Year) %>%
    mutate(normalized_value = (Value / sum(Value, na.rm = TRUE)) * 100) %>% # Normalize
    ungroup() %>%
    select(-Value, -Year)

  # ============================================================================
  # 4. Process China-specific ammonia data (Base Year 2020)
  #    - Map technology types to custom categories and calculate normalized shares.
  # ============================================================================
  ammonia_share_china <- calcOutput("RMI_China", subtype = "ChemStructure_Ammonia", aggregate = TRUE)["CHA", "y2020", ] %>%
    as.data.frame()

  # Map Data1 values to categories for China data
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
  # 5. Combine all ammonia share data (IEA, IEA Petrochem, and China)
  # ============================================================================
  ammonia_share_all <- rbind(ammonia_share_iea, ammonia_share_ieapetro, ammonia_share_china)

  # ============================================================================
  # 6. Calculate actual ammonia route values
  #    - Join the combined share data with production data.
  #    - Compute the actual value using the normalized share.
  # ============================================================================
  ammonia_route_value <- ammonia_share_all %>%
    left_join(ammonia_production, by = "Region") %>%
    mutate(actual_value = normalized_value * Value / 100) %>%
    select(-Value, -Cell, -Data1, -normalized_value)

  # ============================================================================
  # 7. Load non-aggregated ammonia production data (for weighting purposes)
  # ============================================================================
  ammonia_production_all <- calcOutput("IFA_Chem", subtype = "ammonia_statistics_production", aggregate = FALSE) %>%
    .[, c("y2015", "y2016", "y2017", "y2018", "y2019", "y2020"), ]

  # ============================================================================
  # 8. Load regional mapping for aggregation from regions to countries
  # ============================================================================
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")

  # ============================================================================
  # 9. Convert ammonia route data to a magpie object for spatial and temporal analysis
  # ============================================================================
  x <- as.magpie(ammonia_route_value, spatial = 1, temporal = 3)

  # ============================================================================
  # 10. Aggregate regional data to the country level using the mapping and production weights
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
  # 11. Return the final object along with metadata
  # ============================================================================
  return(list(
    x = x,
    weight = NULL,
    unit = "MtNH3",
    description = "Ammonia production distribution by production route, derived from IEA, petrochemical, and China-specific data. 
    The data is regionally mapped, weighted by production values, and formatted as a magpie object for energy system modeling."
  ))
}
