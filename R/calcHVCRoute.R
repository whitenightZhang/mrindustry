#' Calculates HVC production volumes per production route in Mt for 2015-2020
#' based on IEA The Future of Petrochemicals (2018) 
#' 
#' @author Qianzhi Zhang
#'
#' @export
#' @importFrom zoo na.approx
calcHVCRoute <- function() {
  
  # ----------------------------------------------------------
  # Load IEA Petrochemical Production Data for BTX, Propylene, and Ethylene
  #    - Retrieve production data for selected years.
  # ----------------------------------------------------------
  BTX_IEA <- calcOutput("IEA_Petrochem", subtype = "production5type_BTX", aggregate = TRUE) %>%
    .[, c("X2015", "X2017", "X2025"), ] %>%
    as.data.frame()
  
  Propylene_IEA <- calcOutput("IEA_Petrochem", subtype = "production5type_Propylene", aggregate = TRUE) %>%
    .[, c("X2015", "X2017", "X2025"), ] %>%
    as.data.frame()
  
  Ethylene_IEA <- calcOutput("IEA_Petrochem", subtype = "production5type_Ethylene", aggregate = TRUE) %>%
    .[, c("X2015", "X2017", "X2025"), ] %>%
    as.data.frame()
  
  # ----------------------------------------------------------
  # Define a helper function to interpolate missing years and filter data
  #    - Fills in missing years (2015-2020) per Region and source using linear interpolation.
  # ----------------------------------------------------------
  interpolate_and_filter <- function(df) {
    df %>%
      complete(Year = 2015:2020, nesting(Region, source)) %>%  # Fill missing years for each Region and source
      arrange(Region, source, Year) %>%                         # Sort by Region, source, and Year
      group_by(Region, source) %>%                              # Group for interpolation
      mutate(Value = na.approx(Value, na.rm = FALSE)) %>%       # Linear interpolation
      ungroup() %>%
      filter(Year <= 2020)                                      # Keep only years up to 2020
  }
  
  # ----------------------------------------------------------
  # Combine Production Data for HVCs and Interpolate
  #    - Combine the three datasets into one, remove the "X" prefix from year columns,
  #      interpolate missing values, and sum up production by Region and Year.
  # ----------------------------------------------------------
  HVCs_total <- bind_rows(
    BTX_IEA %>% mutate(source = "BTX_IEA"),
    Propylene_IEA %>% mutate(source = "Propylene_IEA"),
    Ethylene_IEA %>% mutate(source = "Ethylene_IEA")
  ) %>%
    mutate(Year = as.numeric(gsub("X", "", Year))) %>%  # Convert year labels to numeric
    select(-Cell, -Data1) %>%                           # Remove unnecessary columns
    interpolate_and_filter() %>%                        # Apply interpolation and filtering
    group_by(Year, Region) %>%                          # Summarise production by Year and Region
    summarise(value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    mutate(source = "Total_HVCs")
  
  # ----------------------------------------------------------
  # Process IEA HVC Share Data for Routes (China-specific)
  #    - Retrieve route share data for 2017.
  #    - For rows with Data1 "MTO_MTA", assign the total to China (CHA) and zero for others.
  #    - Filter for China (CHA) and map the technology types to custom categories.
  # ----------------------------------------------------------
  HVC_share_iea <- calcOutput("IEA_Petrochem", subtype = "RouteRTS_HVCs", aggregate = TRUE)[, "y2017", ] %>%
    as.data.frame() %>%
    group_by(Year) %>%  
    mutate(Value = ifelse(Data1 == "MTO_MTA",
                          ifelse(Region == "CHA", sum(Value[Data1 == "MTO_MTA"], na.rm = TRUE), 0),
                          Value)) %>%  # Assign total MTO_MTA to CHA only
    ungroup() %>%
    filter(Region == "CHA")
  
  # Map Data1 to custom categories
  HVC_share_iea$Category <- case_when(
    HVC_share_iea$Data1 %in% c("MTO_MTA") ~ "mtoMta",
    HVC_share_iea$Data1 %in% c("STC_light") ~ "stCrNg",
    HVC_share_iea$Data1 %in% c("BDH", "STC_heavy", "PDH_NCC") ~ "stCrLiq"
  )
  
  # Group and normalize the share data per Region and Year
  HVC_share_iea <- HVC_share_iea %>%
    group_by(Region, Year, Category) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    group_by(Region, Year) %>%
    mutate(normalized_value = (Value / sum(Value, na.rm = TRUE)) * 100) %>%
    ungroup() %>%
    select(-Value, -Year)
  
  # ----------------------------------------------------------
  # Process IEA HVC Feedstock Share Data
  #    - Retrieve feedstock share data for 2017 and exclude China.
  #    - Map feedstock types to custom categories, normalize the data, and
  #      add zero entries for the "mtoMta" category where needed.
  # ----------------------------------------------------------
  HVC_Feedstockshare_iea <- calcOutput("IEA_Petrochem", subtype = "Feedstock_HVCs", aggregate = TRUE)[, "y2017", ] %>%
    as.data.frame() %>%
    filter(Region != "CHA")  # Exclude China
  
  HVC_Feedstockshare_iea$Category <- case_when(
    HVC_Feedstockshare_iea$Data1 %in% c("Ethane") ~ "stCrNg",
    HVC_Feedstockshare_iea$Data1 %in% c("Bioenergy", "Naphtha", "Other_oil") ~ "stCrLiq"
  )
  
  HVC_Feedstockshare_iea <- HVC_Feedstockshare_iea %>%
    group_by(Region, Year, Category) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    group_by(Region, Year) %>%
    mutate(normalized_value = (Value / sum(Value, na.rm = TRUE)) * 100) %>%
    ungroup() %>%
    select(-Value, -Year) %>%
    bind_rows(
      HVC_Feedstockshare_iea %>%
        distinct(Region) %>%                 # For each unique Region, add a zero entry for mtoMta
        mutate(Category = "mtoMta", normalized_value = 0)
    )
  
  # ----------------------------------------------------------
  # Combine HVC Share Data from Routes and Feedstock
  # ----------------------------------------------------------
  HVC_share_all <- rbind(HVC_share_iea, HVC_Feedstockshare_iea)
  
  # ----------------------------------------------------------
  # Calculate Actual HVC Route Values
  #    - Join the combined share data with the total production data.
  #    - Compute the actual production values based on the normalized shares.
  # ----------------------------------------------------------
  HVC_route_value <- HVC_share_all %>%
    left_join(HVCs_total, by = "Region") %>%
    mutate(actual_value = normalized_value * value / 100) %>%
    select(-value, -normalized_value, -source)

  # ----------------------------------------------------------
  # Load Non-Aggregated HVC Total Production Data for Weighting
  # ----------------------------------------------------------
  HVCs_total_all <- calcOutput("IEA_Petrochem", subtype = "production3type_All", aggregate = FALSE) %>%
    .[, "y2017", "HVCs"]
  
  # ----------------------------------------------------------
  # Aggregate Regional Data to the Country Level
  #    - Convert the data to a magpie object.
  #    - Aggregate regional data to country level using the provided mapping and production weights.
  # ----------------------------------------------------------
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  x <- as.magpie(HVC_route_value, spatial = 1, temporal = 3)
  x <- toolAggregate(
    x,
    rel = map,
    dim = 1,
    from = "RegionCode",
    to = "CountryCode",
    weight = HVCs_total_all[unique(map$CountryCode), , ]
  )
  x[is.na(x)] <- 0  # Replace any NA values with 0
  
  # ----------------------------------------------------------
  # Return Final Aggregated HVC Route Data and Metadata
  # ----------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "MtHVC",
    description = "Aggregated HVC route values from IEA petrochemical data, combining production and feedstock shares with interpolation and regional weighting for energy system analysis."
  ))
}
