#'
#' Calculates methanol production volumes per production route in Mt for 2015-2020
#' based on methanol production volumes (Argus 2018 data, extrapolated to 2015-2020 based on IHS data, and China specific data 2015-2020) and
#' shares for different methanol production routes from IEA (2017) and China specific data (2020)
#'
#' @author Qianzhi Zhang
#'
#' @export
calcMethanolRoute <- function() {
  
  # ---------------------------------------------------------------------------
  # Load Methanol Production Data (Non-China)
  #    - Get production data from "IHS_Meth" for years 2015-2020.
  #    - Exclude China ("CHA") and mark the data with a production identifier.
  # ---------------------------------------------------------------------------
  methanol_production <- calcOutput("IHS_Meth", subtype = "Production_2010-2020", aggregate = TRUE) %>%
    .[, c("y2015", "y2016", "y2017", "y2018", "y2019", "y2020"), ] %>%
    as.data.frame() %>%
    filter(!(.[[2]] %in% c("CHA"))) %>% 
    mutate(Data1 = "Production")
  
  # ---------------------------------------------------------------------------
  # Load Methanol Production Data for China
  #    - Retrieve production data for China ("CHA") from the specified file.
  # ---------------------------------------------------------------------------
  methanol_production_china <- calcOutput("ChinaBaogao", aggregate = TRUE) %>%
    .["CHA", c("y2015", "y2016", "y2017", "y2018", "y2019", "y2020"), "Production"] %>%
    as.data.frame()
  
  # ---------------------------------------------------------------------------
  # Load and Process IEA Petrochemical Methanol Route Data
  #    - Retrieve route data for 2017 from "IEA_Petrochem".
  #    - Exclude China ("CHA") and map technology types to custom categories.
  # ---------------------------------------------------------------------------
  methanol_share_iea <- calcOutput("IEA_Petrochem", subtype = "RouteRTS_Methanol", aggregate = TRUE)[, "y2017", ] %>%
    as.data.frame() %>%
    filter(!(.[[2]] %in% c("CHA")))
  
  # Map technology types to categories for IEA data
  methanol_share_iea$Category <- case_when(
    methanol_share_iea$Data1 %in% c("Coal_GS", "Bio_GS") ~ "meSySol",
    methanol_share_iea$Data1 %in% c("NG_SR", "COG_SR")   ~ "meSyNg",
    methanol_share_iea$Data1 %in% c("Oil_SR")              ~ "meSyLiq"
  )
  
  # Group by Region, Year, and Category, then calculate normalized share (%)
  methanol_share_iea <- methanol_share_iea %>%
    group_by(Region, Year, Category) %>%
    summarise(Value = sum(Value, na.rm = TRUE)) %>%
    group_by(Region, Year) %>%
    mutate(normalized_value = (Value / sum(Value, na.rm = TRUE)) * 100) %>%
    ungroup() %>%
    select(-Value, -Year)
  
  # ---------------------------------------------------------------------------
  # Load and Process China-Specific Methanol Share Data
  #    - Retrieve methanol share data for China ("CHA") from "RMI_China" for 2020.
  #    - Map technology types to custom categories.
  # ---------------------------------------------------------------------------
  methanol_share_china <- calcOutput("RMI_China", subtype = "ChemStructure_Methanol", aggregate = TRUE)["CHA", "y2020", ] %>%
    as.data.frame()
  
  # Map technology types to categories for China data
  methanol_share_china$Category <- case_when(
    methanol_share_china$Data1 %in% c("Coal", "Biomass", "Coal + Green hydrogen") ~ "meSySol",
    methanol_share_china$Data1 %in% c("Coal + CCS")                          ~ "meSySol_cc",
    methanol_share_china$Data1 %in% c("Gas", "Others")                       ~ "meSyNg",
    methanol_share_china$Data1 %in% c("Gas + CCS")                           ~ "meSyNg_cc",
    methanol_share_china$Data1 %in% c("PtX")                                 ~ "meSyH2"
  )
  
  # Group by Region, Year, and Category, then calculate normalized share (%)
  methanol_share_china <- methanol_share_china %>%
    group_by(Region, Year, Category) %>%
    summarise(Value = sum(Value, na.rm = TRUE)) %>%
    group_by(Region, Year) %>%
    mutate(normalized_value = (Value / sum(Value, na.rm = TRUE)) * 100) %>%
    ungroup() %>%
    select(-Value, -Year)
  
  # ---------------------------------------------------------------------------
  # Combine Share Data and Production Data
  #    - Merge IEA and China share data.
  #    - Combine production data from non-China and China sources.
  # ---------------------------------------------------------------------------
  methanol_share_all <- rbind(methanol_share_iea, methanol_share_china)
  methanol_production <- rbind(methanol_production, methanol_production_china)
  
  # Join share data with production data to calculate the actual production values
  methanol_route_value <- methanol_share_all %>%
    left_join(methanol_production, by = "Region") %>% 
    mutate(actual_value = normalized_value * Value / 100) %>%
    select(-Value, -Cell, -Data1, -normalized_value)
  
  # ---------------------------------------------------------------------------
  # Load Non-Aggregated Methanol Production Data (for Weighting)
  # ---------------------------------------------------------------------------
  methanol_production_all <- calcOutput("IHS_Meth", subtype = "Production_2010-2020", aggregate = FALSE) %>%
    .[, c("y2015", "y2016", "y2017", "y2018", "y2019", "y2020"), ]
  
  # ---------------------------------------------------------------------------
  # Load Regional Mapping and Aggregate Data to Country Level
  #    - Convert the data to a magpie object.
  #    - Aggregate regional data to country level using production weights.
  # ---------------------------------------------------------------------------
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry") 
  x <- as.magpie(methanol_route_value, spatial = 1, temporal = 3)
  x <- toolAggregate(
    x,
    rel = map,
    dim = 1,
    from = "RegionCode",
    to = "CountryCode",
    weight = methanol_production_all[unique(map$CountryCode), , ]
  )
  x[is.na(x)] <- 0  # Replace any NA values with 0
  
  # ---------------------------------------------------------------------------
  # Return the Final Aggregated Object and Metadata
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "MtMeOH",
    description = "Methanol production distribution by production route, combining IEA and China data. Regionally aggregated and weighted for energy system modeling."
  ))
}

