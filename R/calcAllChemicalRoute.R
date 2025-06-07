#'
#' @author Qianzhi Zhang
#'
#' @export
calcAllChemicalRoute <- function() {
  
  # ---------------------------------------------------------------------------
  # 1. Load Route Data for Individual Chemicals
  #    - Retrieve ammonia, methanol, hvcs, and overall fertilizer data.
  # ---------------------------------------------------------------------------
  ammonia_route <- calcOutput("AmmoniaRoute", aggregate = TRUE) %>%
    as.data.frame() %>%
    select(-Cell)
  
  methanol_route <- calcOutput("MethanolRoute", aggregate = TRUE) %>%
    as.data.frame() %>%
    select(-Cell)
  
  hvc_route <- calcOutput("HVCRoute", aggregate = TRUE) %>%
    as.data.frame() %>%
    select(-Cell)
  
  nfert_all <- calcOutput("FertilizerRoute", aggregate = TRUE) %>% 
    as.data.frame() %>%
    select(-Cell)
  
  # ---------------------------------------------------------------------------
  # 2. Split fertilizer Data into Production and Conversion Ratio
  # ---------------------------------------------------------------------------
  # 2.1 fertilizer Production Data: rename 'NFert_production' to 'fertProd'
  nfert_route <- nfert_all %>% 
    filter(Data1 == "NFert_production") %>%  
    mutate(Data1 = "fertProd")
  
  # 2.2 fertilizer Conversion Ratio Data
  nfert_ratio <- nfert_all %>%
    filter(Data1 == "NFert_ratio")
  
  
  # ---------------------------------------------------------------------------
  # 3. Process Ammonia Route Data
  #    - Sum ammonia production by Region and Year.
  #    - Combine with fertilizer conversion ratio to compute the final ammonia fraction.
  # ---------------------------------------------------------------------------
  ammonia_total <- ammonia_route %>%
    group_by(Region, Year) %>%
    summarise(Total_Ammonia = sum(Value, na.rm = TRUE), .groups = "drop")
  
  ammonia_tofinal <- ammonia_total %>%
    left_join(nfert_ratio, by = c("Region", "Year")) %>%  # Join by Region and Year
    mutate(amToFinal = Total_Ammonia * (1 - Value)) %>%     # Calculate final ammonia after conversion
    select(Region, Year, amToFinal) %>%                     # Keep only relevant columns
    pivot_longer(cols = c(amToFinal), 
                 names_to = "Data1", 
                 values_to = "Value")
  
  
  # ---------------------------------------------------------------------------
  # 4. Process Methanol Route Data
  #    - Sum methanol production by Region and Year.
  #    - Use hvc data for the 'mtoMta' category to adjust the methanol value.
  # ---------------------------------------------------------------------------
  methanol_total <- methanol_route %>%
    group_by(Region, Year) %>%
    summarise(Total_Methanol = sum(Value, na.rm = TRUE), .groups = "drop")
  
  hvc_MTO <- hvc_route %>%
    filter(Data1 == "mtoMta")
  
  methanol_tofinal <- methanol_total %>%
    left_join(hvc_MTO, by = c("Region", "Year")) %>%   # Join by Region and Year
    mutate(meToFinal = Total_Methanol - 2.624 * Value) %>%  # Adjust methanol using a fixed conversion factor (2.624)
    select(Region, Year, meToFinal) %>%                  # Keep only relevant columns
    pivot_longer(cols = c(meToFinal), 
                 names_to = "Data1", 
                 values_to = "Value")
  
  
  # ---------------------------------------------------------------------------
  # 5. Combine All Chemical Routes
  #    - Combine original route data with the adjusted ammonia and methanol outputs.
  # ---------------------------------------------------------------------------
  AllChemical_all <- rbind(ammonia_route, methanol_route, hvc_route, nfert_route, ammonia_tofinal, methanol_tofinal)
  
  # Add an additional column for processing: set 'opmoPrc' to "standard"
  AllChemical_all <- AllChemical_all %>%
    mutate(opmoPrc = "standard")%>%
    select(Region,Year,Data1,opmoPrc,Value)
  
  
  # ---------------------------------------------------------------------------
  # 6. Load Total Chemical Production Data for Weighting
  # ---------------------------------------------------------------------------
  Chemcial_Total <- calcOutput("ChemicalTotal", aggregate = FALSE) %>%
    .[, "y2020", ]
  
  
  # ---------------------------------------------------------------------------
  # 7. Convert Data to a Magpie Object and Aggregate to Country Level
  #    - Convert the combined data to a magpie object.
  #    - Collapse dimensions, select the year 2020, and aggregate using regional mapping.
  # ---------------------------------------------------------------------------
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  x <- as.magpie(AllChemical_all, spatial = 1, temporal = 2, data = 5)
  
  x <- x[, "y2020", ]
  
  x <- toolAggregate(
    x,
    rel = map,
    dim = 1,
    from = "RegionCode",
    to = "CountryCode",
    weight = Chemcial_Total[unique(map$CountryCode), , ]
  )
  x[is.na(x)] <- 0  # Replace NA values with 0
  
  # Convert from Mt to Gt (megatonnes to gigatonnes)
  x <- x / 1000
  
  
  # ---------------------------------------------------------------------------
  # 8. Return Final Object with Metadata
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "Gt Chemical Routes",
    description = "Aggregated chemical route data integrating ammonia, methanol, hvcs, and fertilizer routes. Data are adjusted by conversion factors and aggregated to country level for 2020 (scaled from Mt to Gt)."
  ))
}
