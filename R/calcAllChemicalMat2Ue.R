#'
#' @author Qianzhi Zhang
#'
#' @export
calcAllChemicalMat2Ue <- function() {

  # ---------------------------------------------------------------------------
  # 1. Define Material-to-UE Conversion Factors
  #    - p37_mat2ue: Conversion factors (mat2ue) for selected products.
  #      The conversion factors are expressed in 2017$/kg or 2017$/kgN.
  # ---------------------------------------------------------------------------
  
  p37_mat2ue <- data.frame(
    Product = c("hvc", "fertilizer", "methFinal", "ammoFinal"),
    mat2ue = c(0.66, 0.73, 0.37, 0.69),  # Conversion factors
    Unit = c("2017$/kg", "2017$/kgN", "2017$/kg", "2017$/kg")
  )
  
  # ---------------------------------------------------------------------------
  # 2. Calculate Chemical Flow UE for 2020
  #    - Retrieve AllChemicalFlow data for 2020, remove unwanted columns, and filter out 
  #      rows for 'ammonia' and 'methanol' (since these are not processed here).
  #    - Join with the conversion factors and calculate the material-to-UE value.
  # ---------------------------------------------------------------------------
  
  feIndustry <- calcOutput("FeDemandIndustry", warnNA = FALSE, aggregate = TRUE)[,, "gdp_SSP2.ue_chemicals"] %>%
    as.data.frame() %>%
    select(-Cell) %>%
    mutate(Year = as.numeric(as.character(Year))) %>%   # Convert factor to character then numeric
    filter(Year %in% c(2020,2025,2030,2035,2040,2045,2050)) %>%
    group_by(Region) %>%
    mutate(Ratio = Value / Value[Year == 2020]) %>%
    ungroup()
  
  # ---------------------------------------------------------------------------
  # 2. Calculate Chemical Flow UE for 2020
  #    - Retrieve AllChemicalFlow data for 2020, remove unwanted columns, and filter out 
  #      rows for 'ammonia' and 'methanol' (since these are not processed here).
  #    - Join with the conversion factors and calculate the material-to-UE value.
  # ---------------------------------------------------------------------------
  IEA_Petrochem_methanol <- calcOutput("IEA_Petrochem", subtype ="production5type_Methanol", aggregate = TRUE)[,,] %>%
    as.data.frame() %>%
    select(-Cell, -Data1) %>%
    filter(!Year %in% c("X2015")) %>%
    mutate(Year = as.numeric(gsub("X", "", Year))) %>%
    group_by(Region) %>%
    # Ensure rows for 2017, 2020, and 2025 exist (if 2020 is missing, it will be created)
    tidyr::complete(Year = c(2017, 2020, 2025)) %>%
    arrange(Region, Year) %>%
    # For each region, interpolate the missing 2020 value if necessary
    mutate(Value = ifelse(is.na(Value) & Year == 2020,
                          Value[Year == 2017] + (Value[Year == 2025] - Value[Year == 2017]) * (2020 - 2017) / (2025 - 2017),
                          Value)) %>%
    ungroup() %>%
    group_by(Region) %>%
    # Use the 2020 value as the baseline (i.e. Ratio = 1 for 2020)
    mutate(Ratio = Value / Value[Year == 2020]) %>%
    ungroup()%>%
    mutate(Data1 = "methanol") %>%
    filter(!Year %in% 2017)
  
  IEA_Petrochem_ammonia <- calcOutput("IEA_Petrochem", subtype ="production5type_Ammonia", aggregate = TRUE)[,,] %>%
    as.data.frame() %>%
    select(-Cell, -Data1) %>%
    filter(!Year %in% c("X2015")) %>%
    mutate(Year = as.numeric(gsub("X", "", Year))) %>%
    group_by(Region) %>%
    # Ensure rows for 2017, 2020, and 2025 exist (if 2020 is missing, it will be created)
    tidyr::complete(Year = c(2017, 2020, 2025)) %>%
    arrange(Region, Year) %>%
    # For each region, interpolate the missing 2020 value if necessary
    mutate(Value = ifelse(is.na(Value) & Year == 2020,
                          Value[Year == 2017] + (Value[Year == 2025] - Value[Year == 2017]) * (2020 - 2017) / (2025 - 2017),
                          Value)) %>%
    ungroup() %>%
    group_by(Region) %>%
    # Use the 2020 value as the baseline (i.e. Ratio = 1 for 2020)
    mutate(Ratio = Value / Value[Year == 2020]) %>%
    ungroup()%>%
    mutate(Data1 = "ammonia") %>%
    filter(!Year %in% 2017)
  
  
  IEA_Petrochem_hvc <- (
    calcOutput("IEA_Petrochem", subtype = "production5type_Ethylene", aggregate = TRUE) +
      calcOutput("IEA_Petrochem", subtype = "production5type_Propylene", aggregate = TRUE) +
      calcOutput("IEA_Petrochem", subtype = "production5type_BTX", aggregate = TRUE)
  ) %>%
    as.data.frame() %>%
    select(-Cell, -Data1) %>%
    filter(!Year %in% c("X2015")) %>%
    mutate(Year = as.numeric(gsub("X", "", Year))) %>%
    group_by(Region) %>%
    # Ensure rows for 2017, 2020, and 2025 exist (if 2020 is missing, it will be created)
    tidyr::complete(Year = c(2017, 2020, 2025)) %>%
    arrange(Region, Year) %>%
    # For each region, interpolate the missing 2020 value if necessary
    mutate(Value = ifelse(is.na(Value) & Year == 2020,
                          Value[Year == 2017] + (Value[Year == 2025] - Value[Year == 2017]) * (2020 - 2017) / (2025 - 2017),
                          Value)) %>%
    ungroup() %>%
    group_by(Region) %>%
    # Use the 2020 value as the baseline (i.e. Ratio = 1 for 2020)
    mutate(Ratio = Value / Value[Year == 2020]) %>%
    ungroup()%>%
    mutate(Data1 = "hvc") %>%
    filter(!Year %in% 2017)
  
  MagPie_Fert <- readRDS("C:/Data/madrat/sources/MagPie_Result/v39kHRc1000_FSDP_reg.rds")%>%
    select(-model)%>%
    select(-version)%>%
    select(-scenset)%>%
    filter(!region %in% "GLO")%>%
    filter(period %in% c(2020,2025,2030,2035,2040,2045,2050)) %>%
    filter(scenario %in% "Bioplastics")%>%
    filter(variable %in% "Resources|Nitrogen|Cropland Budget|Inputs|+|Fertilizer")%>%
    select(-scenario)
  
  mapFSEC <- toolGetMapping("regionmappingFSEC.csv", type = "regional", where = "mrindustry")
  mapH12 <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  
  x <- as.magpie(MagPie_Fert, spatial = 1, temporal = 4)
  MagPie_Fert_249 <- toolAggregate(x, rel = mapFSEC, dim = 1, from = "RegionCode", to = "CountryCode")
  MagPie_Fert_249 <- toolAggregate(MagPie_Fert_249, rel = mapH12, dim = 1, from = "CountryCode", to = "RegionCode") %>%
    as.data.frame() %>%
    select(-Cell,-Data2)%>%
    mutate(Year = as.numeric(as.character(Year))) %>%
    group_by(Region) %>%
    mutate(Ratio = Value / Value[Year == 2020]) %>%
    ungroup()
  
  # Compute new_mat2ue by merging in the conversion factors and replacing NaN fe_change with 1
  merged_data <- rbind(IEA_Petrochem_methanol, IEA_Petrochem_ammonia, IEA_Petrochem_hvc, MagPie_Fert_249) %>%
    dplyr::left_join(feIndustry, by = c("Region", "Year"), suffix = c("", ".fe")) %>%
    dplyr::mutate(fe_change = ifelse(is.nan(Ratio / Ratio.fe), 1, Ratio / Ratio.fe)) %>%
    mutate(Data1 = case_when(
      Data1 == "ammonia" ~ "ammoFinal",
      Data1 == "methanol" ~ "methFinal",
      Data1 == "Resources|Nitrogen|Cropland Budget|Inputs|+|Fertilizer" ~ "fertilizer",
      TRUE ~ Data1
    ))%>%
    dplyr::left_join(p37_mat2ue, by = c("Data1" = "Product")) %>%
    dplyr::mutate(new_mat2ue = mat2ue / fe_change)
  
  # Extend the data: For each Region and Data1 group, ensure rows exist for 2050, 2055, ..., 2100.
  final_data <- merged_data %>%
    mutate(all_in = "ue_chemicals"
    ) %>%
    select(Region,Year,Data1,all_in,new_mat2ue)
  

  x <- as.magpie(final_data, spatial = 1, temporal = 2)
  x <- toolAggregate(x, rel = mapH12, dim = 1, from = "RegionCode", to = "CountryCode")
  
  # ---------------------------------------------------------------------------
  # 7. Set Weighting and Return Final Output
  #    - Create a weight object with the same dimensions as 'x' (all values set to 1).
  #    - Return the aggregated magpie object along with metadata.
  # ---------------------------------------------------------------------------
  weight <- x  # Copy dimensions from x
  weight[, , ] <- 1
  
  return(list(
    x = x,
    weight = weight,
    unit = "2005$/kg or 2005$/kgN",  # Specify units based on conversion factors
    description = "Calculates the unit energy (UE) share of chemicals by applying material-to-UE conversion factors to chemical flows and comparing them to industry demand. The result, including a residual 'OtherChem' share, is aggregated to the country level for 2020."
  ))
}

