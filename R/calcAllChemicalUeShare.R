#' Calculates shares of ammoFinal, methFinal, HVC, fertilizer and OtherChem
#' of total chemical UE for 2020-2050 based on the UE shares in 2020 and the 
#' projected relative increases in production (IEA The Future of Petrochemicals) and 
#' total chemical UE (FeDemandIndustry)
#' 
#' @author Qianzhi Zhang
#'
#' @export
calcAllChemicalUeShare <- function() {

  # ---------------------------------------------------------------------------
  # Retrieve baseline ue shares for 2020
  # ---------------------------------------------------------------------------
  
  AllChemicalUe <- calcOutput("AllChemicalUe", aggregate = TRUE)[, "y2020", ] %>%
    as.data.frame() %>%
    select(-Cell,-Year)
  
  # ---------------------------------------------------------------------------
  # Retrieve Chemical production projections for 2020-2050 (extrapolate between 2017 and 2025 if 2020 is missing)
  # - total Chemical UE projection from calcFeDemandIndustry
  # - Methanol, HVC & Ammonia projections from IEA_Petrochem
  # - Fertilizer demand projections from MagPie
  # and calculate change compared to baseline year
  # ---------------------------------------------------------------------------
  
  feIndustry <- calcOutput("FeDemandIndustry", scenarios=c("SSP2"), warnNA = FALSE, aggregate = TRUE)[,, "SSP2.ue_chemicals"] %>%
    as.data.frame() %>%
    select(-Cell) %>%
    mutate(Year = as.numeric(as.character(Year))) %>%   # Convert factor to character then numeric
    filter(Year %in% c(2020,2025,2030,2035,2040,2045,2050)) %>%
    group_by(Region) %>%
    mutate(Ratio = Value / Value[Year == 2020]) %>%
    ungroup()
  
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
  
  MagPie_Fert <- readRDS("C:/Users/leoniesc/madrat/sources/MagPie_Result/v39kHRc1000_FSDP_reg.rds")%>% # "C:/Data/madrat/sources/MagPie_Result/v39kHRc1000_FSDP_reg.rds"
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
  
  # ---------------------------------------------------------------------------
  # Compute future ue shares by dividing the baseline share with the relative change 
  # in chemical production respective to the relative change in UE chemicals demand
  # ---------------------------------------------------------------------------
  merged_data <- rbind(IEA_Petrochem_methanol, IEA_Petrochem_ammonia, IEA_Petrochem_hvc, MagPie_Fert_249) %>%
    dplyr::left_join(feIndustry, by = c("Region", "Year"), suffix = c("", ".fe")) %>%
    dplyr::mutate(fe_change = ifelse(is.nan(Ratio / Ratio.fe), 1, Ratio / Ratio.fe)) %>%
    mutate(Data1 = case_when(
      Data1 == "ammonia" ~ "ammoFinal",
      Data1 == "methanol" ~ "methFinal",
      Data1 == "Resources|Nitrogen|Cropland Budget|Inputs|+|Fertilizer" ~ "fertilizer",
      TRUE ~ Data1
    ))%>%
    dplyr::left_join(AllChemicalUe, by = c("Region","Data1", "Data2"), suffix=c("",".ue")) %>%
    dplyr::mutate(ue_share = Value.ue / fe_change)%>%
    select(Region,Year,Data1,Data2,ue_share)
    
  # ---------------------------------------------------------------------------
  # Account for Residual ("OtherChem") Share
  # ---------------------------------------------------------------------------
  ue_summary <- merged_data %>%
    group_by(Region, Year, Data2) %>%
    summarise(
      ue_sum = sum(ue_share, na.rm = TRUE),
      .groups = "drop"
    )
  
  final_data <- merged_data %>%
    bind_rows(
      ue_summary %>%
        mutate(
          Data1 = "OtherChem",
          ue_share = 1 - ue_sum
        )
    ) %>% select(-ue_sum) 
  
  x <- as.magpie(final_data, spatial = 1, temporal = 2)
  x <- toolAggregate(x, rel = mapH12, dim = 1, from = "RegionCode", to = "CountryCode")
  
  # ---------------------------------------------------------------------------
  # Set Weighting and Return Final Output
  #    - Create a weight object with the same dimensions as 'x' (all values set to 1).
  #    - Return the aggregated magpie object along with metadata.
  # ---------------------------------------------------------------------------
  weight <- x  # Copy dimensions from x
  weight[, , ] <- 1
  
  return(list(
    x = x,
    weight = weight,
    unit = "share", 
    description = "Material ue shares for 2020-2050 on country level."
  ))
}

