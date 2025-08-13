#' Calculates chemical energy demand from 2005 to 2020 from chemical production per route (AllChemicalRoute2005_2020) 
#' and specific energy consumption for the different routes (retrieved from IEA_PetrochemEI and other literature sources).
#' The energy demand for OtherChem is calculated as the remaining share of total chemical industry energy demand. 
#' Results are aggregated to the country level.
#' 
#' @author Qianzhi Zhang
#'
#' @export
#' @param CCS boolean parameter whether CCS technologies are considered as such in 2020 or assumed to be technologies without CCS
#' 
calcAllChemicalSpecFeDemand <- function(CCS=FALSE) {
  
  # ---------------------------------------------------------------------------
  # Define Conversion Factor
  #    - Only the factor for MWh to GJ is used.
  # ---------------------------------------------------------------------------
  sm_Mwh_2_GJ <- 3.6  # Convert MWh to GJ
  
  # ---------------------------------------------------------------------------
  # Load Base Data
  #    a) Load chemical route data (2005-2020)
  #    b) Load energy intensity data (IEA_PetrochemEI)
  # ---------------------------------------------------------------------------
  AllChemicalRoute2005_2020 <- calcOutput("AllChemicalRoute2005_2020", warnNA = FALSE, aggregate = TRUE, CCS=CCS) %>% 
    as.data.frame() %>% 
    select(-Cell) %>%
    rename(ChemFlow = Value)
  
  IEA_PetrochemEI <- calcOutput("IEA_PetrochemEI", aggregate = TRUE) %>% 
    as.data.frame() %>% 
    select(-Cell, -Year)
  
  # ---------------------------------------------------------------------------
  # Finalize dataframe with specific energy demand of all technologies: 
  # - Augment IEA_PetrochemEI with additional electricity-related data
  # - Add data from other sources and use IEA_PetrochemEI data for regional scaling
  # - Map technologies to REMIND technologies
  # - Adjust specific values for certain regions and processes to avoid higher bottom-up energy demands than top-down (FeDemandIndustry)
  # ---------------------------------------------------------------------------
  
  # specific electricity demand data is global, i.e. not regionally resolved
  add_rows_tbl <- tibble::tribble(
    ~Data1,                                ~Value,
    "fertProd electricity",                 0.18 * 60 / 28 * sm_Mwh_2_GJ, # Source: Palys23 Section 2.3, Page 6; 60/28 is conversion factor tUrea -> tN
    "mtoMta electricity",                   5, # Source: Bazzanella17 Section 4.5.3
    "stCrLiq electricity",                  0.94, # Source: Spallina17 Table 5
    "stCrNg electricity",                   0.16 * sm_Mwh_2_GJ, # Source: A. Boulamanti17 Table 2
    "amSyCoal electricity",                 3.7, # Source: IEA, The Future of Hydrogen19 PAGE | 4
    "amSyNG electricity",                   0.3, # Source: IEA, The Future of Hydrogen19 PAGE | 4
    "amSyLiq electricity",                  2, # Source: IEA, The Future of Petrochemicals18 Table A4
    "meSySol electricity",                  3.7, # Source: IEA, The Future of Hydrogen19 PAGE | 5
    "meSyNg electricity",                   0.3, # Source: IEA, The Future of Hydrogen19 PAGE | 5
    "meSyLiq electricity",                  2 # Source: IEA, The Future of Petrochemicals18 Table A4
  )
   
  regions <- IEA_PetrochemEI %>% select(Region) %>% distinct()
  expanded_rows <- regions %>%
    crossing(add_rows_tbl)
  
  # Load a mapping file (structuremappingIEA_PetrochemEI.csv) that maps technologies to REMIND technologies
  mapping <- toolGetMapping("structuremappingIEA_PetrochemEI_v2.csv", type = "sectoral", where = "mrindustry")
  
  # Combine with main data and add data from other sources
  # for steam cracking and methanol synthesis the IEA_PetrochemEI data is just used for regional scaling, the energy efficiency values are taken from other sources
  SpecFeDem <- IEA_PetrochemEI %>%
    bind_rows(expanded_rows) %>%
    # for Steamcracking for NG, the energy inputs for feedstock (feli_chemicals) and fuel&steam (fega_chemicals) are given individually; for all other technologies, the energy input includes feedstocks
    bind_rows(
      IEA_PetrochemEI %>% 
        select(Region) %>% distinct() %>%
        mutate(
          Data1 = "Steam cracking, feedstock for NG",
          Value = 61.275 * (IEA_PetrochemEI %>% filter(Data1 == "Steam cracking, fuel & steam") %>% pull(Value)) / #Source: A. Boulamanti and J. A. Moya, Renew. Sustain. Energy Rev., 2017, 68, 1205–1212. Table 2, value for ethane as feedstock
            (IEA_PetrochemEI %>% filter(Region == "USA", Data1 == "Steam cracking, fuel & steam") %>% pull(Value) %>% unique())
        )
    ) %>% 
    bind_rows(
      IEA_PetrochemEI %>% 
        select(Region) %>% distinct() %>%
        mutate(
          Data1 = "Steam cracking, fuel & steam for NG",
          Value = 21 * (IEA_PetrochemEI %>% filter(Data1 == "Steam cracking, fuel & steam") %>% pull(Value)) / #Source: A. Boulamanti and J. A. Moya, Renew. Sustain. Energy Rev., 2017, 68, 1205–1212. Table 2, value for ethane as feedstock, Assumption of fuel oil as heating to balance excess oil inputs in the energy balance sheet
            (IEA_PetrochemEI %>% filter(Region == "USA", Data1 == "Steam cracking, fuel & steam") %>% pull(Value) %>% unique())
        )
    ) %>%
    # Adjust "Steam cracking, fuel & steam" values
    mutate(
      Value = ifelse(
        Data1 == "Steam cracking, fuel & steam",
        Value * (66 / # Source: Spallina17 Table 5, including feedstock
                   (IEA_PetrochemEI %>% filter(Region == "MEA", Data1 == "Steam cracking, fuel & steam") %>% pull(Value) %>% unique())),
        Value
      )
    ) %>%
    # Adjust "Methanol, fuel & steam" values
    mutate(
      Value = ifelse(
        Data1 == "Methanol, fuel & steam",
        Value * (33.9 / # Source: IEA, The Future of Hydrogen19 PAGE | 5, including feedstock
                   (IEA_PetrochemEI %>% filter(Region == "EUR", Data1 == "Methanol, fuel & steam") %>% pull(Value) %>% unique())), 
        Value
      )
    ) %>%
    distinct(Region, Data1, .keep_all = TRUE)%>%
    # map technologies to REMIND technologies
    left_join(mapping, by = c("Data1"="Source"))%>%
    select(-Data1)%>% rename(Data1=Target)%>%
    # Adjust specific values for certain regions and processes to avoid higher bottom-up energy demands than top-down (FeDemandIndustry); otherwise Energy Demand of OtherChem will be negative
    mutate(
      Value = ifelse(Data1 == "stCrNg" & Type == "fehos" & Region == "MEA", 61, Value), #Same as Global
      Value = ifelse(Data1 == "stCrLiq" & Type == "fehos" & Region == "MEA", 56.5, Value), #Same as EUR
      Value = ifelse(Data1 == "amSyNG" & Type == "fegas" & Region == "CHA", 41.6, Value), # Should be lower if it is based on NG, Value same as Global (in IEA_Petrochem data), in the IEA data there is no differentiation between different routes
      Value = ifelse(Data1 == "meSyNg" & Type == "fegas" & Region == "CHA", 33.9, Value) # Should be lower if it is based on NG, Value same as Global, in the IEA data there is no differentiation between different routes
    )
    
  # ---------------------------------------------------------------------------
  # Compute Energy Demand from Chemical Routes and SpecFeDemand and summarize to total energy demand per region, year and FE type
  # ---------------------------------------------------------------------------
  feChemical <- AllChemicalRoute2005_2020 %>%
    left_join(SpecFeDem, by = c("Region", "Data1")) %>%
    mutate(Energy_demand = ChemFlow * Value) %>%
    group_by(Region, Year, Type) %>%
    summarise(Total_Energy_Demand = sum(Energy_demand, na.rm = TRUE), .groups = "drop")%>%
    rename(Data2 = Type)
  
  # ---------------------------------------------------------------------------
  # Load Industry Demand Data for Chemicals and map FE types
  # ---------------------------------------------------------------------------
  feIndustry <- calcOutput("FeDemandIndustry", scenarios=c("SSP2"), signif = 4, warnNA = FALSE, aggregate = TRUE)[, 
                                                                                             c("y2005", "y2010", "y2015", "y2020"),
                                                                                             c("SSP2.feelhth_chemicals", "SSP2.feelwlth_chemicals", 
                                                                                               "SSP2.feh2_chemicals", "SSP2.fega_chemicals", 
                                                                                               "SSP2.feli_chemicals", "SSP2.feso_chemicals")
  ] %>%
    as.data.frame() %>%
    select(-Cell) %>%
    mutate(
      Data2 = case_when(
        Data2 %in% c("feelwlth_chemicals", "feelhth_chemicals") ~ "feels",
        Data2 %in% c("feh2_chemicals", "fega_chemicals") ~ "fegas",
        Data2 == "feli_chemicals" ~ "fehos",
        Data2 == "feso_chemicals" ~ "fesos",
        TRUE ~ Data2
      )
    ) %>%
    group_by(Region, Year, Data1, Data2) %>%
    summarise(feChemicals = sum(Value, na.rm = TRUE), .groups = "drop") 
  
  # ---------------------------------------------------------------------------
  # Calculate "OtherChem" Energy Demand.
  #    - Merge with feIndustry and calculate the difference between feIndustry Value and the total energy demand.
  # ---------------------------------------------------------------------------
  OtherChem_FE <- feIndustry %>%
    left_join(feChemical, by = c("Region", "Year", "Data2")) %>%
    mutate(
      Total_Energy_Demand = ifelse(is.na(Total_Energy_Demand), 0, Total_Energy_Demand),
      Total_Difference = feChemicals - Total_Energy_Demand
    ) %>%
    rename(Energy_demand = Total_Difference) %>%
    rename(Type = Data2) %>%
    mutate(Data1 = "chemOld")
  
  # ---------------------------------------------------------------------------
  # Calculate specific Energy Demand of OtherChem and merge with other specific Energy demands (assumed to be constant from 2005-2020)
  # ---------------------------------------------------------------------------
  OtherChem_SpecFeDem <- OtherChem_FE %>%
    left_join(AllChemicalRoute2005_2020, by=c("Region","Year","Data1")) %>%
    mutate(Value = Energy_demand/ChemFlow) %>%
    select(Region, Year, Data1, Type, Value)
  
  years <- OtherChem_SpecFeDem %>% select(Year) %>% distinct()
  
  AllChem_SpecFeDem <- SpecFeDem %>% crossing(years) %>%
    rbind(OtherChem_SpecFeDem) %>%
    mutate(Data2 = "standard") %>%
    select(Region,Year,Data1, Data2, Type, Value)
    
  # ---------------------------------------------------------------------------
  # Aggregate Data to Country Level
  # ---------------------------------------------------------------------------
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  
  x <- as.magpie(AllChem_SpecFeDem, spatial = 1, temporal = 2)
  x <- toolAggregate(
    x,
    rel = map,
    dim = 1,
    from = "RegionCode",
    to = "CountryCode",
    weight = NULL
  )
  x[is.na(x)] <- 0
  weight <- x # get the same dimensions of the data
  weight[, , ] <- 1
  
  # ---------------------------------------------------------------------------
  # Return Final Aggregated Object with Metadata
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = weight,
    unit = "GJ/t-output",
    description = "Chemicals specific final energy demand per technology, final energy carrier, region and year"
  ))
}
