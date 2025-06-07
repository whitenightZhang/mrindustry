#'
#' @author Qianzhi Zhang
#'
#' @export
calcAllChemicalEnergy2005_2020 <- function() {
  
  # ---------------------------------------------------------------------------
  # 1. Define Conversion Factor
  #    - Only the factor for MWh to GJ is used.
  # ---------------------------------------------------------------------------
  sm_Mwh_2_GJ <- 3.6  # Convert MWh to GJ
  
  # ---------------------------------------------------------------------------
  # 2. Load Base Data
  #    a) Load chemical route data (2005-2020)
  #    b) Load energy intensity data (IEA_PetrochemEI), excluding "Cell" and "Year".
  # ---------------------------------------------------------------------------
  AllChemicalRoute2005_2020 <- calcOutput("AllChemicalRoute2005_2020", warnNA = FALSE, aggregate = TRUE) %>% 
    as.data.frame() %>% 
    select(-Cell)
  
  IEA_PetrochemEI <- calcOutput("IEA_PetrochemEI", aggregate = TRUE) %>% 
    as.data.frame() %>% 
    select(-Cell, -Year)
  
  # ---------------------------------------------------------------------------
  # 3. Augment IEA_PetrochemEI with Additional Electricity-Related Data
  #    - Add new rows for various electricity processes and adjust values
  #      where needed.
  # ---------------------------------------------------------------------------
  IEA_PetrochemEI <- IEA_PetrochemEI %>%
    bind_rows(
      IEA_PetrochemEI %>% 
        select(Region) %>% distinct() %>%
        mutate(
          Data1 = "Steam cracking, feedstock for NG",
          Value = 61.275 * (IEA_PetrochemEI %>% filter(Data1 == "Steam cracking, fuel & steam") %>% pull(Value)) / #Source: A. Boulamanti and J. A. Moya, Renew. Sustain. Energy Rev., 2017, 68, 1205–1212. Table 2
            (IEA_PetrochemEI %>% filter(Region == "USA", Data1 == "Steam cracking, fuel & steam") %>% pull(Value) %>% unique())
        )
    ) %>%
    bind_rows(
      IEA_PetrochemEI %>% 
        select(Region) %>% distinct() %>%
        mutate(
          Data1 = "Steam cracking, fuel & steam for NG",
          Value = 21 * # Assumption of fuel oil as heating to balance excess oil inputs in the energy balance sheet
            (IEA_PetrochemEI %>% filter(Data1 == "Steam cracking, fuel & steam") %>% pull(Value)) /
            (IEA_PetrochemEI %>% filter(Region == "USA", Data1 == "Steam cracking, fuel & steam") %>% pull(Value) %>% unique())
        )
    ) %>%
    bind_rows(
      IEA_PetrochemEI %>% select(Region) %>% distinct() %>%
        mutate(Data1 = "fertProd electricity", Value = 0.18 * 60 / 28 * sm_Mwh_2_GJ) # Source: Palys23 Section 2.3, Page 6
    ) %>%
    bind_rows(
      IEA_PetrochemEI %>% select(Region) %>% distinct() %>%
        mutate(Data1 = "mtoMta electricity", Value = 5) # Source: Bazzanella17 Section 4.5.3
    ) %>%
    bind_rows(
      IEA_PetrochemEI %>% select(Region) %>% distinct() %>%
        mutate(Data1 = "stCrLiq electricity", Value = 0.94) # Source: Spallina17 Table 5
    ) %>%
    bind_rows(
      IEA_PetrochemEI %>% select(Region) %>% distinct() %>%
        mutate(Data1 = "stCrNg electricity", Value = 0.16 * sm_Mwh_2_GJ) # Source: A. Boulamanti17 Table 2
    ) %>%
    bind_rows(
      IEA_PetrochemEI %>% select(Region) %>% distinct() %>%
        mutate(Data1 = "amSyCoal electricity", Value = 3.7) # Source: IEA, The Future of Hydrogen19 PAGE | 4
    ) %>%
    bind_rows(
      IEA_PetrochemEI %>% select(Region) %>% distinct() %>%
        mutate(Data1 = "amSyNG electricity", Value = 0.3) # Source: IEA, The Future of Hydrogen19 PAGE | 4
    ) %>%
    bind_rows(
      IEA_PetrochemEI %>% select(Region) %>% distinct() %>%
        mutate(Data1 = "amSyLiq electricity", Value = 2) # Source: IEA, The Future of Petrochemicals18 Table A4
    ) %>%
    bind_rows(
      IEA_PetrochemEI %>% select(Region) %>% distinct() %>% 
        mutate(Data1 = "meSySol electricity", Value = 3.7)  # Source: IEA, The Future of Hydrogen19 PAGE | 5
    ) %>%
    bind_rows(
      IEA_PetrochemEI %>% select(Region) %>% distinct() %>%
        mutate(Data1 = "meSyNg electricity", Value = 0.3)  # Source: IEA, The Future of Hydrogen19 PAGE | 5
    ) %>%
    bind_rows(
      IEA_PetrochemEI %>% select(Region) %>% distinct() %>%
        mutate(Data1 = "meSyLiq electricity", Value = 2) # Source: IEA, The Future of Petrochemicals18 Table A4
    ) %>%
    # Adjust "Methanol, fuel & steam" values
    mutate(
      Value = ifelse(
        Data1 == "Methanol, fuel & steam",
        Value * (33.9 / # Source: IEA, The Future of Hydrogen19 PAGE | 5
                   (IEA_PetrochemEI %>% filter(Region == "EUR", Data1 == "Methanol, fuel & steam") %>% pull(Value) %>% unique())), 
        Value
      )
    ) %>%
    # Adjust "Steam cracking, fuel & steam" values
    mutate(
      Value = ifelse(
        Data1 == "Steam cracking, fuel & steam",
        Value * (66 / # Source: Spallina17 Table 5
                   (IEA_PetrochemEI %>% filter(Region == "MEA", Data1 == "Steam cracking, fuel & steam") %>% pull(Value) %>% unique())),
        Value
      )
    ) %>%
    distinct(Region, Data1, .keep_all = TRUE)
  
  # ---------------------------------------------------------------------------
  # 4. Compute Energy Demand from Chemical Routes
  #    - Load a mapping file (structuremappingIEA_PetrochemEI.csv) that links source and target Data1.
  #    - Join with IEA_PetrochemEI to compute energy demand as: Energy_demand = Value.x * Value.y.
  #    - Adjust specific values for certain regions and processes.
  # ---------------------------------------------------------------------------
  mapping <- toolGetMapping("structuremappingIEA_PetrochemEI.csv", type = "sectoral", where = "mrindustry")
  
  result <- AllChemicalRoute2005_2020 %>%
    left_join(mapping, by = c("Data1" = "Target")) %>%
    left_join(IEA_PetrochemEI, by = c("Region", "Source" = "Data1")) %>%
    mutate(
      Value.y = ifelse(Data1 == "stCrNg" & Type == "feli_chemicals" & Region == "MEA", 61, Value.y), #Same as Global
      Value.y = ifelse(Data1 == "stCrLiq" & Type == "feli_chemicals" & Region == "MEA", 56.5, Value.y), #Same as Global
      Value.y = ifelse(Data1 == "amSyNG" & Type == "fega_chemicals" & Region == "CHA", 41.6, Value.y), # Should be lower if it is based on NG, Value same as Global
      Value.y = ifelse(Data1 == "meSyNg" & Type == "fega_chemicals" & Region == "CHA", 33.9, Value.y) # Should be lower if it is based on NG, Value same as Global
    ) %>%
    filter(!is.na(Source)) %>%
    mutate(Energy_demand = Value.x * Value.y)
  
  # ---------------------------------------------------------------------------
  # 5. Summarize Energy Demand by Region, Year, and Process Type
  # ---------------------------------------------------------------------------
  energy_summary <- result %>%
    group_by(Region, Year, Type) %>%
    summarise(Total_Energy_Demand = sum(Energy_demand, na.rm = TRUE), .groups = "drop")
  
  # ---------------------------------------------------------------------------
  # 6. Load Industry Demand Data for Chemicals
  # ---------------------------------------------------------------------------
  feIndustry <- calcOutput("FeDemandIndustry", signif = 4, warnNA = FALSE, aggregate = TRUE)[, 
                                                                                             c("y2005", "y2010", "y2015", "y2020"),
                                                                                             c("gdp_SSP2.feelhth_chemicals", "gdp_SSP2.feelwlth_chemicals", 
                                                                                               "gdp_SSP2.feh2_chemicals", "gdp_SSP2.fega_chemicals", 
                                                                                               "gdp_SSP2.feli_chemicals", "gdp_SSP2.feso_chemicals")
  ] %>%
    as.data.frame() %>%
    select(-Cell)
  
  # ---------------------------------------------------------------------------
  # 7. Calculate "OtherChem" Energy Demand
  #    - Rename "Type" to "Data2" in energy_summary.
  #    - Merge with feIndustry and calculate the difference between feIndustry Value and the total energy demand.
  #    - Rename the difference as Energy_demand, set Type accordingly, and mark Data1 as "OtherChem".
  # ---------------------------------------------------------------------------
  energy_summary <- energy_summary %>%
    rename(Data2 = Type)
  
  OtherChemCalc <- feIndustry %>%
    left_join(energy_summary, by = c("Region", "Year", "Data2")) %>%
    mutate(
      Total_Energy_Demand = ifelse(is.na(Total_Energy_Demand), 0, Total_Energy_Demand),
      Total_Difference = Value - Total_Energy_Demand
    ) %>%
    rename(Energy_demand = Total_Difference) %>%
    rename(Type = Data2) %>%
    mutate(Data1 = "OtherChem")
  
  # ---------------------------------------------------------------------------
  # 8. Merge Main Energy Result with "OtherChem" and Summarize
  # ---------------------------------------------------------------------------
  MainChem <- result %>% select(-Value.x, -Source, -Value.y)
  
  merged_result <- bind_rows(OtherChemCalc, MainChem) %>%
    mutate(
      mode = "standard",
      Type = case_when(
        Type %in% c("feelwlth_chemicals", "feelhth_chemicals") ~ "feels",
        Type %in% c("feh2_chemicals", "fega_chemicals") ~ "fegas",
        Type == "feli_chemicals" ~ "fehos",
        Type == "feso_chemicals" ~ "fesos",
        TRUE ~ Type
      )
    ) %>%
    group_by(Region, Year, Data1, Type, mode) %>%
    summarise(Energy_demand = sum(Energy_demand, na.rm = TRUE), .groups = "drop") %>%
    mutate(
      Data1 = case_when(
        Type %in% c("fehos", "fesos", "fegas", "feels") & Data1 == "OtherChem" ~ "chemOld",
        TRUE ~ Data1
      )
    ) %>%
    select(Region, Year, Data1, mode, Type, Energy_demand)
  
  # ---------------------------------------------------------------------------
  # 9. Aggregate Data to Country Level
  #    - Load ChemicalTotal data for weighting.
  #    - Retrieve regional mapping.
  #    - Convert merged result to a magpie object and aggregate to country level.
  # ---------------------------------------------------------------------------
  Chemcial_Total <- calcOutput("ChemicalTotal", aggregate = FALSE) %>%
    .[, c("y2005", "y2010", "y2015", "y2020"), ]
  
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  
  x <- as.magpie(merged_result, spatial = 1, temporal = 2)
  x <- toolAggregate(
    x,
    rel = map,
    dim = 1,
    from = "RegionCode",
    to = "CountryCode",
    weight = Chemcial_Total[unique(map$CountryCode), , ]
  )
  x[is.na(x)] <- 0
  
  # ---------------------------------------------------------------------------
  # 10. Return Final Aggregated Object with Metadata
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "EJ",
    description = "Aggregates chemical energy demand from 2005 to 2020 by calculating energy intensities for various processes, adjusting values for specific regions, merging with industry demand data, and aggregating the results to the country level."
  ))
}
