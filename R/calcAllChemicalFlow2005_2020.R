#'
#' @author Qianzhi Zhang
#'
#' @export
calcAllChemicalFlow2005_2020 <- function() {
  
  # ---------------------------------------------------------------------------
  # 1. Define Conversion Factors (p37_mat2ue)
  #    - Conversion factors (mat2ue) for each product in 2005$/kg (or 2005$/kgN)
  # ---------------------------------------------------------------------------
  
  p37_mat2ue <- data.frame(
    Product = c("hvc", "fertilizer", "methFinal", "ammoFinal","OtherChem"),
    mat2ue = c(0.66, 0.73, 0.37, 0.69, 1),  # Conversion factors
    Unit = c("2017$/kg", "2017$/kgN", "2017$/kg", "2017$/kg","2017$/2017$")
  )
  
  # ---------------------------------------------------------------------------
  # 2. Retrieve and Combine AllChemicalUe and Industry Demand Data
  #    - Get AllChemicalUe data (rounded to 8 digits) and remove extra columns.
  #    - Get industry demand (feIndustry) for selected years.
  #    - Join these datasets and calculate Material_Flow as:
  #         Material_Flow = (Value.x from AllChemicalUe * Value.y from feIndustry) / mat2ue
  # ---------------------------------------------------------------------------
  AllChemicalUe <- calcOutput("AllChemicalUe", round = 8, aggregate = TRUE) %>% 
    as.data.frame() %>%
    select(-Cell, -Year)
  
  feIndustry <- calcOutput("FeDemandIndustry", signif = 4, warnNA = FALSE, aggregate = TRUE)[, c("y2005", "y2010", "y2015", "y2020"), "gdp_SSP2.ue_chemicals"] %>%
    as.data.frame() %>%
    select(-Cell)
  
  AllChemicalFlow <- AllChemicalUe %>%
    left_join(feIndustry, by = c("Region" = "Region")) %>%
    left_join(p37_mat2ue, by = c("Data1.x" = "Product")) %>%
    mutate(Material_Flow = Value.x * Value.y / mat2ue)
  
  # ---------------------------------------------------------------------------
  # 3. Adjust Ammonia Flow Based on fertilizer Conversion Ratio
  #    - Retrieve the NFert_ratio from FertilizerRoute (for year 2020).
  #    - Join the NFert_ratio to AllChemicalFlow (by Region).
  #    - For rows where Data1.x is "ammoFinal", adjust Material_Flow by dividing by (1 - NFert_ratio)
  #      and rename Data1.x to "ammonia".
  #    - Bind these adjusted rows back with the original dataset and remove temporary NFert_ratio.
  # ---------------------------------------------------------------------------
  nfert_ratio <- calcOutput("FertilizerRoute", aggregate = TRUE)[, "y2020", ] %>% 
    as.data.frame() %>%
    select(-Cell, -Year) %>%
    filter(Data1 == "NFert_ratio")
  
  AllChemicalFlow <- AllChemicalFlow %>%
    left_join(
      nfert_ratio %>% 
        filter(Data1 == "NFert_ratio") %>%
        select(Region, NFert_ratio = Value),
      by = "Region"
    ) %>%
    # Adjust rows for ammoFinal (ammonia)
    filter(Data1.x == "ammoFinal") %>%
    mutate(
      Data1.x = "ammonia",
      Material_Flow = Material_Flow / (1 - NFert_ratio)
    ) %>%
    bind_rows(AllChemicalFlow) %>%   # Bind back the adjusted rows
    select(-NFert_ratio)
  
  # ---------------------------------------------------------------------------
  # 4. Calculate MeFinalratio for Methanol Adjustment
  #    - Retrieve AllChemicalRoute data for 2020 and remove extra columns.
  #    - From AllChemicalRoute, filter rows for "meToFinal" and "mtoMta".
  #    - Pivot these values wider to have separate columns for meToFinal and mtoMta.
  #    - Calculate MeFinalratio as:
  #         if meToFinal is 0 then 1, else meToFinal / (mtoMta * 2.624 + meToFinal)
  # ---------------------------------------------------------------------------
  AllChemicalRoute <- calcOutput("AllChemicalRoute", aggregate = TRUE)[, "y2020", ] %>% 
    as.data.frame() %>%
    select(-Cell, -Data2, -Year)
  
  MeFinalratio <- AllChemicalRoute %>%
    filter(Data1 %in% c("meToFinal", "mtoMta")) %>%
    pivot_wider(names_from = Data1, values_from = Value) %>%
    mutate(
      MeFinalratio = ifelse(meToFinal == 0, 1, meToFinal / (mtoMta * 2.624 + meToFinal))
    ) %>%
    select(Region, MeFinalratio)
  
  # ---------------------------------------------------------------------------
  # 5. Adjust Methanol Flow Using MeFinalratio
  #    - Join the MeFinalratio with AllChemicalFlow (by Region).
  #    - For rows where Data1.x is "methFinal", adjust Material_Flow by dividing by MeFinalratio
  #      and rename Data1.x to "methanol".
  #    - Bind the new rows with AllChemicalFlow and remove the temporary MeFinalratio column.
  # ---------------------------------------------------------------------------
  AllChemicalFlow <- AllChemicalFlow %>%
    left_join(MeFinalratio, by = "Region")
  
  AllChemicalFlow<- AllChemicalFlow %>%
    bind_rows(
      AllChemicalFlow %>%
        filter(Data1.x == "methFinal") %>%
        mutate(
          Data1.x = "methanol",
          Material_Flow = Material_Flow / MeFinalratio
        )
    ) %>%
    select(-MeFinalratio)  # Remove temporary MeFinalratio column
  
  
  # ---------------------------------------------------------------------------
  # 6. Finalize Output and Aggregate Data to Country Level
  #    - Select the required columns (Region, Year, Data1.x, Material_Flow).
  #    - Retrieve ChemicalTotal data for weighting.
  #    - Get the regional mapping and convert FinalOutput to a magpie object.
  #    - Aggregate the regional data to country level using the mapping and ChemicalTotal weights.
  # ---------------------------------------------------------------------------
  FinalOutput <- AllChemicalFlow %>%
    select(Region, Year, Data1.x, Material_Flow)
  
  Chemcial_Total <- calcOutput("ChemicalTotal", aggregate = FALSE) %>%
    .[, c("y2005", "y2010", "y2015", "y2020"), ]
  
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  
  x <- as.magpie(FinalOutput, spatial = 1, temporal = 2)
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode", 
                     weight = Chemcial_Total[unique(map$CountryCode), , ])
  x[is.na(x)] <- 0
  
  # ---------------------------------------------------------------------------
  # 7. Return the Final Output
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "Gt Chemical Flows",
    description = "Calculates chemical material flow from 2005 to 2020 by applying conversion factors and adjusting ammonia and methanol flows using fertilizer and MeFinal ratios, then aggregating the flows to the country level."
  ))
}

