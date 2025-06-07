#'
#' @author Qianzhi Zhang
#'
#' @export
calcAllChemicalRoute2005_2020 <- function() {
  
  # ---------------------------------------------------------------------------
  # 1. Load and Prepare Chemical Flow Data (2005-2020)
  #    - Retrieve AllChemicalFlow2005_2020 data, remove unnecessary columns,
  #      and rename "Data1" to "Product" for consistency.
  # ---------------------------------------------------------------------------
  AllChemicalFlow2005_2020 <- calcOutput("AllChemicalFlow2005_2020", warnNA = FALSE, aggregate = TRUE) %>% 
    as.data.frame() %>%
    select(-Cell) %>%
    rename(Product = Data1)
  
  # ---------------------------------------------------------------------------
  # 2. Load and Recategorize Chemical Route Data for 2020
  #    - Retrieve AllChemicalRoute data for the year 2020.
  #    - Remove extra columns and recategorize "Data1" into broader product groups,
  #      creating a new "Product" column.
  # ---------------------------------------------------------------------------
  AllChemicalRoute <- calcOutput("AllChemicalRoute", aggregate = TRUE)[, "y2020", ] %>% 
    as.data.frame() %>%
    select(-Cell, -Data2, -Year) %>%
    mutate(
      Product = case_when(
        Data1 %in% c("amSyCoal", "amSyNG", "amSyLiq", "amSyCoal_cc", "amSyNG_cc", "amSyH2") ~ "ammonia",
        Data1 %in% c("meSySol", "meSyNg", "meSyLiq", "meSyH2", "meSySol_cc", "meSyNg_cc") ~ "methanol",
        Data1 %in% c("mtoMta", "stCrNg", "stCrLiq") ~ "hvc",
        Data1 == "amToFinal" ~ "ammoFinal",
        Data1 == "meToFinal" ~ "methFinal",
        Data1 == "fertProd" ~ "fertilizer",
        TRUE ~ Data1  # Retain other values as is
      )
    )
  
  # ---------------------------------------------------------------------------
  # 3. Calculate Share Within Each Product Group
  #    - Group the recategorized route data by Region, Product, and original Data1.
  #    - Sum the values and calculate the share of each route within its Product.
  # ---------------------------------------------------------------------------
  AllChemicalRoute <- AllChemicalRoute %>%
    group_by(Region, Product, Data1) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    group_by(Region, Product) %>%
    mutate(
      Share = Value / sum(Value)
    )
  
  # ---------------------------------------------------------------------------
  # 4. Join Flow Data with Route Shares and Compute Routes_Flow
  #    - Join the 2005-2020 flow data with the route shares by Region and Product.
  #    - Compute Routes_Flow as the product of the original flow (Value.x) and the share.
  #    - Set processing flag (opmoPrc) based on route type.
  #    - Compute the amount to be reduced for both USA (2005) and CHA (2010) to avoid negative value for OtherChem.
  # ---------------------------------------------------------------------------
  AllChemicalRoutes2005_2020 <- AllChemicalFlow2005_2020 %>%
    left_join(AllChemicalRoute, by = c("Region", "Product")) %>%
    mutate(
      Routes_Flow = Value.x * Share,
      opmoPrc = "standard"
    ) %>%
    select(Region, Year, Data1, opmoPrc, Routes_Flow) %>%
    filter(!is.na(Data1))
  
  AllChemicalRoutes2005_2020 <- AllChemicalRoutes2005_2020 %>%
    mutate(
      # Compute the amount to be reduced for both USA (2005) and CHA (2010)
      adjustment = case_when(
        Region == "USA" & Year == 2005 & Data1 == "stCrNg" ~ Routes_Flow * 0.20,
        Region == "CHA" & Year == 2010 & Data1 == "stCrNg" ~ Routes_Flow * 0.20,
        TRUE ~ 0  # Default case: no adjustment
      ),
      
      # Reduce Routes_Flow for stCrNg in specified conditions
      Routes_Flow = case_when(
        Region == "USA" & Year == 2005 & Data1 == "stCrNg" ~ Routes_Flow * 0.80,
        Region == "CHA" & Year == 2010 & Data1 == "stCrNg" ~ Routes_Flow * 0.80,
        TRUE ~ Routes_Flow  # Keep other values unchanged
      )
    ) %>%
    
    # Adjust stCrLiq within the same Region and Year
    group_by(Region, Year) %>%  # Group to ensure sum(adjustment) applies correctly
    mutate(
      Routes_Flow = dplyr::if_else(Data1 == "stCrLiq", Routes_Flow + sum(adjustment), Routes_Flow)
    ) %>%
    ungroup() %>%  # Remove grouping
    select(-adjustment)  # Remove temporary column
  
  # ---------------------------------------------------------------------------
  # 5. Process "OtherChem" Data
  #    - Retrieve AllChemicalUe data for the "OtherChem" product.
  #    - Retrieve industry demand data (feIndustry) for chemicals.
  #    - Join these datasets to compute the Routes_Flow for "OtherChem".
  # ---------------------------------------------------------------------------
  AllChemicalUe <- calcOutput("AllChemicalUe", aggregate = TRUE)[, , "OtherChem"] %>% 
    as.data.frame() %>%
    select(-Cell, -Year)
  
  feIndustry <- calcOutput("FeDemandIndustry", signif = 4, warnNA = FALSE, aggregate = TRUE)[, c("y2005", "y2010", "y2015", "y2020"), "gdp_SSP2.ue_chemicals"] %>%
    as.data.frame() %>%
    select(-Cell)
  
  OtherChem <- left_join(feIndustry, AllChemicalUe, by = c("Region")) %>%
    mutate(
      Data1 = "chemOld",
      Routes_Flow = Value.x * Value.y,
      opmoPrc = "standard"
    ) %>%
    select(Region, Year, Data1, opmoPrc, Routes_Flow)
  
  # ---------------------------------------------------------------------------
  # 6. Combine Route Data with "OtherChem"
  # ---------------------------------------------------------------------------
  AllChemicalRoutes2005_2020 <- bind_rows(AllChemicalRoutes2005_2020, OtherChem)
  
  # ---------------------------------------------------------------------------
  # 7. Aggregate Data to Country Level
  #    - Retrieve ChemicalTotal data for weighting.
  #    - Get regional mapping and convert the combined data to a magpie object.
  #    - Aggregate from regions to country level using the mapping and weights.
  # ---------------------------------------------------------------------------
  Chemcial_Total <- calcOutput("ChemicalTotal", aggregate = FALSE) %>%
    .[, c("y2005", "y2010", "y2015", "y2020"), ]
  
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  
  x <- as.magpie(AllChemicalRoutes2005_2020, spatial = 1, temporal = 2)
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode", 
                     weight = Chemcial_Total[unique(map$CountryCode), , ])
  x[is.na(x)] <- 0
  
  # ---------------------------------------------------------------------------
  # 8. Return the Final Aggregated Object with Metadata
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "Gt Chemical Routes",
    description = "Aggregates chemical route flows from 2005 to 2020 by recategorizing routes into key product groups, computing internal shares, merging with 'OtherChem' unit energy data, and aggregating the results to the country level."
  ))
}

