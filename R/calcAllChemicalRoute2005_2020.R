#' Calculates chemical route flows (including OtherChem) from 2005 to 2020 
#' from total chemical flows based on 2020 route shares.
#' Flows are aggregated to the country level.
#' 
#' @author Qianzhi Zhang
#'
#' @export
#' @param CCS boolean parameter whether CCS technologies are considered as such in 2020 or assumed to be technologies without CCS
#' 
calcAllChemicalRoute2005_2020 <- function(CCS=FALSE) {
  
  # ---------------------------------------------------------------------------
  # Load and Prepare Chemical Flow Data (2005-2020)
  #    - Retrieve AllChemicalFlow2005_2020 data, remove unnecessary columns,
  #      and rename "Data1" to "Product" for consistency.
  # ---------------------------------------------------------------------------
  AllChemicalFlow2005_2020 <- calcOutput("AllChemicalFlow2005_2020", warnNA = FALSE, aggregate = TRUE) %>% 
    as.data.frame() %>%
    select(-Cell) %>%
    rename(Product = Data1)
  
  # ---------------------------------------------------------------------------
  # Load and Recategorize Chemical Route Data for 2020
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
  # Calculate Share Within Each Product Group
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
  # Join Flow Data with Route Shares and Compute Routes_Flow
  #    - Join the 2005-2020 flow data with the route shares by Region and Product.
  #    - Compute Routes_Flow as the product of the original flow (Value.x) and the share.
  #    - Set processing flag (opmoPrc) based on route type.
  # ---------------------------------------------------------------------------
  AllChemicalRoutes2005_2020 <- AllChemicalFlow2005_2020 %>%
    left_join(AllChemicalRoute, by = c("Region", "Product")) %>%
    mutate(
      Routes_Flow = Value.x * Share,
      opmoPrc = "standard"
    ) %>%
    select(Region, Year, Data1, opmoPrc, Routes_Flow) %>%
    filter(!is.na(Data1))
  
  # ---------------------------------------------------------------------------
  # Retrieve "OtherChem" Data
  # ---------------------------------------------------------------------------
  
  OtherChem <- AllChemicalFlow2005_2020 %>% filter(Product=="OtherChem") %>%
    rename(Data1=Product, Routes_Flow=Value)%>%
    mutate(opmoPrc = "standard", Data1="chemOld")
  
  # ---------------------------------------------------------------------------
  # Combine Route Data with "OtherChem"
  # ---------------------------------------------------------------------------
  AllChemicalRoutes2005_2020 <- bind_rows(AllChemicalRoutes2005_2020, OtherChem)
  
  # ---------------------------------------------------------------------------
  # If no CCS technologies are considered, assign the CCS technology to the respective non-CCS technology
  # ---------------------------------------------------------------------------
  if(CCS==FALSE){
    AllChemicalRoutes2005_2020 <- AllChemicalRoutes2005_2020 %>%
      group_by(Region, Year) %>%
      mutate(
        Routes_Flow = ifelse(Data1 == "amSyNG",
                       Routes_Flow + sum(Routes_Flow[Data1 == "amSyNG_cc"], na.rm = TRUE), 
                       Routes_Flow),
        Routes_Flow = ifelse(Data1 == "amSyNG_cc", 0, Routes_Flow)
      ) %>%
      ungroup() 
  }
  
  # ---------------------------------------------------------------------------
  # Aggregate Data to Country Level
  #    - Retrieve ChemicalTotal data for weighting.
  #    - Get regional mapping and convert the combined data to a magpie object.
  #    - Aggregate from regions to country level using the mapping and weights.
  # ---------------------------------------------------------------------------
  Chemical_Total <- calcOutput("ChemicalTotal", aggregate = FALSE) %>%
    .[, c("y2005", "y2010", "y2015", "y2020"), ]
  
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  
  x <- as.magpie(AllChemicalRoutes2005_2020, spatial = 1, temporal = 2)
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode", 
                     weight = Chemical_Total[unique(map$CountryCode), , ])
  x[is.na(x)] <- 0
  
  # ---------------------------------------------------------------------------
  # Return the Final Aggregated Object with Metadata
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "Gt Chemical Routes",
    description = "Chemical route flows from 2005 to 2020 including 'OtherChem'."
  ))
}

