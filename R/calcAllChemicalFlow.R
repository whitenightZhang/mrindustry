#' Aggregates chemical flow data in 2020 from calcAllChemicalRoute into broader categories
#' based on their outputs (ammonia, methanol, HVC, fertilizer and final ammonia/methanol).
#' 
#' @author Qianzhi Zhang
#'
#' @export
calcAllChemicalFlow <- function() {
  
  # ---------------------------------------------------------------------------
  # Load All Chemical Route Data
  #    - Retrieve the aggregated AllChemicalRoute data.
  #    - Remove the unwanted columns 'Cell' and 'Data2'.
  # ---------------------------------------------------------------------------
  AllChemicalRoute <- calcOutput("AllChemicalRoute", aggregate = TRUE) %>%
    as.data.frame() %>%
    select(-Cell, -Data2)
  
  # ---------------------------------------------------------------------------
  # Re-categorize and Summarize Data
  #    - Reclassify Data1 into broader categories:
  #       * 'ammonia' for ammonia-related routes,
  #       * 'methanol' for methanol-related routes,
  #       * 'hvc' for hvc routes,
  #       * 'ammoFinal' for the final ammonia output,
  #       * 'methFinal' for the final methanol output,
  #       * 'fertilizer' for fertilizer production.
  #    - Group the data by Region, Year, and the new Data1 category,
  #      then sum the Value for each group.
  # ---------------------------------------------------------------------------
  AllChemicalRoute_summarized <- AllChemicalRoute %>%
    mutate(Data1 = case_when(
      Data1 %in% c("amSyCoal", "amSyNG", "amSyLiq", "amSyCoal_cc", "amSyNG_cc", "amSyH2") ~ "ammonia",
      Data1 %in% c("meSySol", "meSyNg", "meSyLiq", "meSyH2", "meSySol_cc", "meSyNg_cc") ~ "methanol",
      Data1 %in% c("mtoMta", "stCrNg", "stCrLiq") ~ "hvc",
      Data1 == "amToFinal" ~ "ammoFinal",
      Data1 == "meToFinal" ~ "methFinal",
      Data1 == "fertProd" ~ "fertilizer",
      TRUE ~ Data1
    )) %>%
    group_by(Region, Year, Data1) %>%
    summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop")
  
  # ---------------------------------------------------------------------------
  # Load Total Chemical Production Data for Weighting
  #    - Retrieve the ChemicalTotal data for 2020 to be used as weights.
  # ---------------------------------------------------------------------------
  Chemical_Total <- calcOutput("ChemicalTotal", aggregate = FALSE) %>%
    .[, "y2020", ]
  
  # ---------------------------------------------------------------------------
  # Convert Data to Magpie Object and Aggregate to Country Level
  #    - Retrieve regional mapping information.
  #    - Convert the summarized data to a magpie object.
  #    - Aggregate the regional data to the country level using the mapping and weights.
  # ---------------------------------------------------------------------------
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  x <- as.magpie(AllChemicalRoute_summarized, spatial = 1, temporal = 2)
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode", 
                     weight = Chemical_Total[unique(map$CountryCode), , ])
  x[is.na(x)] <- 0  # Replace any missing values with 0
  
  # ---------------------------------------------------------------------------
  # Return the Final Object with Metadata
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "Gt Chemical Flows & GtN (fertilizer)",
    description = "Aggregated chemical flow data recategorized into key sectors (ammonia, methanol, hvc, final outputs, and fertilizer) and aggregated to country level for 2020."
  ))
}

