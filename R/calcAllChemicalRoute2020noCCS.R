#'
#' @author Qianzhi Zhang
#'
#' @export
calcAllChemicalRoute2020noCCS <- function() {
  
  # ---------------------------------------------------------------------------
  # 1. Load and Adjust Chemical Route Data for 2020 (Excluding CCS)
  #    - Retrieve AllChemicalRoute2005_2020 data for the year 2020.
  #    - Remove the "Cell" column.
  #    - For each Region and Year:
  #         * For rows with Data1 "amSyNG", add the sum of values from "amSyNG_cc".
  #         * Set values for "amSyNG_cc" to 0.
  # ---------------------------------------------------------------------------
  AllChemicalRoute2020 <- calcOutput("AllChemicalRoute2005_2020", warnNA = FALSE, aggregate = TRUE)[, "y2020", ] %>% 
    as.data.frame() %>%
    select(-Cell) %>%
    group_by(Region, Year) %>%
    mutate(
      Value = ifelse(Data1 == "amSyNG",
                     Value + sum(Value[Data1 == "amSyNG_cc"], na.rm = TRUE),
                     Value),
      Value = ifelse(Data1 == "amSyNG_cc", 0, Value)
    ) %>%
    ungroup()
  
  # ---------------------------------------------------------------------------
  # 2. Retrieve Weighting Data for 2020
  #    - Load ChemicalTotal data for 2020 to be used as weights during aggregation.
  # ---------------------------------------------------------------------------
  Chemcial_Total <- calcOutput("ChemicalTotal", aggregate = FALSE) %>%
    .[, c("y2020"), ]
  
  # ---------------------------------------------------------------------------
  # 3. Retrieve Regional Mapping
  #    - Get the mapping data to aggregate from regions to countries.
  # ---------------------------------------------------------------------------
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  
  # ---------------------------------------------------------------------------
  # 4. Convert Data to a Magpie Object and Aggregate to Country Level
  #    - Convert the adjusted data into a magpie object.
  #    - Aggregate regional data to country level using the mapping and weighting.
  # ---------------------------------------------------------------------------
  x <- as.magpie(AllChemicalRoute2020, spatial = 1, temporal = 2)
  x <- toolAggregate(
    x,
    rel = map,
    dim = 1,
    from = "RegionCode",
    to = "CountryCode",
    weight = Chemcial_Total[unique(map$CountryCode), , ]
  )
  x[is.na(x)] <- 0  # Replace missing values with 0
  
  # ---------------------------------------------------------------------------
  # 5. Return the Final Aggregated Object with Metadata
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "Gt Chemical Routes",
    description = "Aggregated chemical route data for 2020 excluding CCS: 'amSyNG_cc' values are added into 'amSyNG' and then set to zero, with results aggregated to the country level."
  ))
}

