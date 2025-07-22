#'
#'Calculates the total fertilizer production (MtN, as the sum of urea, AN, CAN and AS) per country 
#'and the fraction of ammonia production (MtN/MtN) used for fertilizer production based on IFA data.
#'
#' @author Qianzhi Zhang
#'
#' @export
calcFertilizerRoute <- function() {
  
  # ---------------------------------------------------------------------------
  # Load and Preprocess fertilizer Production Data
  #    - Retrieve production data for Urea, AN, CAN, and AS from IFA datasets.
  #    - Convert the production values from ktN to MtN by dividing by 1000.
  # ---------------------------------------------------------------------------
  
  # Urea production data from IFA_ChemN
  Urea_IFA <- calcOutput("IFA_Chem", subtype = "urea_statistics_production", unitNitrogen=TRUE, aggregate = TRUE) %>%
    .[, c("y2015", "y2016", "y2017", "y2018", "y2019", "y2020"), ] %>%
    as.data.frame() %>%
    mutate(Value = Value / 1000)  # Scale down values
  
  # Ammonium Nitrate (AN) production data from IFA_ChemNAppend
  AN_IFA <- calcOutput("IFA_ChemAppend", subtype = "AN_statistics_production", unitNitrogen=TRUE, aggregate = TRUE) %>%
    .[, c("y2015", "y2016", "y2017", "y2018", "y2019", "y2020"), ] %>%
    as.data.frame() %>%
    mutate(Value = Value / 1000)
  
  # Calcium Ammonium Nitrate (CAN) production data from IFA_ChemNAppend
  CAN_IFA <- calcOutput("IFA_ChemAppend", subtype = "CAN_statistics_production", unitNitrogen=TRUE, aggregate = TRUE) %>%
    .[, c("y2015", "y2016", "y2017", "y2018", "y2019", "y2020"), ] %>%
    as.data.frame() %>%
    mutate(Value = Value / 1000)
  
  # Ammonium Sulfate (AS) production data from IFA_ChemNAppend
  AS_IFA <- calcOutput("IFA_ChemAppend", subtype = "AS_statistics_production", unitNitrogen=TRUE, aggregate = TRUE) %>%
    .[, c("y2015", "y2016", "y2017", "y2018", "y2019", "y2020"), ] %>%
    as.data.frame() %>%
    mutate(Value = Value / 1000)
  
  
  # ---------------------------------------------------------------------------
  # Load and Preprocess Ammonia Production Data
  #    - Retrieve ammonia production data from IFA_ChemN.
  #    - Convert the production value and change the 'Year' column to numeric.
  # ---------------------------------------------------------------------------
  
  Ammonia_IFA <- calcOutput("IFA_Chem", subtype = "ammonia_statistics_production", unitNitrogen=TRUE, aggregate = TRUE) %>%
    .[, c("y2015", "y2016", "y2017", "y2018", "y2019", "y2020"), ] %>%
    as.data.frame() %>%
    mutate(
      NAmmonia_production = Value / 1000,             # Scale down ammonia production values
      Year = as.numeric(gsub("y", "", Year))           # Convert Year string (e.g., "y2015") to numeric
    ) %>%
    select(-Value, -Cell, -Data1)  # Remove unnecessary columns
  
  
  # ---------------------------------------------------------------------------
  # Combine fertilizer Production Data
  #    - Bind Urea, AN, CAN, and AS datasets.
  #    - Convert the 'Year' column to numeric, remove extra columns,
  #      and calculate the total fertilizer production per Region and Year.
  # ---------------------------------------------------------------------------
  
  NFert_total <- bind_rows(
    Urea_IFA %>% mutate(source = "Urea_IFA"),
    AN_IFA %>% mutate(source = "AN_IFA"),
    CAN_IFA %>% mutate(source = "CAN_IFA"),
    AS_IFA %>% mutate(source = "AS_IFA")
  ) %>%
    mutate(Year = as.numeric(gsub("y", "", Year))) %>%  # Convert Year to numeric
    select(-Cell, -Data1) %>%
    group_by(Year, Region) %>%
    summarise(
      NFert_production = sum(Value, na.rm = TRUE),  # Sum production values per region and year
      .groups = "drop"
    ) %>%
    mutate(source = "Total_NFert")
  
  
  # ---------------------------------------------------------------------------
  # Calculate fertilizer Conversion Ratio
  #    - Join ammonia production data with total fertilizer production.
  #    - Compute the fertilizer ratio as NFert_production / NAmmonia_production, capped at 1.
  # ---------------------------------------------------------------------------
  
  NFert_output <- Ammonia_IFA %>%
    left_join(NFert_total, by = c("Region", "Year")) %>%
    mutate(
      NFert_ratio = pmin(NFert_production / NAmmonia_production, 1)  # Cap the ratio at 1
    ) %>%
    select(-NAmmonia_production, -source)
  
  
  # ---------------------------------------------------------------------------
  # Load Weighting Data
  #    - Retrieve non-aggregated Urea production data for the year 2020.
  # ---------------------------------------------------------------------------
  
  Urea_IFA_all <- calcOutput("IFA_Chem", subtype = "urea_statistics_production", unitNitrogen=TRUE, aggregate = FALSE) %>%
    .[, c("y2020"), ]
  
  
  # ---------------------------------------------------------------------------
  # Aggregate Data to Country Level
  #    - Retrieve regional mapping.
  #    - Convert NFert_output to a magpie object and aggregate regional data to country level
  #      using the production data as weights.
  # ---------------------------------------------------------------------------
  
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  x <- as.magpie(NFert_output, spatial = 1, temporal = 2)
  x <- toolAggregate(
    x,
    rel = map,
    dim = 1,
    from = "RegionCode",
    to = "CountryCode",
    weight = Urea_IFA_all[unique(map$CountryCode), , ]
  )
  x[is.na(x)] <- 0  # Replace NA values with 0
  
  
  # ---------------------------------------------------------------------------
  # Return Final Aggregated Object and Metadata
  # ---------------------------------------------------------------------------
  
  return(list(
    x = x,
    weight = NULL,
    unit = "MtN Fert",
    description = "fertilizer conversion ratio derived from IFA chemical data, representing the fraction of ammonia production converted to fertilizer production, aggregated to country level."
  ))
}
