#'
#' @author Qianzhi Zhang
#'
#' @export
calcAllChemicalUe <- function() {
  
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
  AllChemicalFlow <- calcOutput("AllChemicalFlow", aggregate = TRUE)[, "y2020", ] %>%
    as.data.frame() %>%
    select(-Cell) %>%
    filter(!Data1 %in% c("ammonia", "methanol")) %>%
    left_join(p37_mat2ue, by = c("Data1" = "Product")) %>%
    mutate(ue_material = Value * mat2ue)
  
  
  # ---------------------------------------------------------------------------
  # 3. Retrieve Industrial Demand Data for Chemicals
  #    - Get the feIndustry data from the FeDemandIndustry output for 2020.
  # ---------------------------------------------------------------------------
  feIndustry <- calcOutput("FeDemandIndustry", warnNA = FALSE, aggregate = TRUE)[, "y2020", "gdp_SSP2.ue_chemicals"] %>%
    as.data.frame() %>%
    select(-Cell)
  
  
  # ---------------------------------------------------------------------------
  # 4. Calculate UE Share for Each Chemical Product
  #    - Join the chemical flow data with industry demand data (by Region).
  #    - Compute the share (ue_share) by dividing the ue_material by the corresponding industry demand.
  # ---------------------------------------------------------------------------
  AllChemicalUE <- AllChemicalFlow %>%
    left_join(feIndustry, by = "Region") %>%
    mutate(ue_share = ue_material / Value.y) %>%   # 'Value.y' from feIndustry
    select(Region, Year.x, Data1.x, ue_share)
  
  
  # ---------------------------------------------------------------------------
  # 5. Account for Residual ("OtherChem") Share
  #    - For each region and year, sum the ue_share of existing products.
  #    - Create a new row for "OtherChem" representing the remaining share (1 minus the sum).
  # ---------------------------------------------------------------------------
  ue_summary <- AllChemicalUE %>%
    group_by(Region, Year.x) %>%
    summarise(
      ue_sum = sum(ue_share, na.rm = TRUE),
      .groups = "drop"
    )
  
  AllChemicalUE <- AllChemicalUE %>%
    bind_rows(
      ue_summary %>%
        mutate(
          Data1.x = "OtherChem",       # New product name for residual share
          ue_share = 1 - ue_sum         # Remaining share
        )
    ) %>%
    mutate(all_in = "ue_chemicals") %>%
    select(Region, Year.x, Data1.x, all_in, ue_share)
  
  
  # ---------------------------------------------------------------------------
  # 6. Convert to Magpie Object and Aggregate to Country Level
  #    - Retrieve regional mapping.
  #    - Convert the UE data to a magpie object and aggregate from regions to countries.
  # ---------------------------------------------------------------------------
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  x <- as.magpie(AllChemicalUE, spatial = 1, temporal = 2)
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode")
  
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
    unit = "2017$/kg or 2017$/kgN",  # Specify units based on conversion factors
    description = "Calculates the unit energy (UE) share of chemicals by applying material-to-UE conversion factors to chemical flows and comparing them to industry demand. The result, including a residual 'OtherChem' share, is aggregated to the country level for 2020."
  ))
}

