#' Convert MMSA_Methanol
#'
#' Convert MMSA (Methanol Market Services Asia) Global Methanol Outlook 2023 Growth and Decarbonization.xlsx data
#' to ISO country level.
#'
#' @return MagPIE object of the MMSA_Methanol data aggregated to country level.
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#'   a <- convertMMSA_Methanol(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset
convertMMSA_Methanol <- function(x) {
  # ---------------------------------------------------------------------------
  # Extract China-Specific Data
  #    - Isolate data corresponding to "China" from the MMSA_Methanol object.
  # ---------------------------------------------------------------------------
  x_China <- x["China", ]
  
  # Convert the country name in the China-specific data to its ISO code.
  getItems(x_China, dim = 1) <- toolCountry2isocode(getItems(x_China, dim = 1))
  
  # ---------------------------------------------------------------------------
  # Separate Production Capacity and Total Demand Data (Excluding China)
  #    - x_Dem: Data for Total Demand (excluding China-specific rows).
  #    - x_Cap: Data for Production Capacity (excluding China-specific rows).
  # ---------------------------------------------------------------------------
  x_Dem <- x["China", , "Production capacity", invert = TRUE]
  x_Cap <- x["China", , "Total Demand", invert = TRUE]
  
  # ---------------------------------------------------------------------------
  # Load Regional-to-Country Mapping
  #    - Load the mapping file and filter out regions labeled as "rest".
  # ---------------------------------------------------------------------------
  file_name <- "regionmappingMMSA.csv"
  map <- toolGetMapping(file_name, type = "regional", where = "mrindustry") %>%
    dplyr::filter(.data$IFAReg != "rest")
  
  # ---------------------------------------------------------------------------
  # Retrieve Weighting Data for Aggregation
  #    - For capacity data: Retrieve methanol capacity data for 2018.
  # ---------------------------------------------------------------------------
  Methanol_C <- calcOutput("IHS_Meth", subtype = "Capacity_2010-2020", aggregate = FALSE)[, "y2018", ]
  
  # ---------------------------------------------------------------------------
  # Aggregate Production Capacity Data to Country Level
  #    - Use the methanol capacity weighting data (Methanol_C) to aggregate x_Cap.
  # ---------------------------------------------------------------------------
  x_Cap <- toolAggregate(
    x_Cap,
    rel = map,
    dim = 1,
    from = "IFAReg",
    to = "CountryCode",
    weight = Methanol_C[unique(map$CountryCode), , drop = FALSE]
  )
  
  # ---------------------------------------------------------------------------
  # Retrieve Weighting Data for Demand Aggregation
  #    - Retrieve final energy (FE) data for 2018 to serve as weights for total demand aggregation.
  # ---------------------------------------------------------------------------
  fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)
  fe_2018 <- fe[, "y2018", "FE (EJ/yr)"]
  
  # ---------------------------------------------------------------------------
  # Aggregate Total Demand Data to Country Level
  #    - Use the FE data (fe_2018) as weights to aggregate x_Dem.
  # ---------------------------------------------------------------------------
  x_Dem <- toolAggregate(
    x_Dem,
    rel = map,
    dim = 1,
    from = "IFAReg",
    to = "CountryCode",
    weight = fe_2018[unique(map$CountryCode), , drop = FALSE]
  )
  
  # ---------------------------------------------------------------------------
  # Combine Aggregated Data and China-specific Data
  #    - Merge the aggregated capacity and demand data.
  #    - Then, add back the China-specific data.
  # ---------------------------------------------------------------------------
  x <- mbind(x_Cap, x_Dem)
  x <- mbind(x, x_China)
  
  # ---------------------------------------------------------------------------
  # Finalize: Fill Missing Countries
  #    - Fill any missing country entries with 0 to ensure completeness.
  # ---------------------------------------------------------------------------
  x <- toolCountryFill(x, fill = 0)
  
  # ---------------------------------------------------------------------------
  # Return the Final MagPIE Object
  # ---------------------------------------------------------------------------
  return(x)
}
