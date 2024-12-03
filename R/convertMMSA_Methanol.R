#' Convert MMSA_Methanol
#'
#' Convert MMSA(Methanol Market Services Asia) Global Methanol Outlook 2023 Growth and Decarbonization.xlsx data to ISO country level.
#'
#'
#' @return magpie object of the MMSA_Methanol data
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#' a <- convertMMSA_Methanol(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset

convertMMSA_Methanol <- function(x) {
  
  # Extract data specific to China
  x_China <- x["China", ]
  
  # Convert country names in x_China to ISO codes
  getItems(x_China, dim = 1) <- toolCountry2isocode(getItems(x_China, dim = 1))
  
  # Separate production capacity and total demand data (excluding China)
  x_Cap <- x["China", , "Production capacity", invert = TRUE]
  x_Dem <- x["China", , "Total Demand", invert = TRUE]
  
  # Load the regional-to-country mapping file
  file_name <- "regionmappingMMSA.csv"
  map <- toolGetMapping(file_name, type = "regional", where = "mrindustry") %>%
    dplyr::filter(.data$IFAReg != "rest")  # Exclude regions labeled as "rest"
  
  # Retrieve total chemical production data for 2018
  ChemTotal <- calcOutput("ChemicalTotal", aggregate = FALSE)
  ChemTotal_2018 <- ChemTotal[, "y2018", "TOTAL.CHEMICAL.NECHEM"]
  
  # Aggregate production capacity data to the country level using ChemTotal as weight
  x_Cap <- toolAggregate(
    x_Cap,
    rel = map,
    dim = 1,
    from = "IFAReg",
    to = "CountryCode",
    weight = ChemTotal_2018[unique(map$CountryCode), , drop = FALSE]
  )
  
  # Retrieve final energy (FE) data for 2018
  fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)
  fe_2018 <- fe[, "y2018", "FE (EJ/yr)"]
  
  # Aggregate total demand data to the country level using FE as weight
  x_Dem <- toolAggregate(
    x_Dem,
    rel = map,
    dim = 1,
    from = "IFAReg",
    to = "CountryCode",
    weight = fe_2018[unique(map$CountryCode), , drop = FALSE]
  )
  
  # Combine aggregated production capacity and total demand data
  x <- mbind(x_Cap, x_Dem)
  
  # Add back the China-specific data
  x <- mbind(x, x_China)
  
  # Fill missing countries with 0 for consistency
  x <- toolCountryFill(x, fill = 0)
  
  # Return the processed magpie object
  return(x)
}
