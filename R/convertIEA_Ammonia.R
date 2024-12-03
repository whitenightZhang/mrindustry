#' Convert IEA_Ammonia
#'
#' Convert IEA Ammonia Technology Roadmap 2021 “Fig 2.9 Ammonia production by process route and 
#' scenario in major ammonia producing regions” data to ISO country level.
#'
#'
#' @param x MAgPIE object containing IEA Ammonia data region resolution
#' @return MAgPIE object of the IEA Ammonia data disaggregated to country level
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#' a <- convertIEA_ammonia(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset

convertIEA_Ammonia <- function(x) {
  # Define the "others" regions (aggregate regions from the dataset)
  others <- c(
    "North America", "Europe", "Eurasia", "Middle East", "Africa", "Central and South America"
  )
  
  # Extract aggregated regions from the dataset
  xReg <- x[others, , ]
  
  # Extract country-level data by excluding the "others" regions
  xCtry <- x[others, , invert = TRUE]
  
  # Convert country names in xCtry to ISO codes
  getItems(xCtry, dim = 1) <- toolCountry2isocode(getItems(xCtry, dim = 1))
  
  # Load the mapping file for regional-to-country mappings
  map <- toolGetMapping("regionmappingIEA_Ammonia.csv", type = "regional", where = "mrindustry") %>%
    dplyr::filter(.data$IFAReg != "rest")  # Exclude regions labeled as "rest"
  
  # Aggregate the regional data (xReg) to the country level using the mapping
  xReg <- toolAggregate(xReg, rel = map, dim = 1, from = "IFAReg", to = "CountryCode")
  
  # Combine the aggregated regional data (xReg) and the country-level data (xCtry)
  x <- mbind(xReg, xCtry)
  
  # Fill missing countries with 0 to ensure completeness
  x <- toolCountryFill(x, fill = 0)
  
  # TODO:: Currently it can only be used for national scales and assumes that all countries in the same region have the same share.
  return(x)
}

