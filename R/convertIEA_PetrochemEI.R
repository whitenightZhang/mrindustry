#' Convert IEA_PetrochemEI
#'
#' Convert IEA Chemical and Petrochemical Sector 2009 “Table 12. Petro Regional Coef” data as magclass object
#'
#' @param x MAgPIE object containing IEA_PetrochemEI region resolution
#' @return MAgPIE object of the IEA_PetrochemEI data disaggregated to country level
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#' a <- convertIEA_PetrochemEI(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset

convertIEA_PetrochemEI <- function(x) {
  # ---------------------------------------------------------------------------
  # Load and Apply Initial Mapping
  #    - Load the mapping file "regionmappingIEA_PetrochemEI.csv" to convert 
  #      from IEA-specific country codes ("IEACountry") to a regional classification ("RegionCode").
  # ---------------------------------------------------------------------------
  map <- toolGetMapping("regionmappingIEA_PetrochemEI.csv", type = "regional", where = "mrindustry")
  x <- toolAggregate(x, rel = map, dim = 1, from = "IEACountry", to = "RegionCode")
  
  # ---------------------------------------------------------------------------
  # Further Aggregate to ISO Country Level
  #    - Load a broader mapping file "regionmappingH12.csv" to aggregate the data 
  #      from the regional level ("RegionCode") to the country level ("CountryCode").
  # ---------------------------------------------------------------------------
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode")
  
  # ---------------------------------------------------------------------------
  # Return the Final MagPIE Object
  # ---------------------------------------------------------------------------
  return(x)
}
