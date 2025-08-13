#' Convert IEA_Ammonia
#'
#' Convert IEA Ammonia Technology Roadmap 2021 “Fig 2.9 Ammonia production by process route and
#' scenario in major ammonia producing regions” data to ISO country level.
#'
#' @param x MagPIE object containing IEA Ammonia data at regional resolution.
#' @return MagPIE object of the IEA Ammonia data disaggregated to country level.
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#'   a <- convertIEA_Ammonia(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset
convertIEA_Ammonia <- function(x) {
  
  # ---------------------------------------------------------------------------
  # Define "Others" Regions
  #    - These regions represent aggregated regions from the dataset.
  # ---------------------------------------------------------------------------
  others <- c(
    "North America", "Europe", "Eurasia", "Middle East", "Africa", "Central and South America"
  )
  
  # ---------------------------------------------------------------------------
  # Separate Regional and Country-level Data
  #    - xReg: Extract data corresponding to the aggregated "others" regions.
  #    - xCtry: Extract individual country data by excluding the "others" regions.
  # ---------------------------------------------------------------------------
  xReg <- x[others, , ]
  xCtry <- x[others, , invert = TRUE]
  
  # ---------------------------------------------------------------------------
  # Convert Country Names to ISO Codes
  #    - Convert the names in the country-level data (xCtry) to ISO codes.
  # ---------------------------------------------------------------------------
  getItems(xCtry, dim = 1) <- toolCountry2isocode(getItems(xCtry, dim = 1))
  
  # ---------------------------------------------------------------------------
  # Load Regional-to-Country Mapping
  #    - Retrieve mapping that converts IFA region codes (IFAReg) to ISO country codes.
  #    - Exclude regions labeled as "rest".
  # ---------------------------------------------------------------------------
  map <- toolGetMapping("regionmappingIEA_Ammonia.csv", type = "regional", where = "mrindustry") %>%
    dplyr::filter(.data$IFAReg != "rest")
  
  # ---------------------------------------------------------------------------
  # Retrieve Weighting Data
  #    - Get the ammonia production data (IFA_Chem ammonia statistics) for 2020 to be used as weights.
  # ---------------------------------------------------------------------------
  Ammonia <- calcOutput("IFA_Chem", subtype = "ammonia_statistics_production", aggregate = FALSE)[, "y2020", ]
  
  # ---------------------------------------------------------------------------
  # Aggregate Regional Data to Country Level
  #    - Aggregate the aggregated "others" regions (xReg) to country level using the mapping and weights.
  # ---------------------------------------------------------------------------
  xReg <- toolAggregate(
    xReg,
    rel = map,
    dim = 1,
    from = "IFAReg",
    to = "CountryCode",
    weight = Ammonia[unique(map$CountryCode), , ]
  )
  
  # ---------------------------------------------------------------------------
  # Combine Aggregated Regional Data with Individual Country Data
  # ---------------------------------------------------------------------------
  x <- mbind(xReg, xCtry)
  
  # ---------------------------------------------------------------------------
  # Fill Missing Country Data with 0
  #    - Ensure completeness by replacing any missing values with 0.
  # ---------------------------------------------------------------------------
  x <- toolCountryFill(x, fill = 0)
  
  # ---------------------------------------------------------------------------
  # Return the Final Disaggregated MagPIE Object
  # ---------------------------------------------------------------------------
  return(x)
}
