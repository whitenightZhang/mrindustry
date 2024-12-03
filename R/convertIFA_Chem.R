#' Convert IFA_Chem
#'
#' Convert IFA_Chem data to ISO country level.
#'
#'
#' @param x MAgPIE object containing IFA data region resolution
#' @return MAgPIE object of the IFA data disaggregated to country level
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#' a <- convertIFA_Chem(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset

convertIFA_Chem <- function(x) {
  # Extract the subtype information from the "data" dimension of x
  subtype <- unlist(strsplit(getNames(x, dim = "data"), "_"))
  
  # Check if the first subtype is "ammonia"
  if (subtype[1] == "ammonia") {
    
    if (subtype[2] == "capacities") {
      # For ammonia capacities, convert country names to ISO codes
      getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1), mapping = c("Taiwan, China" = "TWN"))
      # Fill missing countries with 0
      x <- toolCountryFill(x, fill = 0)
      
    } else if (subtype[2] == "statistics") {
      # Handle ammonia statistics
      
      # Define the "Others" regions to aggregate
      others <- c(
        "Others West Europe", "Others Central Europe", "Others EECA",
        "Others Latin America", "Others Africa", "Others West Asia", "Others South Asia",
        "Others East Asia", "Others", "Others Caribbean"
      )
      
      # Extract the "Others" regions
      xReg <- x[others, , ]
      
      # Aggregate specific regions
      xReg["Others Latin America", , ] <- xReg["Others Latin America", , ] + xReg["Others Caribbean", , ]
      xReg["Others", , ] <- xReg["Others", , ] + xReg["Others EECA", , ]
      
      # Remove unwanted rows
      xReg <- xReg[!(rownames(xReg) %in% c("Others Caribbean", "Others EECA")), , drop = FALSE]
      
      # Extract individual countries
      xCtry <- x[others, , invert = TRUE]
      # Convert country names to ISO codes
      getItems(xCtry, dim = 1) <- toolCountry2isocode(getItems(xCtry, dim = 1), mapping = c("Taiwan, China" = "TWN"))
      
      # Read region mapping file
      map <- toolGetMapping("regionmappingIFA_Chem_ammonia.csv", type = "regional", where = "mrindustry") %>%
        dplyr::filter(.data$IFAReg != "rest")
      
      # Retrieve final energy (FE) data for weighting, TODO:: Should be discussed whether to use different year.
      fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)[, "y2015", "FE (EJ/yr)"]
      
      # Aggregate "Others" regions to countries using weights
      xReg <- toolAggregate(xReg, rel = map, from = "IFAReg", to = "CountryCode", weight = fe[unique(map$CountryCode), , ])
      
      # Combine regional and country data
      x <- mbind(xReg, xCtry)
    }
    
  } else if (subtype[1] == "urea") {
    
    if (subtype[2] == "capacities") {
      # For urea capacities, convert country names to ISO codes
      getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1), mapping = c("Taiwan, China" = "TWN"))
      # Fill missing countries with 0
      x <- toolCountryFill(x, fill = 0)
      
    } else if (subtype[2] == "statistics") {
      # Handle urea statistics
      
      # Define the "Others" regions to aggregate
      others <- c(
        "Others West Europe", "Others Central Europe", "Others EECA",
        "Others Latin America", "Others Africa", "Others West Asia", "Others Oceania",
        "Others East Asia", "Others", "Others Caribbean"
      )
      
      # Extract the "Others" regions
      xReg <- x[others, , ]
      
      # Aggregate specific regions
      xReg["Others Latin America", , ] <- xReg["Others Latin America", , ] + xReg["Others Caribbean", , ]
      xReg["Others", , ] <- xReg["Others", , ] + xReg["Others Oceania", , ] + xReg["Others EECA", , ] + xReg["Others Central Europe", , ]
      
      # Remove unwanted rows
      xReg <- xReg[!(rownames(xReg) %in% c("Others Caribbean", "Others Oceania", "Others EECA", "Others Central Europe")), , drop = FALSE]
      
      # Extract individual countries
      xCtry <- x[c(others, "Kosovo"), invert = TRUE]
      # Convert country names to ISO codes
      getItems(xCtry, dim = 1) <- toolCountry2isocode(getItems(xCtry, dim = 1), mapping = c("Taiwan, China" = "TWN"))
      
      # Read region mapping file
      map <- toolGetMapping("regionmappingIFA_Chem_urea.csv", type = "regional", where = "mrindustry") %>%
        dplyr::filter(.data$IFAReg != "rest")
      
      # Retrieve final energy (FE) data for weighting
      fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)[, "y2015", "FE (EJ/yr)"]
      
      # Aggregate "Others" regions to countries using weights
      xReg <- toolAggregate(xReg, rel = map, from = "IFAReg", to = "CountryCode", weight = fe[unique(map$CountryCode), , ])
      
      # Combine regional and country data
      x <- mbind(xReg, xCtry)
    }
    
  } else {
    # Stop if the subtype combination is invalid
    stop("Invalid subtype combination")
  }
  
  # Return the processed object
  return(x)
}

