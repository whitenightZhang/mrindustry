#' Convert IFA_Chem
#'
#' Convert IFA_Chem data to ISO country level.
#'
#' @param x MagPIE object containing IFA data at region resolution.
#' @return MagPIE object of the IFA data disaggregated to country level.
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#'   a <- convertIFA_Chem(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset
convertIFA_Chem <- function(x) {
  # ---------------------------------------------------------------------------
  # Extract Subtype Information
  #    - Retrieve the "data" dimension name from x and split it by underscore.
  #    - This helps determine the processing route (ammonia vs. urea, statistics vs. capacities).
  # ---------------------------------------------------------------------------
  subtype <- unlist(strsplit(getNames(x, dim = "data"), "_"))

  # ---------------------------------------------------------------------------
  # Process Ammonia Data
  # ---------------------------------------------------------------------------
  if (subtype[1] == "ammonia") {
    
    # --- Capacities ---
    if (subtype[2] == "capacities") {
      # Convert country names to ISO codes with a specific mapping.
      getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1),
                                                  mapping = c("Taiwan, China" = "TWN"))
      # Fill missing country entries with 0.
      x <- toolCountryFill(x, fill = 0)
      
      # --- Statistics ---
    } else if (subtype[2] == "statistics") {
      # Define "Others" regions to be aggregated.
      others <- c("Others West Europe", "Others Central Europe", "Others EECA",
                  "Others Latin America", "Others Africa", "Others West Asia",
                  "Others South Asia", "Others East Asia", "Others", "Others Caribbean")
      
      # Extract the "Others" subset.
      xReg <- x[others, , ]
      
      # Aggregate specific "Others" groups:
      # Combine "Others Latin America" with "Others Caribbean"
      xReg["Others Latin America", , ] <- xReg["Others Latin America", , ] +
        xReg["Others Caribbean", , ]
      # Combine "Others" with "Others EECA"
      xReg["Others", , ] <- xReg["Others", , ] +
        xReg["Others EECA", , ]
      
      # Remove rows that have been aggregated out.
      xReg <- xReg[!(rownames(xReg) %in% c("Others Caribbean", "Others EECA")), , drop = FALSE]
      
      # Extract individual countries (not in "others").
      xCtry <- x[others, , invert = TRUE]
      # Convert these country names to ISO codes.
      getItems(xCtry, dim = 1) <- toolCountry2isocode(getItems(xCtry, dim = 1),
                                                      mapping = c("Taiwan, China" = "TWN"))
      
      # Read regional mapping file for ammonia.
      map <- toolGetMapping("regionmappingIFA_Chem_ammonia.csv", type = "regional", where = "mrindustry") %>%
        dplyr::filter(.data$IFAReg != "rest")
      
      # Retrieve final energy data for weighting (from IEA FE data).
      fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)[, "y2015", "FE (EJ/yr)"]
      
      # Aggregate the "Others" regions to country level using the mapping and FE data.
      xReg <- toolAggregate(xReg, rel = map, from = "IFAReg", to = "CountryCode", weight = fe[unique(map$CountryCode), , ])
      
      # Combine the aggregated "others" with the individual countries.
      x <- mbind(xReg, xCtry)
    }
    
    # ---------------------------------------------------------------------------
    # Process Urea Data
    # ---------------------------------------------------------------------------
  } else if (subtype[1] == "urea") {
    
    # --- Capacities ---
    if (subtype[2] == "capacities") {
      # Convert country names to ISO codes.
      getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1),
                                                  mapping = c("Taiwan, China" = "TWN"))
      # Fill missing countries with 0.
      x <- toolCountryFill(x, fill = 0)
      
      # --- Statistics ---
    } else if (subtype[2] == "statistics") {
      # Define "Others" regions for urea data.
      others <- c("Others West Europe", "Others Central Europe", "Others EECA",
                  "Others Latin America", "Others Africa", "Others West Asia",
                  "Others Oceania", "Others East Asia", "Others", "Others Caribbean")
      
      # Extract "Others" regions.
      xReg <- x[others, , ]
      
      # Aggregate specific groups:
      xReg["Others Latin America", , ] <- xReg["Others Latin America", , ] +
        xReg["Others Caribbean", , ]
      xReg["Others", , ] <- xReg["Others", , ] +
        xReg["Others Oceania", , ] +
        xReg["Others EECA", , ] +
        xReg["Others Central Europe", , ]
      
      # Remove aggregated rows.
      xReg <- xReg[!(rownames(xReg) %in% c("Others Caribbean", "Others Oceania", "Others EECA", "Others Central Europe")), , drop = FALSE]
      
      # Extract individual country-level data (plus Kosovo).
      xCtry <- x[c(others, "Kosovo"), invert = TRUE]
      # Convert country names to ISO codes.
      getItems(xCtry, dim = 1) <- toolCountry2isocode(getItems(xCtry, dim = 1),
                                                      mapping = c("Taiwan, China" = "TWN"))
      
      # Read regional mapping file for urea.
      map <- toolGetMapping("regionmappingIFA_Chem_urea.csv", type = "regional", where = "mrindustry") %>%
        dplyr::filter(.data$IFAReg != "rest")
      
      # Retrieve final energy data for weighting.
      fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)[, "y2015", "FE (EJ/yr)"]
      
      # Aggregate the "Others" regions to country level.
      xReg <- toolAggregate(xReg, rel = map, from = "IFAReg", to = "CountryCode", weight = fe[unique(map$CountryCode), , ])
      
      # Combine aggregated "others" with individual country data.
      x <- mbind(xReg, xCtry)
      
      # Try to Fix Negative data
      x <- abs(x)
    }
    
    # ---------------------------------------------------------------------------
    # Handle Invalid Subtype Combination
    # ---------------------------------------------------------------------------
  } else {
    stop("Invalid subtype combination")
  }
  
  # ---------------------------------------------------------------------------
  # Return the Processed MagPIE Object
  # ---------------------------------------------------------------------------
  return(x)
}

