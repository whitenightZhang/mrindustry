#' Convert IFA_Chem
#'
#' Convert IFA_Chem data to ISO country level.
#'
#' @param x MagPIE object containing IFA data at regional resolution.
#' @return MagPIE object of the IFA data disaggregated to country level.
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#'   a <- convertIFA_ChemAppend(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset
convertIFA_ChemAppend <- function(x) {
  # ---------------------------------------------------------------------------
  # Extract Subtype Information
  #    - Retrieve the subtype from the comment attached to x.
  #    - Split the subtype string (e.g., "AN_statistics_production") into its components.
  # ---------------------------------------------------------------------------
  subtype <- unlist(strsplit(getComment(x), "_"))

  # ---------------------------------------------------------------------------
  # Process Based on Subtype: Capacities vs. Statistics
  # ---------------------------------------------------------------------------
  if (subtype[2] == "capacities") {
    # ------------------------------
    # Capacities Subtype Processing
    # ------------------------------
    # Define "Others" regions to aggregate
    others <- c(
      "West Europe", "Central Europe", "East Europe & Central Asia",
      "North America", "Latin America", "Africa", "West Asia", "South Asia",
      "East Asia", "Oceania"
    )
    
    # Extract aggregated regional data (xReg) and individual country data (xCtry)
    xReg <- x[others, , ]
    xCtry <- x[others, , invert = TRUE]
    
    # Convert individual country names to ISO codes (with special mapping)
    getItems(xCtry, dim = 1) <- toolCountry2isocode(getItems(xCtry, dim = 1), 
                                                    mapping = c("Taiwan, China" = "TWN"))
    
    # Read the regional mapping file for capacities
    map <- toolGetMapping("regionmappingIFA_Chem_othersC.csv", type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$IFAReg != "rest")
    
    # Retrieve weighting data from urea capacities
    urea <- calcOutput("IFA_Chem", subtype = "urea_capacities_capacities", aggregate = FALSE)
    
    # Determine available years from both datasets
    xReg_years <- as.numeric(sub("y", "", colnames(xReg)[grepl("^y\\d{4}$", colnames(xReg))]))
    urea_years <- as.numeric(sub("y", "", colnames(urea)[grepl("^y\\d{4}$", colnames(urea))]))
    start_year <- min(xReg_years, na.rm = TRUE)
    end_year   <- max(xReg_years, na.rm = TRUE)
    last_available_year <- max(urea_years, na.rm = TRUE)
    
    # Loop over each year and perform aggregation using appropriate weights
    for (year in start_year:end_year) {
      year_col <- paste0("y", year)
      weight_col <- if (year <= last_available_year) paste0("y", year) else paste0("y", last_available_year)
      
      if (weight_col %in% colnames(urea)) {
        aggregated_result <- toolAggregate(
          xReg[, year_col, drop = FALSE],
          rel = map,
          from = "IFAReg",
          to = "CountryCode",
          weight = urea[, weight_col, drop = FALSE][unique(map$CountryCode), , drop = FALSE]
        )
        if (year == start_year) {
          xNew <- aggregated_result
        } else {
          xNew <- mbind(xNew, aggregated_result)
        }
      } else {
        warning(paste("Weight column", weight_col, "is missing in urea data. Skipping aggregation for year", year, "."))
      }
    }
    
    # Combine aggregated "others" regions with individual country data
    x <- mbind(xNew, xCtry)
    
  } else if (subtype[2] == "statistics") {
    # ------------------------------
    # Statistics Subtype Processing
    # ------------------------------
    # Define "Others" regions for statistics
    others <- c(
      "Total West Europe", "Total Central Europe", "Total E_ Europe & C_ Asia",
      "Total North America", "Total Latin America", "Total Africa", 
      "Total West Asia", "Total South Asia", "Total East Asia", "Total Oceania", "Total Various"
    )
    
    # Extract aggregated regional data (xReg) and individual country data (xCtry)
    xReg <- x[others, , ]
    xCtry <- x[others, , invert = TRUE]
    
    # Convert individual country names to ISO codes
    getItems(xCtry, dim = 1) <- toolCountry2isocode(getItems(xCtry, dim = 1),
                                                    mapping = c("Taiwan, China" = "TWN"))
    
    # Read the regional mapping file for statistics
    map <- toolGetMapping("regionmappingIFA_Chem_othersP.csv", type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$IFAReg != "rest")
    
    # Retrieve weighting data from urea statistics (using third component from subtype)
    urea <- calcOutput("IFA_Chem", subtype = paste0("urea_statistics_", subtype[3]), aggregate = FALSE)
    
    # Determine available years from both datasets
    xReg_years <- as.numeric(sub("y", "", colnames(xReg)[grepl("^y\\d{4}$", colnames(xReg))]))
    urea_years <- as.numeric(sub("y", "", colnames(urea)[grepl("^y\\d{4}$", colnames(urea))]))
    start_year <- min(xReg_years, na.rm = TRUE)
    end_year   <- max(xReg_years, na.rm = TRUE)
    last_available_year <- max(urea_years, na.rm = TRUE)
    
    # Loop over each year and perform aggregation using appropriate weights
    for (year in start_year:end_year) {
      year_col <- paste0("y", year)
      weight_col <- if (year <= last_available_year) paste0("y", year) else paste0("y", last_available_year)
      
      if (weight_col %in% colnames(urea)) {
        aggregated_result <- toolAggregate(
          xReg[, year_col, drop = FALSE],
          rel = map,
          from = "IFAReg",
          to = "CountryCode",
          weight = urea[, weight_col, drop = FALSE][unique(map$CountryCode), , drop = FALSE]
        )
        if (year == start_year) {
          xNew <- aggregated_result
        } else {
          xNew <- mbind(xNew, aggregated_result)
        }
      } else {
        warning(paste("Weight column", weight_col, "is missing in urea data. Skipping aggregation for year", year, "."))
      }
    }
    
    # Combine aggregated "others" regions with individual country data
    x <- mbind(xNew, xCtry)
    
  } else {
    stop("Invalid subtype combination")
  }
  
  # ---------------------------------------------------------------------------
  # Finalize Output: Fill Missing Country Data with 0
  # ---------------------------------------------------------------------------
  x <- toolCountryFill(x, fill = 0)
  
  # Return the processed MagPIE object
  return(x)
}
