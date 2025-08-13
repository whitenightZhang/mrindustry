#' Convert IHS_Meth
#'
#' Convert 9TH RUSSIA & CIS OIL & GAS EXECUTIVE SUMMIT 2019 METHANOL INS.xlsx data to ISO country level.
#'
#' @param x MagPIE object containing IHS_Meth data at regional resolution.
#' @return MagPIE object of the IHS_Meth data disaggregated to country level.
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#'   a <- convertIHS_Meth(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset
convertIHS_Meth <- function(x) {
  # ---------------------------------------------------------------------------
  # Extract Subtype Information
  #    - Retrieve the subtype string from the comment attached to x.
  #    - Split the subtype string to determine the temporal resolution (2010-2020 vs. 2018)
  #      and the data type (Production, Capacity, or Demand).
  # ---------------------------------------------------------------------------
  subtype <- getComment(x)
  subtype <- unlist(strsplit(strsplit(getComment(x), " ")[[1]][3], "_"))
  
  # ---------------------------------------------------------------------------
  # Separate China-specific Data from Other Countries
  #    - xReg: Data for China.
  #    - xCtry: Data for all other countries.
  # ---------------------------------------------------------------------------
  xReg <- x["China", , ]        # Data specific to China
  xCtry <- x["China", , invert = TRUE]  # All other countries
  
  # ---------------------------------------------------------------------------
  # Convert Country Names in xReg to ISO Codes
  # ---------------------------------------------------------------------------
  getItems(xReg, dim = 1) <- toolCountry2isocode(getItems(xReg, dim = 1))
  
  # ---------------------------------------------------------------------------
  # Load Regional-to-Country Mapping
  #    - Load the mapping file for IHS_Meth (assumed for IEA or similar)
  #    - Exclude regions labeled as "rest"
  # ---------------------------------------------------------------------------
  file_name <- "regionmappingIHS_Meth_2018.csv"
  map <- toolGetMapping(file_name, type = "regional", where = "mrindustry") %>%
    dplyr::filter(.data$IFAReg != "rest")
  
  # ---------------------------------------------------------------------------
  # Aggregate Data Based on Temporal Resolution
  #    - For the 2010-2020 case: Loop over each year from 2010 to 2020.
  #    - For each year, retrieve the appropriate weighting data and aggregate xCtry.
  #    - For the Demand case, use FE data as weights.
  # ---------------------------------------------------------------------------
  if (subtype[2] == "2010-2020") {
    years <- paste0("y", 2010:2020)
    results <- list()
    
    if (subtype[1] %in% c("Production", "Capacity")) {
      # Retrieve total chemical production weighting data
      ChemTotal <- calcOutput("ChemicalTotal", aggregate = FALSE)
      
      for (year in years) {
        # Extract weighting data for the current year from TOTAL.CHEMICAL.NECHEM
        ChemTotal_year <- ChemTotal[, year, "TOTAL.CHEMICAL.NECHEM"]
        
        # Aggregate country-level data for the current year using the mapping
        results[[year]] <- toolAggregate(
          xCtry[, year, ],  # Data for current year
          rel = map,
          dim = 1,
          from = "IFAReg",
          to = "CountryCode",
          weight = ChemTotal_year[unique(map$CountryCode), , drop = FALSE]
        )
      }
    } else if (subtype[1] == "Demand") {
      # Retrieve final energy (FE) data for weighting
      fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)
      
      for (year in years) {
        # Extract FE data for the current year
        fe_year <- fe[, year, "FE (EJ/yr)"]
        
        # Aggregate country-level data for the current year using the mapping
        results[[year]] <- toolAggregate(
          xCtry[, year, ],  # Data for current year
          rel = map,
          dim = 1,
          from = "IFAReg",
          to = "CountryCode",
          weight = fe_year[unique(map$CountryCode), , drop = FALSE]
        )
      }
    } else {
      stop("Invalid subtype combination")
    }
    
    # Combine the aggregated results for all years into one MagPIE object
    x_aggregated <- do.call(mbind, results)
    x <- mbind(x_aggregated, xReg)
    
  } else if (subtype[2] == "2018") {
    # For the 2018 case, aggregate only for the year 2018.
    if (subtype[1] %in% c("Production", "Capacity")) {
      ChemTotal <- calcOutput("ChemicalTotal", aggregate = FALSE)
      ChemTotal_2018 <- ChemTotal[, "y2018", "TOTAL.CHEMICAL.NECHEM"]
      x <- toolAggregate(
        xCtry[, "y2018", ],
        rel = map,
        dim = 1,
        from = "IFAReg",
        to = "CountryCode",
        weight = ChemTotal_2018[unique(map$CountryCode), , drop = FALSE]
      )
      x <- mbind(x, xReg)
      
    } else if (subtype[1] == "Demand") {
      fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)
      fe_2018 <- fe[, "y2018", "FE (EJ/yr)"]
      x <- toolAggregate(
        xCtry[, "y2018", ],
        rel = map,
        dim = 1,
        from = "IFAReg",
        to = "CountryCode",
        weight = fe_2018[unique(map$CountryCode), , drop = FALSE]
      )
      x <- mbind(x, xReg)
    } else {
      stop("Invalid subtype combination")
    }
  } else {
    stop("Invalid subtype combination")
  }
  
  # ---------------------------------------------------------------------------
  # Finalize and Return the Aggregated Data
  #    - Fill missing country entries with 0.
  # ---------------------------------------------------------------------------
  x <- toolCountryFill(x, fill = 0)
  
  return(x)
}
