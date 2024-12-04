#' Convert IHS_Meth
#'
#' Convert 9TH RUSSIA & CIS OIL & GAS EXECUTIVE SUMMIT 2019 METHANOL INS.xlsx data to ISO country level.
#'
#' @param x MAgPIE object containing IHS_Meth region resolution
#' @return MAgPIE object of the IHS_Meth data disaggregated to country level
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#' a <- convertIHS_Meth(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset

convertIHS_Meth <- function(x) {
  # Extract the subtype information from the magpie object
  subtype <- getComment(x)
  subtype <- unlist(strsplit(subtype, "_"))

  # Separate China-specific data from the rest
  xReg <- x["China", , ]  # Data specific to China
  xCtry <- x["China", , invert = TRUE]  # All other countries

  # Convert region names in xReg to ISO codes
  getItems(xReg, dim = 1) <- toolCountry2isocode(getItems(xReg, dim = 1))

  # Load the regional-to-country mapping file
  file_name <- "regionmappingIHS_Meth_2018.csv"
  map <- toolGetMapping(file_name, type = "regional", where = "mrindustry") %>%
    dplyr::filter(.data$IFAReg != "rest")  # Exclude regions labeled as "rest"

  # Handle cases where the subtype includes the year range "2010-2020"
  if (subtype[2] == "2010-2020") {
    years <- paste0("y", 2010:2020)

    if (subtype[1] == "Production" || subtype[1] == "Capacity") {
      # Aggregate based on chemical production or capacity for each year
      ChemTotal <- calcOutput("ChemicalTotal", aggregate = FALSE)
      results <- list()

      # Loop through each year
      for (year in years) {
        # Extract ChemTotal data for the current year
        ChemTotal_year <- ChemTotal[, year, "TOTAL.CHEMICAL.NECHEM"]

        # Perform aggregation for x using the current year's weights
        results[[year]] <- toolAggregate(
          xCtry[, year, ],  # Select data for the current year
          rel = map,
          dim = 1,
          from = "IFAReg",
          to = "CountryCode",
          weight = ChemTotal_year[unique(map$CountryCode), , drop = FALSE]  # Ensure weights match dimensions
        )
      }

      # Combine the results into a single magpie object
      x_aggregated <- do.call(mbind, results)
      x <- mbind(x_aggregated, xReg)

    } else if (subtype[1] == "Demand") {
      # Aggregate based on energy demand (FE) for each year
      fe <- calcOutput("FE", source = "IEA", aggregate = FALSE)
      results <- list()

      # Loop through each year
      for (year in years) {
        # Extract FE data for the current year
        fe_year <- fe[, year, "FE (EJ/yr)"]

        # Perform aggregation for x using the current year's weights
        results[[year]] <- toolAggregate(
          xCtry[, year, ],  # Select data for the current year
          rel = map,
          dim = 1,
          from = "IFAReg",
          to = "CountryCode",
          weight = fe_year[unique(map$CountryCode), , drop = FALSE]  # Ensure weights match dimensions
        )
      }

      # Combine the results into a single magpie object
      x_aggregated <- do.call(mbind, results)
      x <- mbind(x_aggregated, xReg)

    } else {
      stop("Invalid subtype combination")
    }

    # Handle cases where the subtype includes the specific year "2018"
  } else if (subtype[2] == "2018") {
    if (subtype[1] == "Production" || subtype[1] == "Capacity") {
      # Aggregate based on chemical production or capacity for 2018
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
      # Aggregate based on energy demand (FE) for 2018
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

  # Fill missing countries with 0 for consistency
  x <- toolCountryFill(x, fill = 0)

  # Return the processed magpie object
  return(x)
}
