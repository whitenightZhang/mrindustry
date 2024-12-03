#' Convert IEA_Petrochem
#'
#' Convert IEA The Future of  Petrochemicals 2018 “Fig 4.1 Petrochem Production”, "Fig A.1 Petrochem Prod Region",
#' "Fig 4.5 Petrochem Feedstock","Fig 4.9 Petro Prod Route RTS","Fig 5.10 Petro Prod Route CTS" data as magclass object
#'
#'
#' @param x MAgPIE object containing IEA Ammonia data region resolution
#' @return MAgPIE object of the IEA Ammonia data disaggregated to country level
#' @author Qianzhi
#' @examples
#' \dontrun{
#' a <- convertIEA_Petrochem(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset

convertIEA_Petrochem <- function(x) {
  # Extract the subtype from the magpie object
  subtype <- getComment(x)
  
  # Handle "production3type" or "production5type"
  if (subtype == "production3type" || subtype == "production5type") {
    
    # Retrieve total chemical production data for weighting
    ChemTotal <- calcOutput("ChemicalTotal", aggregate = FALSE)[, "y2017", "TOTAL.CHEMICAL.NECHEM"]
    
    # Load the regional-to-country mapping
    file_name <- "regionmappingIEA_Petrochem_7region.csv"
    map <- toolGetMapping(file_name, type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$IFAReg != "rest")  # Exclude "rest" regions
    
    # Aggregate the data from regions to countries using the chemical total as weight
    x <- toolAggregate(x, rel = map, dim = 1, from = "IFAReg", to = "CountryCode", weight = ChemTotal[unique(map$CountryCode), , ])
    
  } else if (subtype == "Feedstock" || subtype == "RouteRTS" || subtype == "RouteCTS") {
    
    # Retrieve feedstock data for solids, gases, and liquids
    ChemFeed <- calcOutput("ChemicalFeedstocks", aggregate = FALSE)
    ChemFeed_solids <- ChemFeed[, "y2017", "solids"]
    ChemFeed_gas <- ChemFeed[, "y2017", "gases"]
    ChemFeed_liq <- ChemFeed[, "y2017", "liquids"]
    ChemFeed_TOT <- ChemFeed_solids + ChemFeed_gas + ChemFeed_liq  # Total feedstock
    
    # Load the regional-to-country mapping
    file_name <- "regionmappingIEA_Petrochem_5region.csv"
    map <- toolGetMapping(file_name, type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$IFAReg != "rest")  # Exclude "rest" regions
    
    # Define feedstock categories and filter for available columns
    Solid_columns <- c("Coal", "MTO_MTA", "Coal_GS", "COG_SR", "Bioenergy", "BDH", "MTO/MTA", "Bio_GS")
    Gas_columns <- c("Ethane", "Natural_gas", "STC_light", "NG_SR")
    Oil_columns <- c("Naphtha", "Other_oil", "STC_heavy", "PDH_NCC", "Oil_SR")
    
    available_columns <- dimnames(x)[[3]]  # Retrieve available columns in x
    Solid_columns <- intersect(Solid_columns, available_columns)  # Filter by available columns
    Gas_columns <- intersect(Gas_columns, available_columns)
    Oil_columns <- intersect(Oil_columns, available_columns)
    
    # Extract data for each feedstock type
    x_sol <- x[, , Solid_columns]
    x_gas <- x[, , Gas_columns]
    x_oil <- x[, , Oil_columns]
    
    # Aggregate each feedstock type using the appropriate weight
    x_sol <- toolAggregate(x_sol, rel = map, dim = 1, from = "IFAReg", to = "CountryCode", weight = ChemFeed_solids[unique(map$CountryCode), , ])
    x_gas <- toolAggregate(x_gas, rel = map, dim = 1, from = "IFAReg", to = "CountryCode", weight = ChemFeed_gas[unique(map$CountryCode), , ])
    x_oil <- toolAggregate(x_oil, rel = map, dim = 1, from = "IFAReg", to = "CountryCode", weight = ChemFeed_liq[unique(map$CountryCode), , ])
    
    # Merge the aggregated data for solids, gases, and liquids
    x <- mbind(x_sol, x_gas, x_oil)
    
  } else {
    # Stop with an error message for invalid subtype combinations
    stop("Invalid subtype combination")
  }
  
  # Fill missing countries with 0 for completeness
  x <- toolCountryFill(x, fill = 0)
  
  # Return the processed magpie object
  return(x)
}

