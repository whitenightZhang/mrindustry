#' Convert IEA_Petrochem
#'
#' Convert IEA The Future of Petrochemicals 2018 data (including several figures such as
#' "Fig 4.1 Petrochem Production", "Fig A.1 Petrochem Prod Region", "Fig 4.5 Petrochem Feedstock",
#' "Fig 4.9 Petro Prod Route RTS", "Fig 5.10 Petro Prod Route CTS") into a MagPIE object
#' aggregated to ISO country level.
#'
#' @param x MagPIE object containing IEA Petrochem data at regional resolution.
#' @return MagPIE object of the IEA Petrochem data disaggregated to country level.
#' @author Qianzhi
#' @examples
#' \dontrun{
#'   a <- convertIEA_Petrochem(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset
convertIEA_Petrochem <- function(x) {
  # ---------------------------------------------------------------------------
  # Extract Subtype Information
  #    - The subtype is extracted from the comment attached to x.
  #    - This string is then split by "_" to determine which processing branch to use.
  # ---------------------------------------------------------------------------
  subtype <- unlist(
    strsplit(
      strsplit(getComment(x), " ")[[1]][3], "_"
    )
  )
  
  # ---------------------------------------------------------------------------
  # Process "production3type" Data
  #    - When the subtype indicates "production3type", the data are disaggregated
  #      by extracting separate chemical components (Ammonia, Methanol, HVCs).
  #    - Weighting data are retrieved from corresponding IFA_Chem, IHS_Meth, and ChemicalTotal datasets.
  #    - Regional data are aggregated to country level using a 7-region mapping.
  # ---------------------------------------------------------------------------
  if (subtype[1] == "production3type") {
    # Retrieve weighting data for each chemical component
    Ammonia <- calcOutput("IFA_Chem", subtype = "ammonia_statistics_production", aggregate = FALSE)[, "y2017", ]
    Methanol_P <- calcOutput("IHS_Meth", subtype ="Production_2010-2020", aggregate = FALSE)[, "y2017", ]
    Chemcial_Total <- calcOutput("ChemicalTotal", aggregate = FALSE)[, "y2017", ]
    
    # Extract specific chemical slices from x
    x_Ammonia <- x[, , "Ammonia"]
    x_Methanol <- x[, , "Methanol"]
    x_HVCs     <- x[, , "HVCs"]
    
    # Load regional-to-country mapping for 7 regions, excluding "rest" regions
    file_name <- "regionmappingIEA_Petrochem_7region.csv"
    map <- toolGetMapping(file_name, type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$IFAReg != "rest")
    
    # Aggregate each chemical component to country level using corresponding weights
    x_Ammonia <- toolAggregate(x_Ammonia, rel = map, dim = 1, from = "IFAReg", to = "CountryCode",
                               weight = Ammonia[unique(map$CountryCode), , ])
    x_Methanol <- toolAggregate(x_Methanol, rel = map, dim = 1, from = "IFAReg", to = "CountryCode",
                                weight = Methanol_P[unique(map$CountryCode), , ])
    x_HVCs <- toolAggregate(x_HVCs, rel = map, dim = 1, from = "IFAReg", to = "CountryCode",
                            weight = Chemcial_Total[unique(map$CountryCode), , ])
    
    # Combine aggregated results from the three components
    x <- mbind(x_Ammonia, x_Methanol, x_HVCs)
    
    # ---------------------------------------------------------------------------
    # Process "production5type" Data
    #    - When the subtype indicates "production5type", a specific weight is chosen based on
    #      the second component (e.g., "Ammonia", "Methanol", "Ethylene", etc.).
    #    - The entire x object is then aggregated to the country level using that specific weight.
    # ---------------------------------------------------------------------------
  } else if (subtype[1] == "production5type") {
    # Retrieve weighting data for production5type
    Ammonia <- calcOutput("IFA_Chem", subtype = "ammonia_statistics_production", aggregate = FALSE)[, "y2017", ]
    Methanol_P <- calcOutput("IHS_Meth", subtype ="Production_2010-2020", aggregate = FALSE)[, "y2017", ]
    Chemcial_Total <- calcOutput("ChemicalTotal", aggregate = FALSE)[, "y2017", ]
    
    # Load mapping for 7 regions
    file_name <- "regionmappingIEA_Petrochem_7region.csv"
    map <- toolGetMapping(file_name, type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$IFAReg != "rest")
    
    # Choose the appropriate weight based on the second component of the subtype
    specific_weight <- switch(
      subtype[2],
      "Ammonia" = Ammonia[unique(map$CountryCode), , ],
      "Methanol" = Methanol_P[unique(map$CountryCode), , ],
      "Ethylene" = Chemcial_Total[unique(map$CountryCode), , ],
      "Propylene" = Chemcial_Total[unique(map$CountryCode), , ],
      "BTX" = Chemcial_Total[unique(map$CountryCode), , ]
    )
    
    # Aggregate the entire x object using the chosen weight
    x <- toolAggregate(x, rel = map, dim = 1, from = "IFAReg", to = "CountryCode", specific_weight)
    
    # ---------------------------------------------------------------------------
    # Process Feedstock and Route Data
    #    - For subtypes "Feedstock", "RouteRTS", or "RouteCTS", use a 5-region mapping.
    #    - Weighting is done using the total chemical energy data.
    # ---------------------------------------------------------------------------
  } else if (subtype[1] == "Feedstock" || subtype[1] == "RouteRTS" || subtype[1] == "RouteCTS") {
    # Load 5-region mapping for feedstock/route data, excluding "rest" regions
    file_name <- "regionmappingIEA_Petrochem_5region.csv"
    map <- toolGetMapping(file_name, type = "regional", where = "mrindustry") %>%
      dplyr::filter(.data$IFAReg != "rest")
    
    # Retrieve overall weighting data for chemical energy
    ChemFE_TOT <- calcOutput("ChemicalTotal", warnNA = FALSE, aggregate = FALSE)[, "y2017", ]
    
    # Aggregate the x object using the 5-region mapping and overall chemical energy weights
    x <- toolAggregate(x, rel = map, dim = 1, from = "IFAReg", to = "CountryCode",
                       weight = ChemFE_TOT[unique(map$CountryCode), , ])
  } else {
    # -------------------------------------------------------------------------
    # Error Handling
    #    - Stop if the subtype combination is invalid.
    # -------------------------------------------------------------------------
    stop("Invalid subtype combination")
  }
  
  # ---------------------------------------------------------------------------
  # 6. Finalize and Return
  #    - Fill missing country entries with 0 to ensure completeness.
  # ---------------------------------------------------------------------------
  x <- toolCountryFill(x, fill = 0)
  
  return(x)
}
