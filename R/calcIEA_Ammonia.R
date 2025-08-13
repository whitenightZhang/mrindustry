#' Read-in IEA Ammonia Technology Roadmap 2021 Fig 2.9 Ammonia production by process route and
#' scenario in major ammonia producing regions data as a magclass object.
#'
#' @param subtype Different scenarios of Ammonia data that should be read. Available types are:
#'                \itemize{
#'                  \item BaseYear_2020: Base year data in 2020
#'                  \item STEPS_2050: IEA STEPS scenario in 2050
#'                  \item SDS_2050: IEA SDS scenario in 2050
#'                }
#'
#' @author Qianzhi Zhang
#'
#' @export
calcIEA_Ammonia <- function(subtype) {
  x <- readSource("IEA_Ammonia", subtype = subtype) 
  
  # ---------------------------------------------------------------------------
  # Gas with CCS route in 2020 is only employed in the US. The aggregation performed in convertIEA_Ammonia assigned it 
  # to all North American countries. To fix this, the total of "Gas_with_CCS" category shares are reassigned to USA and of all other regions set to zero.
  # ---------------------------------------------------------------------------
  if (subtype == "BaseYear_2020") {
    # total Gas_with_CCS
    am_syngcc_total <- sum(x[, , "Gas_with_CCS"], na.rm = TRUE)
    
    # Set only USA to total, others to zero
    x[, , "Gas_with_CCS"] <- ifelse(getItems(x, dim = 1) == "USA",
                                    am_syngcc_total,
                                    0)
  }
  
  # Calculate normalized shares within each Region-Year
  totals <- dimSums(x, dim = 3)  # keep Region & Year, drop Data1
  normalized <- x / totals * 100
  normalized[is.nan(normalized)] <- 0
  x <- normalized
  
  # ---------------------------------------------------------------------------
  # Retrieve Weighting Data
  #    - Get the ammonia production data (IFA_Chem ammonia statistics) for 2020 to be used as weights.
  # ---------------------------------------------------------------------------
  Ammonia <- calcOutput("IFA_Chem", subtype = "ammonia_statistics_production", aggregate = FALSE)[, "y2020", ]
  Ammonia[c("AUS", "NZL"), , ] <- 0 # set weight to zero as there is no information on routes and otherwise the shares for CAZ region will not sum up to 100%
  
  return(list(
    x = x,
    weight = Ammonia,
    unit = "%",
    description = "Share on ammonia production routes in different regions from IEA Ammonia Technology Roadmap 2021"
  ))
}
