#' Retrieves IFA (International fertilizer Association) data containing
#' production volumes and/or capacities for ammonium nitrate (AN), ammonium sulphate (AS),
#' calcium ammonium nitrate (CAN) and urea ammonium nitrate (UAN).
#'
#' @param subtype Character string indicating the IFA product and data type to read.
#'                Available combinations include:
#'                \itemize{
#'                  \item AN_statistics_production
#'                  \item AS_statistics_production
#'                  \item CAN_statistics_production
#'                  \item AN_capacities_capacities, AS_capacities_capacities, UAN_capacities_capacities
#'                }
#' @param unitNitrogen  boolean parameter
#'                      TRUE to return production statistics in unit KtN 
#'                      FALSE to return production statistics in Kt product
#'                      capacities are always in KtN
#'
#' @return Magpie object of the IFA data.
#'
#' @author Qianzhi Zhang
#'
#' @export
calcIFA_ChemAppend <- function(subtype, unitNitrogen = FALSE) {
  x <- readSource("IFA_ChemAppend", subtype = subtype)
  
  if (unitNitrogen==TRUE){
    subtype <- unlist(strsplit(subtype, "_"))
    
    if (subtype[2] == "statistics") {
      # conversion based on nitrogen content of the product
      if (subtype[1] == "AN") {
        x <- x*0.35 # 2*14/80
      } else if (subtype[1] == "AS") {
        x <- x*0.21 # 2*14/132
      } else if (subtype[1] == "CAN") {
        x <- x*0.27 # nitrogen content of CAN according to IFASTAT.org
      }
    }
  }
  
  return(list(
    x = x,
    weight = NULL,
    unit = "Kt Product (or KtN for capacities and for unitNitrogen=TRUE)",
    description = "IFA data which contains production, capacity, consumption and import/export data for 
    ammonium nitrate (AN), ammonium sulphate (AS), calcium ammonium nitrate (CAN) and urea ammonium nitrate (UAN)."
  ))
}
