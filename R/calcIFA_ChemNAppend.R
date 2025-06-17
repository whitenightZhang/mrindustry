#'
#' @author Qianzhi Zhang
#'
#' @export
calcIFA_ChemNAppend <- function(subtype) {
  x <- readSource("IFA_ChemAppend", subtype = subtype)
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
  
  return(list(
    x = x,
    weight = NULL,
    unit = "Kt N",
    description = "IFA data which contains production, capacity, consumption and import/export data for 
    ammonium nitrate (AN), ammonium sulphate (AS), calcium ammonium nitrate (CAN) and urea ammonium nitrate (UAN)."
  ))
}
