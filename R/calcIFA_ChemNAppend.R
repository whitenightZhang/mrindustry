#'
#' @author Qianzhi Zhang
#'
#' @export
calcIFA_ChemNAppend <- function(subtype) {
  x <- readSource("IFA_ChemAppend", subtype = subtype)
  subtype <- unlist(strsplit(subtype, "_"))
  
  if (subtype[2] == "statistics") {
    
    if (subtype[1] == "AN") {
      x <- x*0.35
    } else if (subtype[1] == "AS") {
      x <- x*0.21
    } else if (subtype[1] == "CAN") {
      x <- x*0.28
    }
  }
  
  return(list(
    x = x,
    weight = NULL,
    unit = "Kt N",
    description = "IFA data which contains production, capacity, consumption and import/export data for ammonia and urea."
  ))
}
