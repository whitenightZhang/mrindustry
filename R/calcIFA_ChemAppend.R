#'
#' @author Qianzhi Zhang
#'
#' @export
calcIFA_ChemAppend <- function(subtype) {
  x <- readSource("IFA_ChemAppend", subtype = subtype)
  return(list(
    x = x,
    weight = NULL,
    unit = "Kt Product (or KtN for capacities)",
    description = "IFA data which contains production, capacity, consumption and import/export data for 
    ammonium nitrate (AN), ammonium sulphate (AS), calcium ammonium nitrate (CAN) and urea ammonium nitrate (UAN)."
  ))
}
