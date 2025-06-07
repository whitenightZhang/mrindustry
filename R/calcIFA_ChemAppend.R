#'
#' @author Qianzhi Zhang
#'
#' @export
calcIFA_ChemAppend <- function(subtype) {
  x <- readSource("IFA_ChemAppend", subtype = subtype)
  return(list(
    x = x,
    weight = NULL,
    unit = "Kt Product",
    description = "IFA data which contains production, capacity, consumption and import/export data for ammonia and urea."
  ))
}
