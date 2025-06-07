#'
#' @author Qianzhi Zhang
#'
#' @export
calcIFA_Chem <- function(subtype) {
  x <- readSource("IFA_Chem", subtype = subtype)
  return(list(
    x = x,
    weight = NULL,
    unit = "Kt Product",
    description = "IFA data which contains production, capacity, consumption and import/export data for ammonia and urea."
  ))
}
