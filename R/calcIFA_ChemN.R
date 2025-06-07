#'
#' @author Qianzhi Zhang
#'
#' @export
calcIFA_ChemN <- function(subtype) {
  
  x <- readSource("IFA_Chem", subtype = subtype)
  subtype <- unlist(strsplit(subtype, "_"))
  
  if (subtype[1] == "ammonia") {
    x <- x/17*14
  } else if (subtype[1] == "urea") {
    x <- x/60*28
  } 
  
  return(list(
    x = x,
    weight = NULL,
    unit = "Kt N",
    description = "IFA data which contains production, capacity, consumption and import/export data for ammonia and urea."
  ))
}
