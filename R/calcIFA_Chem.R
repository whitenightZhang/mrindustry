#' Retrieve IFA (International fertilizer Association) data containing 
#' production, consumption, export and import volumes as well as capacities for 
#' ammonia and urea.
#'
#' @param subtype Character string specifying the type of IFA product data to read.
#'                Available types are:
#'                \itemize{
#'                  \item For product: ammonia, urea
#'                  \item For data sheet: statistics, capacities
#'                  \item For product characteristics: consumption, production, export, import, capacities
#'                }
#' @param unitNitrogen  boolean parameter
#'                      TRUE to return data in unit KtN 
#'                      FALSE to return data in Kt product
#'                
#' @return magpie object of the IFA data
#' @author Qianzhi Zhang
#'
#' @export
calcIFA_Chem <- function(subtype, unitNitrogen = FALSE) {
  x <- readSource("IFA_Chem", subtype = subtype)
  
  if (unitNitrogen == TRUE){
    subtype <- unlist(strsplit(subtype, "_"))
    if (subtype[1] == "ammonia") {
      x <- x/17*14
    } else if (subtype[1] == "urea") {
      x <- x/60*28
    } 
  }
  
  return(list(
    x = x,
    weight = NULL,
    unit = "Kt Product (or KtN for unitNitrogen=TRUE)",
    description = "IFA data which contains production, capacity, consumption and import/export data for ammonia and urea."
  ))
}
