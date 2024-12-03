#'
#' @author Qianzhi Zhang
#'
#' @export
calcIEA_Petrochem <- function(subtype) { 
  x <- readSource("IEA_Petrochem", subtype = subtype)
  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Product",
    description = "Data on production volumes, feedstocks and production routes for different chemical products from IEA The Future of  Petrochemicals 2018"
  ))
}
