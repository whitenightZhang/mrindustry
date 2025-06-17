#'
#' @author Qianzhi Zhang
#'
#' @export
calcIEA_PetrochemEI <- function() {
  
  x <- readSource("IEA_PetrochemEI")
  weight <- x # get the same dimensions of the data
  weight[, , ] <- 1
  
  return(list(
    x = x,
    weight = weight,
    unit = "GJf/t-output",
    description = "Country specific energy consumption for the production of key chemicals from the IEA Information Paper Chemical and Petrochemical Sector 2009 "
  ))
}
