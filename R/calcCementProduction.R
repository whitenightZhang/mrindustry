#' @author Bennet Weiss
calcCementProduction <- function(x) {
  x <- readSource("Andrew2019", convert=TRUE)
  x[is.na(x)] <- 0
  output <- list(x = x, weight = NULL, unit = "tonne (t)", description = "Annual cement production")
  return(output)
}