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
    unit = "%",
    description = "Share on ammonia production routes in different regions from IEA Ammonia Technology Roadmap 2021"
  ))
}
