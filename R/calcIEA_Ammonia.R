#'
#' @author Qianzhi Zhang
#'
#' @export
calcIEA_Ammonia <- function(subtype) {
  x <- readSource("IEA_Ammonia", subtype = subtype)
  return(list(
    x = x,
    weight = NULL,
    unit = "%",
    description = "Share on ammonia production routes in different regions from IEA Ammonia Technology Roadmap 2021"
  ))
}
