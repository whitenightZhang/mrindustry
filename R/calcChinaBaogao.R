#' Retrieve Methanol capacity, Production and consumption data in China from ChinaBaogao 
#' (Chinese business website that publishes industry analyses.)
#' 
#' @author Qianzhi Zhang
#'
#' @export
calcChinaBaogao <- function() {
  x <- readSource("ChinaBaogao") * 1e-2 # Convert to Mt
  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Production",
    description = "Methanol capacity, Production and consumption data in China from ChinaBaogao website"
  ))
}
