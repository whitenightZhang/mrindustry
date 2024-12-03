#'
#' @author Qianzhi Zhang
#'
#' @export
calcChinaBaogao <- function() {.
  x <- readSource("ChinaBaogao") * 1e-2 # Convert to Mt
  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Production",
    description = "Methanol capicaity, Production and cunsumption datas in China from ChinaBaogao website"
  ))
}
