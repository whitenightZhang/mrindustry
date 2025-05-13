#' Calculate Clinker-to-Cement Ratio
#'
#' @md
#' @return A list with a [`magpie`][magclass::magclass] object `x`, `weight`,
#'   `unit`, and `description`.
#'
#' @author Michaja Pehl
#' @seealso [`readADVANCE_WP2()`], [`convertADVANCE_WP2()`]
#' @export
calcClinkerToCementRatio <- function() {

  x <- readSource("ADVANCE_WP2", "clinker-to-cement-ratio")
  getItems(x, dim = 2) <- 2005
  getNames(x) <- NULL

  list(x = x,
       weight = calcOutput("GDPPast", aggregate = FALSE, years = 2015),
       unit = "ratio",
       description = "clinker-to-cement ratio")
}
