#' Calculate Clinker-to-Cement Ratio
#'
#' @md
#' @return A list with a [`magpie`][magclass::magclass] object `x`, `weight`,
#'   `unit`, and `description`.
#'
#' @importFrom magclass getYears<-
#' @author Michaja Pehl
#' @seealso [`calcOutput()`], [`readADVANCE_WP2()`], [`convertADVANCE_WP2()`]
#' @export
calcClinker_to_cement_ratio <- function() {
  list(x = `getYears<-`(readSource("ADVANCE_WP2", "clinker-to-cement-ratio"), 2005),
       weight = calcOutput("GDPPast", aggregate = FALSE, years = 2015),
       unit = "ratio",
       description = "clinker-to-cement ratio")
}
