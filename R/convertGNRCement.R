#' @author Bennet Weiss
#' @param subtype Currently only clinker_production is supported. Later, ...
#'
convertGNRCement <- function(x, subtype) {
  if (subtype == "clinker_production") {
    x <- x["World", , , invert = TRUE]
    getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
    x <- toolCountryFill(x, fill = NA, verbosity = 2)
    return(x)
  } else {
    stop("Invalid subtype. Valid subtypes are: clinker_production")
  }
}
