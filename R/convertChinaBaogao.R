#' Convert ChinaBaogao
#'
#' Convert ChinaBaogao Methanol 2023.xlsx data to ISO country level.
#'
#' @param x MagPIE object containing ChinaBaogao Methanol data at regional resolution.
#' @return MagPIE object of the ChinaBaogao Methanol data disaggregated to country level.
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#'   a <- convertChinaBaogao(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset
convertChinaBaogao <- function(x) {
  # ---------------------------------------------------------------------------
  # Convert Country Names to ISO Codes
  #    - Replace region names in the spatial dimension (dim = 1) with their ISO equivalents.
  # ---------------------------------------------------------------------------
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  
  # ---------------------------------------------------------------------------
  # Fill Missing Country Data
  #    - Ensure that any missing entries in the country dimension are filled with 0.
  # ---------------------------------------------------------------------------
  x <- toolCountryFill(x, fill = 0)
  
  # ---------------------------------------------------------------------------
  # Return the Processed MagPIE Object
  # ---------------------------------------------------------------------------
  return(x)
}
