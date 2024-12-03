#' Convert ChinaBaogao
#'
#' Convert ChinaBaogao Methanol 2023.xlsx data to ISO country level.
#'
#' @param x MAgPIE object containing ChinaBaogao Methanol region resolution
#' @return MAgPIE object of the ChinaBaogao Methanol data disaggregated to country level
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#' a <- convertChinaBaogao(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset

convertChinaBaogao <- function(x) {
  # Convert country names in the spatial dimension (dim = 1) to ISO codes
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  
  # Fill missing country data with 0 for consistency
  x <- toolCountryFill(x, fill = 0)
  
  # Return the processed magpie object
  return(x)
}

