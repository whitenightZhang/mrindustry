#' Convert RMI_China
#'
#' Convert RMI (Rocky Mountain Institute) Transforming China’s Chemicals Industry Pathways and Outlook under the Carbon Neutrality Goal 2022.xlsx
#' "ES1-3 China Chemical Demand", "ES29 China Chemical Structure" data to ISO country level.
#'
#'
#' @param x MAgPIE object containing RMI_China data region resolution
#' @return MAgPIE object of the RMI_China data disaggregated to country level
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#' a <- convertRMI_China(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset

convertRMI_China <- function(x) {
  # Convert country names in the spatial dimension (dim = 1) to ISO codes
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))

  # Fill missing countries with 0 for consistency
  x <- toolCountryFill(x, fill = 0)

  # Return the processed magpie object
  return(x)
}
