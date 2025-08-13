#' Convert RMI_China
#'
#' Convert RMI (Rocky Mountain Institute) Transforming China’s Chemicals Industry Pathways and Outlook under the Carbon Neutrality Goal 2022.xlsx
#' "ES1-3 China Chemical Demand", "ES29 China Chemical Structure" data to ISO country level.
#'
#' @param x MagPIE object containing RMI_China data at regional resolution.
#' @return MagPIE object of the RMI_China data disaggregated to country level.
#' @author Qianzhi Zhang
#' @examples
#' \dontrun{
#'   a <- convertRMI_China(x)
#' }
#'
#' @importFrom magclass where
#' @importFrom magpiesets findset
convertRMI_China <- function(x) {
  # ---------------------------------------------------------------------------
  # Convert Country Names to ISO Codes
  #    - Replace region names in the spatial dimension (dim = 1) with ISO codes.
  # ---------------------------------------------------------------------------
  getItems(x, dim = 1) <- toolCountry2isocode(getItems(x, dim = 1))
  
  # ---------------------------------------------------------------------------
  # Fill Missing Country Data
  #    - Replace any missing entries in the country dimension with 0.
  # ---------------------------------------------------------------------------
  x <- toolCountryFill(x, fill = 0)
  
  # ---------------------------------------------------------------------------
  # Return the Processed MagPIE Object
  # ---------------------------------------------------------------------------
  return(x)
}
