#' Read PlasticsEoL
#'
#' Read-in data for the End-of-Life fate of plastics
#' from 1.Stegmann, P., Daioglou, V., Londo, M., van Vuuren,
#' D. P. & Junginger, M. Plastic futures and their CO2 emissions.
#' Nature 612, 272–276 (2022).
#' https://www.nature.com/articles/s41586-022-05422-5
#' Link to SI:
#' https://static-content.springer.com/esm/art%3A10.1038%2Fs41586-022-05422-5/MediaObjects/41586_2022_5422_MOESM1_ESM.xlsx #nolint
#'
#' @md
#' @return magpie object of the data
#' @author Falk Benke, Simón Moreno
#' @seealso \code{\link{readSource}}
#' @examples
#' \dontrun{
#' a <- readSource(type = "Stegmann2022")
#' }
#'
readStegmann2022 <- function() {
  readxl::read_xlsx("41586_2022_5422_MOESM1_ESM.xlsx", sheet = "Data") %>%
    tidyr::pivot_longer(tidyselect::starts_with("2"), names_to = "period") %>%
    as.magpie(spatial = "Region", temporal = "period", tidy = TRUE)
}
