#' Read Data from Andrew's 2019 paper
#' https://zenodo.org/records/11207133
#' Last version update 17.05.2024
#' Dataset update from:
#' Andrew, R.M., 2019. Global CO2 emissions from cement production, 1928–2018.
#' Earth System Science Data 11, 1675–1710. https://doi.org/10.5194/essd-11-1675-2019.
#' @author Bennet Weiss
readAndrew2019 <- function() {
  path <- file.path("v1", "1. annual_cement_production.csv")
  data <- readr::read_csv(path)
  # clean up data such that the country row becomes a column, too
  data_extracted <- tidyr::pivot_longer(data, -"Year", names_to = "region", values_to = "Value")
  x <- magclass::as.magpie(data_extracted, spatial = 2)
  return(x)
}
