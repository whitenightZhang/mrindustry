#' Calculates global clinker ratio by employing cement and clinker production from Andrew's 2019 paper.
#' @author Bennet Weiss
#' @param x magclass object
calcCementProduction <- function(x) {
  clinker <- readSource("Andrew2019", subtype = "clinker")
  cement <- readSource("Andrew2019", subtype = "cement")
  # remove values from before 1900 to align time dimensions
  cement <- cement[,magclass::getYears(cement, as.integer = TRUE) >= 1900,]
  ratio <- clinker / cement

  x[is.na(x)] <- 0 # TODO implement smarter way to fill NA values
  unit <- "ratio"
  description <- paste(
    "Annual clinker-to-cement ratio as by cement and clinker production data from",
    "Andrew, R.M., 2019. Global CO2 emissions from cement production, 1928-2018.",
    "Earth System Science Data 11, 1675-1710. https://doi.org/10.5194/essd-11-1675-2019.",
    "Data reported on https://zenodo.org/records/11207133.",
    "Accessed: 24.02.2025."
  )
  output <- list(x = ratio, weight = NULL, unit = unit, description = description)
  return(output)
}
