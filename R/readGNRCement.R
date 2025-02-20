#' Read GNRCement data from GCCA
#' @author Bennet Weiss
#' @param subtype Currently only clinker_production is supported. Later, ...
#'
readGNRCement <- function(subtype) {
  if (subtype == "clinker_production") {
    data1 <- readxl::read_xlsx(file.path("v1", "Postdam_GCCA_GNR_2022.xlsx"), sheet = "World", range = "A10:C30")
    data2 <- readxl::read_xlsx(file.path("v1", "Postdam_GCCA_GNR_2022.xlsx"), sheet = "Austria", , range = "A10:C30")
    data <- rbind(data1, data2)
    x <- as.magpie(data, spatial = 1)
    return(x)
  } else {
    stop("Invalid subtype. Valid subtypes are: clinker_production")
  }
}
