#' Calculate Total Chemical Energy consumption from IEA energy balances (Energy use + Feedstocks)
#'
#' @author Qianzhi Zhang
#'
#' @export
#' @importFrom dplyr inner_join
#'
calcChemicalTotal <- function() {
  # read in FE data from IEA and convert from ktoe to EJ
  IEA <- readSource("IEA", subtype = "EnergyBalances", convert = TRUE)
  Chem_FE <- IEA[, , "TOTAL.CHEMICAL"] * 4.1868e-5
  Chem_NE <- IEA[, , "TOTAL.NECHEM"] * 4.1868e-5
  Chem_TOT <- Chem_FE + Chem_NE

  return(list(
    x = Chem_TOT,
    weight = NULL,
    unit = "EJ",
    description = "Total Chemical Energy consumption (Energy use + Feedstocks) from IEA energy balances"
  ))
}
