#' Read-in MMSA Global Methanol Outlook 2023 Growth and Decarbonization data
#' containing regional methanol capacities and demands.
#' @author Qianzhi Zhang
#'
#' @export
calcMMSA_Methanol <- function() {
  x <- readSource("MMSA_Methanol")  %>% collapseDim() * 1e-3    # convert kt to Mt.
  return(list(
    x = x,
    weight = NULL,
    unit = "Mt",
    description = "Methanol production capacity and demand data in different regions from MMSA Global Methanol Outlook 2023 Growth and Decarbonization"
  ))
}
