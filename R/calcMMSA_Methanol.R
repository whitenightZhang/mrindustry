#'
#' @author Qianzhi Zhang
#'
#' @export
calcMMSA_Methanol <- function(subtype) {
  x <- readSource("MMSA_Methanol")  %>% collapseDim() * 1e-3    # convert kt to Mt.
  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Production",
    description = "Methanol production datas in different regions from MMSA Global Methanol Outlook 2023 Growth and Decarbonization"
  ))
}
