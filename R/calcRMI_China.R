#' Read-in RMI (Rocky Mountain Institute) "Transforming China Chemicals Industry Pathways and Outlook under the Carbon Neutrality Goal 2022.xlsx"
#' data from either "ES1-3 China Chemical Demand" or "ES29 China Chemical Structure" sheets as a magclass object.
#' Data contains chemical demand projections 2020-2050 for ammonia, methanol and ethylene and the feedstock structure for the production of ammonia, methanol and ethylene
#' in the zero-carbon scenario.
#' 
#' @param subtype[1] Type of RMI_China data sheet to read. Available types are:
#'   \itemize{
#'     \item ChemDemand: ES1-3 China Chemical Demand
#'     \item ChemStructure: ES10 China Chemical Structure
#'   }
#' @param subtype[2] The specific product from the RMI_China data to read. Available types are:
#'   \itemize{
#'     \item Ammonia
#'     \item Methanol
#'     \item Ethylene
#'   }
#' @author Qianzhi Zhang
#'
#' @export
calcRMI_China <- function(subtype) {
  x <- readSource("RMI_China", subtype = subtype)
  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Product or %",
    description = "Chemical (ammonia, methanol & ethylene) demand or share of different production routes datas in China 
    from RMI (Rocky Mountain Institute) Transforming China Chemicals Industry Pathways and Outlook under the Carbon Neutrality Goal 2022"
  ))
}
