#' Read-in IEA The Future of Petrochemicals 2018 data from several figures 
#' (e.g., "Fig 4.1 Petrochem Production", "Fig A.1 Petrochem Prod Region", 
#' "Fig 4.5 Petrochem Feedstock", "Fig 4.9 Petro Prod Route RTS", 
#' "Fig 5.10 Petro Prod Route CTS") as a MagPIE object.
#'
#' @param subtype[1] Different data sheets to read. Available types are:
#'   \itemize{
#'     \item Feedstock: Fig 4.5 Petrochem Feedstock for HVCs, Ammonia, Methanol
#'     \item RouteRTS: Fig 4.9 Petro Prod Route RTS (Reference Technology Scenario) for HVCs, Ammonia, Methanol
#'     \item RouteCTS: Fig 5.10 Petro Prod Route CTS (Clean Technology Scenario) for HVCs, Ammonia, Methanol
#'     \item production3type: Fig 4.1 Petrochem Production for HVCs, Ammonia, Methanol
#'     \item production5type: Fig A.1 Petrochem Prod Region for Ethylene, Propylene, BTX, Ammonia, Methanol
#'   }
#'
#' @param subtype[2] Different products to read. Available types are:
#'   \itemize{
#'     \item HVCs
#'     \item Ammonia
#'     \item Methanol
#'     \item Ethylene
#'     \item Propylene
#'     \item BTX
#'   }
#'
#' @return MagPIE object of the IEA Petrochem data.
#' @author Qianzhi Zhang
#'
#' @export
calcIEA_Petrochem <- function(subtype) {
  x <- readSource("IEA_Petrochem", subtype = subtype)
  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Product",
    description = "Data on production volumes, feedstocks and production routes for different chemical products from IEA The Future of  Petrochemicals 2018"
  ))
}
