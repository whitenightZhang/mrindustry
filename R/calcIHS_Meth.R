#' Retrieve methanol production, capacity and demand data for 2018 from 
#' 9TH RUSSIA & CIS OIL & GAS EXECUTIVE SUMMIT 2019 METHANOL,
#' optionally the 2018 data is interpolated to 2010-2020 based on 
#' temporal coverage of Methanol capacities 2010-2020 from IHS Markit 
#'
#' @param subtype[1] Type of Methanol data to read. Available types are:
#'                \itemize{
#'                  \item Production
#'                  \item Capacity
#'                  \item Demand
#'                }
#' @param subtype[2] Temporal coverage of Methanol data. Available types are:
#'                \itemize{
#'                  \item 2010-2020: Interpolated 2018 data to 2010-2020 using total global methanol production data.
#'                  \item 2018: Data for the year 2018.
#'                }
#'
#' @return Magpie object of the IHS_Meth data.
#' @author Qianzhi Zhang
#'
#' @export
calcIHS_Meth <- function(subtype) {
  x <- readSource("IHS_Meth", subtype = subtype)
  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Product",
    description = "Methanol production/capacity/demand datas in different regions from 9TH RUSSIA & CIS OIL & GAS EXECUTIVE SUMMIT 2019 METHANOL INS"
  ))
}
