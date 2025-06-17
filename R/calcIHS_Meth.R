#'
#' @author Qianzhi Zhang
#'
#' @export
calcIHS_Meth <- function(subtype) {
  x <- readSource("IHS_Meth", subtype = subtype)
  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Product",
    description = "Methanol production datas in different regions from 9TH RUSSIA & CIS OIL & GAS EXECUTIVE SUMMIT 2019 METHANOL INS"
  ))
}
