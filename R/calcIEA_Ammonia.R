#' Read-in IEA Ammonia Technology Roadmap 2021 Fig 2.9 Ammonia production by process route and
#' scenario in major ammonia producing regions data as a magclass object.
#'
#' @param subtype Different scenarios of Ammonia data that should be read. Available types are:
#'                \itemize{
#'                  \item BaseYear_2020: Base year data in 2020
#'                  \item STEPS_2050: IEA STEPS scenario in 2050
#'                  \item SDS_2050: IEA SDS scenario in 2050
#'                }
#'
#' @author Qianzhi Zhang
#'
#' @export
calcIEA_Ammonia <- function(subtype) {
  x <- readSource("IEA_Ammonia", subtype = subtype)
  
  return(list(
    x = x,
    weight = NULL,
    unit = "%",
    description = "Share on ammonia production routes in different regions from IEA Ammonia Technology Roadmap 2021"
  ))
}
