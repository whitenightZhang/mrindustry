#'
#' @author Qianzhi Zhang
#'
#' @export
calcRMI_China <- function(subtype) {
  x <- readSource("RMI_China", subtype = subtype)
  return(list(
    x = x,
    weight = NULL,
    unit = "Mt Product & %",
    description = "Chemical (ammonia, methanol & ethylene) demand and share of different production routes datas in China 
    from RMI (Rocky Mountain Institute) Transforming China Chemicals Industry Pathways and Outlook under the Carbon Neutrality Goal 2022"
  ))
}
