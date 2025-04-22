#' Calculates the lifetimes of residential and non-residential buildings, as well as of civil engeneering.
#' @author Bennet Weiss
#' @param x magclass object
calcBuildingLifetimes <- function(x) {
    res <- readSource("PostedLifetimes", convert = TRUE, subtype = "Res")
    nonres <- readSource("PostedLifetimes", convert = TRUE, subtype = "NonRes")
    civ <- readSource("PostedLifetimes", convert = TRUE, subtype = "Civ")

    # examples: 
    # 1. set nonres value from china to its res value
    # 2. set OAS to average of China, India and Japan

    # return the combination of res, nonres, civ.
}