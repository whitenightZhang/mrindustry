#' Calculates the lifetimes of residential and non-residential buildings, as well as of civil engeneering.
#' @author Bennet Weiss
#' @param x magclass object
calcBuildingLifetimes <- function(x, subtype) {
    # TODO check if i should do subtypes as additional variable of function
    res <- readSource("PostedLifetimes", convert = TRUE, subtype = "Res")

    # add missing regions/countries, calculate averages, etc...
}