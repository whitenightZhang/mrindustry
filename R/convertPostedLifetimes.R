#' convert data from PostedLifetimes
#' @author Bennet Weiss
#' @param x Magpie object
#' @param subtype End-use type of construction. Can be either "Res", "NonRes" or "Civ".
convertPostedLifetimes <- function(x) {
    # convert country name to iso3 if applicable, other region names stay untouched
    regions <- magclass::getItems(x, dim = 1)
    isocodes <- madrat::toolCountry2isocode(regions)
    magclass::getItems(x, dim = 1)[!is.na(isocodes)] <- isocodes[!is.na(isocodes)]
    x_out <- madrat::toolCountryFill(x, fill = NA, verbosity = 2)

    # fill values by region
    # regionmapping also found in rd3mod/inputdata/mappings/regional
    regionmapping <- toolGetMapping("regionmapping_21_EU11.csv", where = "mrindustry", type = "regional")
    
    # fill all EUR regions with Europe value
    eur_regions <- unique(regionmapping$RegionCode[regionmapping$missingH12 == "EUR"])
    eur_mask <- magclass::getItems(x_out, dim = 1) %in% regionmapping$CountryCode[regionmapping$RegionCode %in% eur_regions]
    eur_countries <- magclass::getItems(x_out, dim = 1)[eur_mask]
    x_out[eur_countries,,] <- x["Europe",,]

    # fill all LAM regions with Rest of South America value
    lam_mask <- magclass::getItems(x_out, dim = 1) %in% regionmapping$CountryCode[regionmapping$RegionCode == "LAM"]
    lam_countries <- magclass::getItems(x_out, dim = 1)[lam_mask]
    x_out[lam_countries,,] <- x["Rest of South America",,]
    
    # TODO: set China data to be constant in beginning and end of time period

    return(x_out)
}