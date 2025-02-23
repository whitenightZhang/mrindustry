#' @author Bennet Weiss
convertAndrew2019 <- function(x) {
    # remove unrecognized countries for now
    x <- toolCountryFill(x, fill=NaN, verbosity=2)
    return(x)
    
    # TODO take care of other data that does not have clear country code...
    # set countries that are unrecognized to NAN for now TODO correct that
    #countries <- getItems(x, dim=1)
    #is_iso3 <- countries %in% unique(countrycode::codelist$iso3c)
    # replace countries with ISO3 code where not already done
    # TODO find better way to deal with the countries that don't match..., e.g. add DDR to Germany before...
    #countries_fixed <- ifelse(
    #  is_iso3, 
    #  countries,
      # countrycode method works better than toolCountry2isocode for some reason
      # toolCountry2isocode sometimes replaces with XXX for no apparent reason
    #  countrycode::countrycode(countries, origin = "country.name", destination = "iso3c")  # Convert others
    #)
    # getItems(x, dim=1) <- countries_fixed
}