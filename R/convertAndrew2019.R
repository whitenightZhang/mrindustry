#' Convert data from Andrew 2019
#' @author Bennet Weiss
#' @param x Magpie object
convertAndrew2019 <- function(x) {
    no_remove_warning <- c(
      "VDR", "East & West Pakistan", "Federation of Malaya-Singapore", "YMD", "FORMER YEMEN",
      "French Indo-China", "Japan (Excluding The Ruyuku Islands)", "ANT", "Netherland Antilles and Aruba",
      "Peninsular Malaysia", "Republic of South Vietnam", "Rhodesia-Nyasaland", "Rwanda-Urundi", "Sabah",
      "Sarawak", "SUDAN", "Tanganyika", "United Korea", "Zanzibar", # countries with NA entries
      "DDR", "DEW", # only one value in 1990
      "KNA___215", "KNA___216", "PCZ", "French Equatorial Africa", "Kuwaiti Oil Fires", "Leeward Islands", "Pacific Islands (Palau)", # zero anyways
      "French West Africa", "KSV", "Ryukyu Islands" # unsure
    )
    getItems(x, dim=1)[which(getItems(x, dim=1) == "YEMEN")] <- "YEM"
    getItems(x, dim=1)[which(getItems(x, dim=1) == "REPUBLIC OF SUDAN")] <- "SDN"
    x <- toolISOhistorical(x, overwrite = FALSE) # probably not needed
    x <- toolCountryFill(x, fill=NA, verbosity=1, no_remove_warning=no_remove_warning)
    # Puerto Rico is missing in dataset. Probably no problem.
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