#' Convert data from PostedBuiltLifespan. Lifetimes are assigned to region based on regionmapping.
#' @author Bennet Weiss
#' @param x Magpie object
convertPostedBuiltLifespan <- function(x) {
  # convert country name to iso3 if applicable, other region names stay untouched
  regions <- magclass::getItems(x, dim = 1)
  no_remove_warning <- c("Africa", "CIS", "Europe", "Rest of South America")
  isocodes <- madrat::toolCountry2isocode(regions, ignoreCountries = no_remove_warning)
  magclass::getItems(x, dim = 1)[!is.na(isocodes)] <- isocodes[!is.na(isocodes)]
  x_out <- madrat::toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = no_remove_warning)

  # fill values by region
  # regionmapping also found in rd3mod/inputdata/mappings/regional TODO: rausehmen
  regionmapping <- toolGetMapping("regionmapping_21_EU11.csv", where = "mappingfolder", type = "regional")

  # EUR
  # fill all countries with European average
  eur_regions <- unique(regionmapping$RegionCode[regionmapping$missingH12 == "EUR"])
  eur_countries <- getRegionCountries(x_out, eur_regions, regionmapping)
  x_out[eur_countries, , ] <- x["Europe", , ]

  # NEU
  # EUR values
  neu_regions <- unique(regionmapping$RegionCode[regionmapping$missingH12 == "NEU"])
  neu_countries <- getRegionCountries(x_out, neu_regions, regionmapping)
  x_out[neu_countries, , ] <- x["Europe", , ]

  # LAM
  # "Rest of South America" Residential values
  lam_countries <- getRegionCountries(x_out, "LAM", regionmapping)
  x_out[lam_countries, , ] <- x["Rest of South America", , "Res"]

  # CHN
  # fill all years before 1950 with 1950 value and all years after 2015 with 2015 value
  # fill non-residential with residential values (following Cao 2019)
  first_non_na <- x_out["CHN", 1950, "Res"]
  last_non_na <- x_out["CHN", 2015, "Res"]
  x_out["CHN", , "Res"][magclass::getYears(x_out, as.integer = TRUE) < 1950] <- first_non_na
  x_out["CHN", , "Res"][magclass::getYears(x_out, as.integer = TRUE) > 2015] <- last_non_na
  x_out["CHN", , "NonRes"] <- x_out["CHN", , "Res"]

  # CHA
  # copy CHN values
  cha_countries <- getRegionCountries(x_out, "CHA", regionmapping)
  x_out[cha_countries, , ] <- x_out["CHN", , ]

  # OAS
  # average of China, India and Japan
  oas_countries <- getRegionCountries(x_out, "OAS", regionmapping)
  x_out[oas_countries, , ] <- (x_out["CHN", , ] + x_out["IND", , ] + x_out["JPN", , ]) / 3

  # CAZ
  # average of EU and USA
  caz_countries <- getRegionCountries(x_out, "CAZ", regionmapping)
  x_out[caz_countries, , ] <- (x["Europe", , ] + x_out["USA", , ]) / 2

  # MEA
  # values from Iran
  mea_countries <- getRegionCountries(x_out, "MEA", regionmapping)
  x_out[mea_countries, , ] <- x_out["IRN", , ]

  # SSA
  # values from Africa
  ssa_countries <- getRegionCountries(x_out, "SSA", regionmapping)
  x_out[ssa_countries, , ] <- x["Africa", , ]

  # REF
  # values from CIS
  ref_countries <- getRegionCountries(x_out, "REF", regionmapping)
  x_out[ref_countries, , ] <- x["CIS", , ]

  # IND, JPN, USA are fine by default

  return(x_out)
}

#' @author Bennet Weiss
#' @param x Magpie object
#' @param region string or vector of strings to identify the region
#' @param regionmapping mapping of regions to countries
getRegionMask <- function(x, region, regionmapping) {
  mask <- magclass::getItems(x, dim = 1) %in% regionmapping$CountryCode[regionmapping$RegionCode %in% region]
  return(mask)
}

#' @author Bennet Weiss
#' @param x Magpie object
#' @param region string or vector of strings to identify the region
#' @param regionmapping mapping of regions to countries
getRegionCountries <- function(x, region, regionmapping) {
  regionmask <- getRegionMask(x, region, regionmapping)
  countries <- magclass::getItems(x, dim = 1)[regionmask]
  return(countries)
}
