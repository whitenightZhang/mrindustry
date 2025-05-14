#' Get region mask that selects countries of a region based on a region mapping.
#' 
#' @author Bennet Weiss
#' @param x Magpie object
#' @param region string or vector of strings to identify the region
#' @param regionmapping mapping of regions to countries
toolGetRegionMask <- function(x, region, regionmapping) {
  mask <- magclass::getItems(x, dim = 1) %in% regionmapping$CountryCode[regionmapping$RegionCode %in% region]
  return(mask)
}

#' Retrieve countries of a region from a region mapping.
#' 
#' @author Bennet Weiss
#' @param x Magpie object
#' @param region string or vector of strings to identify the region
#' @param regionmapping mapping of regions to countries
toolGetRegionCountries <- function(x, region, regionmapping) {
  regionmask <- toolGetRegionMask(x, region, regionmapping)
  countries <- magclass::getItems(x, dim = 1)[regionmask]
  return(countries)
}
