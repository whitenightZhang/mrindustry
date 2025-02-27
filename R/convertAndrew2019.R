#' Convert data from Andrew 2019.
#' @author Bennet Weiss
#' @param x Magpie object
convertAndrew2019 <- function(x) {
  no_remove_warning <- c(
    "VDR", "East & West Pakistan", "Federation of Malaya-Singapore", "YMD", "FORMER YEMEN",
    "French Indo-China", "Japan (Excluding The Ruyuku Islands)", "ANT", "Netherland Antilles and Aruba",
    "Peninsular Malaysia", "Republic of South Vietnam", "Rhodesia-Nyasaland", "Rwanda-Urundi", "Sabah",
    "Sarawak", "SUDAN", "Tanganyika", "United Korea", "Zanzibar", # countries with NA entries
    "DDR", "DEW", # only one value in 1990
    "KNA___215", "KNA___216", "PCZ", "French Equatorial Africa", "Kuwaiti Oil Fires",
    "Leeward Islands", "Pacific Islands (Palau)", # zero anyways
    "French West Africa", "KSV", "Ryukyu Islands" # unsure
  )
  magclass::getItems(x, dim = 1)[which(getItems(x, dim = 1) == "YEMEN")] <- "YEM"
  magclass::getItems(x, dim = 1)[which(getItems(x, dim = 1) == "REPUBLIC OF SUDAN")] <- "SDN"
  x <- madrat::toolISOhistorical(x, overwrite = FALSE) # probably not needed
  x <- madrat::toolCountryFill(x, fill = NA, verbosity = 2, no_remove_warning = no_remove_warning)
  # Puerto Rico is missing in dataset. Probably no problem.
  return(x)
}
