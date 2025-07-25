#' Filters chemical route flows from calcAllChemicalRoute2005_2020 for the year 2020
#' can be removed if calcAllChemicalRoute2005_2020 is read into REMIND
#' 
#' @author Qianzhi Zhang
#'
#' @export
#' @param CCS boolean parameter whether CCS technologies are considered as such in 2020 or assumed to be technologies without CCS
#' 
calcAllChemicalRoute2020 <- function(CCS=FALSE) {
  
  AllChemicalRoute2020<- calcOutput("AllChemicalRoute2005_2020",CCS=CCS,warnNA = FALSE, aggregate = TRUE)[,"y2020",]%>% 
    as.data.frame() %>%
    select(-Cell)
  
  Chemical_Total <- calcOutput("ChemicalTotal", aggregate = FALSE)%>%
    .[,c("y2020"),]
  
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry") 
  
  x <- as.magpie(AllChemicalRoute2020, spatial = 1, temporal = 2)
  # Aggregate the regional data (xReg) to the country level using the mapping
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode", weight = Chemical_Total[unique(map$CountryCode), , ])
  x[is.na(x)] <- 0
  
  return(list(
    x = x,
    weight = NULL,
    unit = "Gt Chemical Routes",
    description = "Chemical route flows filtered for the year 2020"
  ))
}
