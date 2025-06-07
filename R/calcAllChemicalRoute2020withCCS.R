#'
#' @author Qianzhi Zhang
#'
#' @export
calcAllChemicalRoute2020withCCS <- function() {
  
  AllChemicalRoute2020<- calcOutput("AllChemicalRoute2005_2020",warnNA = FALSE, aggregate = TRUE)[,"y2020",]%>% 
    as.data.frame() %>%
    select(-Cell)
  
  Chemcial_Total <- calcOutput("ChemicalTotal", aggregate = FALSE)%>%
    .[,c("y2020"),]
  
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry") 
  
  x <- as.magpie(AllChemicalRoute2020, spatial = 1, temporal = 2)
  # Aggregate the regional data (xReg) to the country level using the mapping
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode", weight = Chemcial_Total[unique(map$CountryCode), , ])
  x[is.na(x)] <- 0
  
  return(list(
    x = x,
    weight = NULL,
    unit = "TODO ???",
    description = "TODO ???"
  ))
}
