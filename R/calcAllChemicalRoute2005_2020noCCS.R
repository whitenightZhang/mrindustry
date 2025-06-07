#'
#' @author Qianzhi Zhang
#'
#' @export
calcAllChemicalRoute2005_2020noCCS <- function() {

  AllChemicalRoute2005_2020 <- calcOutput("AllChemicalRoute2005_2020", warnNA = FALSE, aggregate = TRUE) %>%
    as.data.frame() %>%
    select(-Cell) %>%
    group_by(Region, Year) %>%
    mutate(
      Value = ifelse(Data1 == "amSyNG",
                     Value + sum(Value[Data1 == "amSyNG_cc"], na.rm = TRUE), 
                     Value),
      Value = ifelse(Data1 == "amSyNG_cc", 0, Value)
    ) %>%
    ungroup() 
  
  Chemcial_Total <- calcOutput("ChemicalTotal", aggregate = FALSE)%>%
    .[,c("y2005","y2010","y2015","y2020"),]
  
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry") 
  
  x <- as.magpie(AllChemicalRoute2005_2020, spatial = 1, temporal = 2)
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
