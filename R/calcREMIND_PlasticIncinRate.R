#'
#' @author Qianzhi Zhang
#'
#' @export
calcREMIND_PlasticIncinRate <- function() {
  REMIND_PlasticIncinRate <- calcOutput("OECD_PlasticIncinRate") %>%
    as.data.frame() %>%
    mutate(
      Year = as.integer(as.character(Year))
    ) %>%
    filter(
      Year >= 2005, Year <= 2100,
      ( (Year <= 2060 & (Year - 2005) %% 5 == 0) |
          (Year > 2060 & (Year - 2060) %% 10 == 0) )
    ) %>%
    filter(Data1 == "PVC") %>%
    select(Region, Year, Value)
  
  
  x <- as.magpie(REMIND_PlasticIncinRate,spatial = 1,temporal = 2)
  
  file_name <- "regionmappingH12.csv"
  map <- toolGetMapping(file_name, type = "regional", where = "mrindustry")
  
  x <- toolAggregate(x, rel = map, dim = 1, from = "RegionCode", to = "CountryCode") 
  
  weight <- x # get the same dimensions of the data
  weight[, , ] <- 1
  return(list(
    x = x,
    weight = weight,
    unit = "% Plastic incineration",
    description = "Aggregated chemical flow data recategorized into key sectors (ammonia, methanol, hvc, final outputs, and fertilizer) and aggregated to country level for 2020."
  ))
}

