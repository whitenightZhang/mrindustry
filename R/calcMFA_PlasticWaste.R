#'
#' @author Qianzhi Zhang
#' 
#' @param subtype Different scenarios of Ammonia data that should be read. Available types are:
#'                \itemize{
#'                  \item export
#'                  \item import
#'                }
#' @export
calcMFA_PlasticWaste<- function() {
  
  PlasticWaste <- read.csv("C:/Users/MiniTOP/Flodym/simson/recycling_by_region_year.csv", header = TRUE, sep = ",") %>%
    filter(
      Year >= 2005, Year <= 2100,
      ( (Year <= 2060 & (Year - 2005) %% 5 == 0) |
          (Year > 2060 & (Year - 2060) %% 10 == 0) )
    ) %>%
    select(Region, Year, Material, Use) %>%
    filter(
      !(Material %in% c("PVC", "Others", "PET", "PUR"))
    ) %>%
    group_by(Region, Year) %>%
    summarise(
      Use = sum(Use, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(Region, Year, Use)
  

  x <- as.magpie(PlasticWaste,
                 spatial = 1, temporal = 2)
  
  map <- toolGetMapping("regionmappingH12.csv", type = "regional", where = "mrindustry")
  
  #### 目前只考虑＞2020年的初值 ####
  
  x[, as.numeric(gsub("y", "", getYears(x))) > 2020, ] <- 
    x[, as.numeric(gsub("y", "", getYears(x))) > 2020, ] - 
    x[, as.numeric(gsub("y", "", getYears(x))) == 2020, ]
  
  #### 目前只考虑＞2020年的初值 ####
  
  # 先列出要提取的年份
  years_5 <- seq(2005, 2060, by = 5)  # 2005到2060每5年
  years_10 <- seq(2070, 2100, by = 10)  # 2070到2100每10年
  years_all <- c(years_5, years_10)  # 合并
  
  # 正式提取数据
  gdp_ssp2 <- calcOutput(
    "GDP",
    average2020 = FALSE,
    naming      = "scenario",
    aggregate   = FALSE
  )[, paste0("y", years_all), "SSP2"]
  
  
  x_final <- toolAggregate(
    x,
    rel    = map,
    dim    = 1,
    from   = "RegionCode",
    to     = "CountryCode",
    weight = gdp_ssp2[unique(map$CountryCode), , ]
  )
  
  return(list(
    x = x_final/1000,
    weight = NULL,
    unit = "Gt Plastic",
    description = "Aggregated chemical flow data recategorized into key sectors (ammonia, methanol, hvc, final outputs, and fertilizer) and aggregated to country level for 2020."
  ))
}

