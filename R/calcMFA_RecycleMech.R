#'
#' @author Qianzhi Zhang
#' 
#' @param subtype Different scenarios of Ammonia data that should be read. Available types are:
#'                \itemize{
#'                  \item export
#'                  \item import
#'                }
#' @export
calcMFA_RecycleMech<- function() {
  
  # 先把plastic_materials_para准备好
  plastic_materials_para <- data.frame(
    Material = c("ABS_ASA_SAN", "BioPlastics", "Fibres", "Elastomers_tyres",
                 "HDPE", "PET", "PP", "PS", "PUR", "LDPE", "PVC", "Others"),
    Feedstock = c("Aromatics_Olefins", "Biobased", "Aromatics_Olefins", "Olefins",
                  "Olefins", "Aromatics_Olefins", "Olefins", "Aromatics", "Aromatics_Olefins",
                  "Olefins", "Olefins", "Mixed"),
    Polymerization_Yield = c(0.85, NA, 0.95, 0.80,
                             0.98, 0.95, 0.98, 0.96, 0.85,
                             0.97, 0.95, 0.85),
    CH_Content_Ratio = c(0.95, 0.65, 0.80, 0.95,
                         1.00, 0.80, 1.00, 1.00, 0.85,
                         1.00, 0.43, 0.80)
  )
  
  # 加载PlasticRecycleMech
  PlasticRecycle <- read.csv("C:/Users/MiniTOP/Flodym/simson/recycling_by_region_year.csv", header = TRUE, sep = ",") %>%
    filter(
      Year >= 2005, Year <= 2100,
      ( (Year <= 2060 & (Year - 2005) %% 5 == 0) |
          (Year > 2060 & (Year - 2060) %% 10 == 0) )
    ) %>%
    select(Region, Year, Material, Use)  # 注意这里Material列需要存在！
  
  # 然后合并两张表
  PlasticRecycleMech_CH <- PlasticRecycleMech %>%
    left_join(plastic_materials_para, by = "Material") %>%
    mutate(
      CH_Mass = Use * Polymerization_Yield * CH_Content_Ratio
    ) %>%
    filter(
      Material %in% c("ABS_ASA_SAN", "HDPE", "PET", "PP", "LDPE")
    ) %>%
    group_by(Region, Year) %>%
    summarise(
      CH_Mass = sum(CH_Mass, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    select(Region, Year, CH_Mass)
  
  
  x <- as.magpie(PlasticRecycleMech_CH,
                 spatial = 1, temporal = 2)
  
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
    x = x_final/1000*0.79,
    weight = NULL,
    unit = "Gt HVC",
    description = "Aggregated chemical flow data recategorized into key sectors (ammonia, methanol, hvc, final outputs, and fertilizer) and aggregated to country level for 2020."
  ))
}

