#' Calculate UNCTAD Plastic Trade Flows
#'
#' Read and aggregate UNCTAD plastic trade data (imports/exports) for multiple plastic categories
#' at regional or country level, with overrides for key economies.
#'
#' @param subtype Character; scenario to read. Options:
#'   \itemize{
#'     \item Final_Region         - Imports/exports of final plastics by region
#'     \item Final_Country        - Imports/exports of final plastics by country
#'     \item Waste_Region         - Plastic waste flows by region
#'     \item Waste_Country        - Plastic waste flows by country
#'     \item Primary_Region       - Imports/exports of primary plastics by region
#'     \item Primary_Country      - Imports/exports of primary plastics by country
#'     \item Intermediate_Region  - Imports/exports of intermediate forms of plastic by region
#'     \item Intermediate_Country - Imports/exports of intermediate forms of plastic by country
#'     \item Manufactured_Region  - Imports/exports of intermediate manufactured plastic goods by region
#'     \item Manufactured_Country - Imports/exports of intermediate manufactured plastic goods by country
#'   }
#'
#' @author Qianzhi Zhang
#' @export
calcUNCTAD_PlasticTrade <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Setup: file paths, mappings, and weights
  # ---------------------------------------------------------------------------
  trade_file     <- "C:/Users/leoniesc/madrat/sources/UNCTAD/US_PlasticsTradebyPartner.csv"
  map_file       <- "regionmappingH12.csv"
  recode_regions <- c(
    "European Union (2020 …)" = "EUR",
    "China"                    = "CHA",
    "United States of America" = "USA"
  )
  code_map <- c(
    "Lao People's Dem_ Rep_"        = "LAO",
    "Congo, Dem_ Rep_ of the"       = "COD",
    "Netherlands (Kingdom of the)" = "NLD",
    "Venezuela (Bolivarian Rep_ of)"= "VEN",
    "Switzerland, Liechtenstein"    = "CHE",
    "State of Palestine"            = "PSE"
  )
  raw_data <- read.csv(trade_file)
  map_df   <- toolGetMapping(map_file, type = "regional", where = "mrindustry")
  gdp_ssp2 <- calcOutput("GDP", scenario="SSP2",average2020 = FALSE, naming = "scenario", aggregate = FALSE)[,paste0("y", 2005:2022), "SSP2"]
  
  # ---------------------------------------------------------------------------
  # Helper: aggregate MagPIE object to country level
  # ---------------------------------------------------------------------------
  agg_to_country <- function(m) {
    toolAggregate(m, rel = map_df, dim = 1,
                  from = "RegionCode", to = "CountryCode",
                  weight = gdp_ssp2[unique(map_df$CountryCode), , ])
  }
  
  # ---------------------------------------------------------------------------
  # Helper: build region-level flows for given product and tag
  # ---------------------------------------------------------------------------
  build_region_flow <- function(prod_label, data2_tag) {
    # get data on country level and aggregate to region
    df_c <- raw_data %>%
      dplyr::filter(Product.Label == prod_label,
                    Partner.Label == "World") %>% # World is all trading partners aggregated
      tidyr::replace_na(list(Metric.tons.in.thousands = 0)) %>%
      dplyr::mutate(Year = as.integer(as.character(Year))) %>%
      dplyr::select(Country = Economy.Label, Year,
                    Data1 = Flow.Label, Value = Metric.tons.in.thousands)
    m_c <- as.magpie(df_c, spatial = 1, temporal = 2)
    getItems(m_c,1) <- toolCountry2isocode(getItems(m_c,1), mapping = code_map)
    m_c <- m_c[!is.na(getItems(m_c,1)), , ]
    m_c <- toolCountryFill(m_c, fill = 0); m_c[is.na(m_c)] <- 0
    m_r <- toolAggregate(m_c, rel = map_df, dim = 1,
                         from = "CountryCode", to = "RegionCode")
    m_r[is.na(m_r)] <- 0
    # get data directly on regional level and use this instead of aggregated data if available
    df_ov <- raw_data %>%
      dplyr::filter(Economy.Label %in% names(recode_regions),
                    Product.Label == prod_label,
                    Partner.Label == "World") %>%
      tidyr::replace_na(list(Metric.tons.in.thousands = 0)) %>%
      dplyr::mutate(Region = dplyr::recode(Economy.Label, !!!recode_regions),
                    Year   = as.integer(as.character(Year))) %>%
      dplyr::select(Region, Year, Data1 = Flow.Label, Value = Metric.tons.in.thousands)
    df_r <- as.data.frame(m_r) %>%
      dplyr::mutate(Year = as.integer(as.character(Year))) %>%
      dplyr::select(Region, Year, Data1, Value)
    df_f <- df_r %>%
      dplyr::left_join(df_ov, by = c("Region", "Year", "Data1"), suffix = c("", ".new")) %>%
      dplyr::mutate(Value = dplyr::if_else(!is.na(Value.new), Value.new, Value),
                    Data2 = data2_tag) %>%
      dplyr::select(Region, Year, Data2, Data1, Value)
    m_f <- as.magpie(df_f, spatial = 1, temporal = 2); m_f[is.na(m_f)] <- 0
    x <- agg_to_country(m_f)
    return(x / 1000) # thousand tons to Mt
  }
  
  # ---------------------------------------------------------------------------
  # Dispatch region-level subtypes
  # ---------------------------------------------------------------------------
  if (subtype == "Final_Region") {
    return(list(
      x           = build_region_flow("Final manufactured plastics goods", "Final Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Region-level imports/exports of final plastics"
    ))
  }
  if (subtype == "Primary_Region") {
    return(list(
      x           = build_region_flow("Plastics in primary forms", "Primary Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Region-level imports/exports of primary plastics"
    ))
  }
  if (subtype == "Intermediate_Region") {
    return(list(
      x           = build_region_flow("Intermediate forms of plastic", "Intermediate Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Region-level imports/exports of intermediate plastic forms"
    ))
  }
  if (subtype == "Manufactured_Region") {
    return(list(
      x           = build_region_flow("Intermediate manufactured plastic goods", "Manufactured Plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Region-level imports/exports of manufactured plastic goods"
    ))
  }
  
  # ---------------------------------------------------------------------------
  # Helper: build country-level flows for given product
  # ---------------------------------------------------------------------------
  build_country_flow <- function(prod_label) {
    df <- raw_data %>%
      dplyr::filter(Product.Label == prod_label,
                    Partner.Label == "World") %>%
      tidyr::replace_na(list(Metric.tons.in.thousands = 0)) %>%
      dplyr::mutate(Year = as.integer(as.character(Year))) %>%
      dplyr::select(Region = Economy.Label, Year,
                    Data1 = Flow.Label, Value = Metric.tons.in.thousands)
    m_x <- as.magpie(df, spatial = 1, temporal = 2)
    getItems(m_x,1) <- toolCountry2isocode(getItems(m_x,1), mapping = code_map)
    m_x <- m_x[!is.na(getItems(m_x,1)), , ]
    m_x <- toolCountryFill(m_x, fill = 0); m_x[is.na(m_x)] <- 0
    return(m_x / 1000) # thousand tons to Mt
  }
  
  # ---------------------------------------------------------------------------
  # Dispatch country-level subtypes
  # ---------------------------------------------------------------------------
  if (subtype == "Final_Country") {
    return(list(
      x           = build_country_flow("Final manufactured plastics goods"),
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level imports/exports of final plastics"
    ))
  }
  if (subtype == "Primary_Country") {
    return(list(
      x           = build_country_flow("Plastics in primary forms"),
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level imports/exports of primary plastics"
    ))
  }
  if (subtype == "Intermediate_Country") {
    return(list(
      x           = build_country_flow("Intermediate forms of plastic"),
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level imports/exports of intermediate plastic forms"
    ))
  }
  if (subtype == "Manufactured_Country") {
    return(list(
      x           = build_country_flow("Intermediate manufactured plastic goods"),
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level imports/exports of manufactured plastic goods"
    ))
  }
  
  # ---------------------------------------------------------------------------
  # Dispatch waste subtypes
  # ---------------------------------------------------------------------------
  if (subtype == "Waste_Region") {
    x <- build_region_flow("Plastic waste", "Plastic Waste")
    return(list(
      x           = x,
      weight      = NULL,
      unit        = "Mt",
      description = "Region-level plastic waste flows"
    ))
  }
  if (subtype == "Waste_Country") {
    df <- raw_data %>%
      dplyr::filter(Product.Label == "Plastic waste", Partner.Label == "World") %>%
      tidyr::replace_na(list(Metric.tons.in.thousands = 0)) %>%
      dplyr::mutate(Year = as.integer(as.character(Year))) %>%
      dplyr::select(Region = Economy.Label, Year,
                    Data1 = Flow.Label, Value = Metric.tons.in.thousands)
    m_x <- as.magpie(df, spatial = 1, temporal = 2)
    getItems(m_x,1) <- toolCountry2isocode(getItems(m_x,1), mapping = code_map)
    m_x <- m_x[!is.na(getItems(m_x,1)), , ]
    m_x <- toolCountryFill(m_x, fill = 0); m_x[is.na(m_x)] <- 0
    return(list(
      x           = m_x / 1000, # thousand tons to Mt
      weight      = NULL,
      unit        = "Mt",
      description = "Country-level plastic waste flows"
    ))
  }
  
  # ---------------------------------------------------------------------------
  # Error handling for unknown subtype
  # ---------------------------------------------------------------------------
  stop("Unknown subtype: ", subtype)
}
