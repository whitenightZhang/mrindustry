#' Read MMSA_Methanol
#'
#' Read-in MMSA Global Methanol Outlook 2023 Growth and Decarbonization.xlsx sheets as a magclass object.
#' Data contains regional methanol capacities and demands. The data is merged from different figures covering different regions.
readMMSA_Methanol <- function() {
  # ---------------------------------------------------------------------------
  # Read Regional Demand, Production and Production Capacity Data for Europe, Asia(Less China), North America and Middle East
  # ---------------------------------------------------------------------------
  filename <- "MMSA Global Methanol Outlook 2023 Growth and Decarbonization.xlsx"
  data_4region <- read_excel(filename, sheet = "Page 7-11 Methanol S&D ", range = "A1:M13", skip = 0)
  
  # ---------------------------------------------------------------------------
  # Read Capacity Data for a different set of regions: Europe, Asia, North America, South America, Middle East and Rest of World
  #   - remove regions that are already included in the first set: Europe, North Ameria and Middle East
  # ---------------------------------------------------------------------------
  data_capacity_add <- read_excel(
    filename,
    sheet = "Page 5-6 Methanol S&D region",
    range = "A1:L7",
    skip = 0
  ) %>%
    dplyr::rename("Region" = 1) %>%
    dplyr::mutate(Type = "Production capacity") %>%
    dplyr::relocate(.data$Type, .after = 1) %>%
    dplyr::filter(!.data$Region %in% c("Europe", "North America", "Middle East"))
  
  # ---------------------------------------------------------------------------
  # Get China Production capacity by subtracting "Asia (Less China)" values of the first dataset from "Asia" of the second dataset
  # ---------------------------------------------------------------------------
  rows_capacity <- data_capacity_add$Region == "Asia"
  rows_4region <- data_4region$Region == "Asia (Less China)" & data_4region$Type == "Production capacity"
  numeric_cols <- sapply(data_capacity_add, is.numeric)
  
  data_capacity_add[rows_capacity, numeric_cols] <-
    data_capacity_add[rows_capacity, numeric_cols] -
    data_4region[rows_4region, numeric_cols]
  
  data_capacity_add$Region[data_capacity_add$Region == "Asia"] <- "China"
  
  # ---------------------------------------------------------------------------
  # Read Demand Data for a different set of regions and again remove regions that are already included in the first set
  #   - this time, there is already specific demand data for China, so there is no need to calculate this from Asia
  # ---------------------------------------------------------------------------
  data_demand_add <- read_excel(
    filename,
    sheet = "Page 5-6 Methanol S&D region",
    range = "A8:L14",
    skip = 0
  ) %>%
    dplyr::rename("Region" = 1) %>%
    dplyr::mutate(Type = "Total Demand") %>%
    dplyr::relocate(.data$Type, .after = 1) %>%
    dplyr::filter(!.data$Region %in% c("Europe", "North America", "Middle East", "Asia"))
  
  # ---------------------------------------------------------------------------
  # Remove rows with "Production" from data_4region, as we are only interested in Production Capacity and Demand.
  # ---------------------------------------------------------------------------
  data_4region <- data_4region[data_4region$Type != "Production", ]
  
  # ---------------------------------------------------------------------------
  # Combine the two Datasets from the two different sets of regions
  # ---------------------------------------------------------------------------
  data <- rbind(data_4region, data_capacity_add, data_demand_add)
  
  # ---------------------------------------------------------------------------
  # Reshape Data to Long Format
  #    - Pivot the data from wide to long format, with "Year" as the variable.
  # ---------------------------------------------------------------------------
  data <- tidyr::pivot_longer(data, names_to = "Year", cols = c(3:13))
  
  # ---------------------------------------------------------------------------
  # Convert to MagPIE Object and Finalize
  #    - Convert the reshaped data into a magpie object with appropriate spatial and temporal dimensions.
  #    - Replace NA values with 0.
  # ---------------------------------------------------------------------------
  data <- as.magpie(data, spatial = 1, temporal = 3)
  data[is.na(data)] <- 0
  
  # ---------------------------------------------------------------------------
  # 9. Return the Processed MagPIE Object
  # ---------------------------------------------------------------------------
  return(data)
}
