#' Read MMSA_Methanol
#'
#' Read-in MMSA(Methanol Market Services Asia) Global Methanol Outlook 2023 Growth and Decarbonization.xlsx file as
#' magclass object
#'
#' @return magpie object of the MMSA_Methanol data
#' @author Qianzhi Zhang
#' @seealso [readSource()]
#' \dontrun{
#' a <- readSource(type = "MMSA_Methanol")
#' }
#'
#' @importFrom readxl read_excel tidyr pivot_longer pivot_wider

readMMSA_Methanol <- function() {
  # Define the filename for the MMSA Methanol Outlook file
  filename <- "MMSA Global Methanol Outlook 2023 Growth and Decarbonization.xlsx"
  
  # Read regional data for Methanol Supply and Demand
  data_4region <- read_excel(filename, sheet = "Page 7-11 Methanol S&D ", range = "A1:M13", skip = 0)
  
  # Process Production Capacity data
  data_capacity_add <- read_excel(
    filename,
    sheet = "Page 5-6 Methanol S&D region",
    range = "A1:L7",
    skip = 0
  ) %>%
    dplyr::rename(Region = 1) %>%  # Rename the first column to "Region"
    dplyr::mutate(Type = "Production capacity") %>%  # Add a "Type" column with "Production capacity"
    dplyr::relocate(Type, .after = 1) %>%  # Move "Type" column to the second position
    dplyr::filter(!Region %in% c("Europe", "North America", "Middle East"))  # Exclude specified regions
  
  # Identify rows for Asia in data_capacity_add
  rows_capacity <- data_capacity_add$Region == "Asia"
  
  # Identify rows for Asia (Less China) in data_4region
  rows_4region <- data_4region$Region == "Asia (Less China)" & data_4region$Type == "Production capacity"
  
  # Select numeric columns only (exclude non-numeric columns)
  numeric_cols <- sapply(data_capacity_add, is.numeric)
  
  # Subtract Asia (Less China) data from Asia data to isolate China
  data_capacity_add[rows_capacity, numeric_cols] <- 
    data_capacity_add[rows_capacity, numeric_cols] - 
    data_4region[rows_4region, numeric_cols]
  
  # Rename Asia to China
  data_capacity_add$Region[data_capacity_add$Region == "Asia"] <- "China"
  
  # Process Total Demand data
  data_demand_add <- read_excel(
    filename,
    sheet = "Page 5-6 Methanol S&D region",
    range = "A8:L14",
    skip = 0
  ) %>%
    dplyr::rename(Region = 1) %>%  # Rename the first column to "Region"
    dplyr::mutate(Type = "Total Demand") %>%  # Add a "Type" column with "Total Demand"
    dplyr::relocate(Type, .after = 1) %>%  # Move "Type" column to the second position
    dplyr::filter(!Region %in% c("Europe", "North America", "Middle East", "Asia"))  # Exclude specified regions
  
  # Exclude rows with "Production" from data_4region
  data_4region <- data_4region[data_4region$Type != "Production", ]
  
  # Combine data_4region, data_capacity_add, and data_demand_add
  data <- rbind(data_4region, data_capacity_add, data_demand_add)
  
  # Transform data from wide to long format
  data <- tidyr::pivot_longer(data, names_to = "Year", cols = c(3:13))
  
  # Convert the data to a magpie object for further processing
  data <- as.magpie(data, spatial = 1, temporal = 3)
  
  # Replace NA values with 0
  data[is.na(data)] <- 0
  
  # Return the processed magpie object
  return(data)
}

