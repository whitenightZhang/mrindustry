readMMSA_Methanol <- function() {
  # ---------------------------------------------------------------------------
  # 1. Define Filename and Read Regional Data
  #    - Set the filename for the MMSA Global Methanol Outlook 2023 file.
  #    - Read regional data (e.g., Methanol Supply and Demand) from the specified sheet and range.
  # ---------------------------------------------------------------------------
  filename <- "MMSA Global Methanol Outlook 2023 Growth and Decarbonization.xlsx"
  data_4region <- read_excel(filename, sheet = "Page 7-11 Methanol S&D ", range = "A1:M13", skip = 0)
  
  # ---------------------------------------------------------------------------
  # 2. Process Production Capacity Data
  #    - Read the Production Capacity data from a separate sheet.
  #    - Rename the first column to "Region", add a "Type" column, and relocate it.
  #    - Exclude specified regions (Europe, North America, Middle East).
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
  # 3. Isolate China from Asia Data
  #    - Identify rows corresponding to "Asia" in production capacity data.
  #    - Identify rows for "Asia (Less China)" from the regional data (data_4region).
  #    - For numeric columns, subtract "Asia (Less China)" values from "Asia" to isolate China.
  #    - Rename "Asia" to "China".
  # ---------------------------------------------------------------------------
  rows_capacity <- data_capacity_add$Region == "Asia"
  rows_4region <- data_4region$Region == "Asia (Less China)" & data_4region$Type == "Production capacity"
  numeric_cols <- sapply(data_capacity_add, is.numeric)
  
  data_capacity_add[rows_capacity, numeric_cols] <-
    data_capacity_add[rows_capacity, numeric_cols] -
    data_4region[rows_4region, numeric_cols]
  
  data_capacity_add$Region[data_capacity_add$Region == "Asia"] <- "China"
  
  # ---------------------------------------------------------------------------
  # 4. Process Total Demand Data
  #    - Read Total Demand data from the same sheet as capacity.
  #    - Rename the first column to "Region", add a "Type" column ("Total Demand"),
  #      relocate the "Type" column, and filter out specified regions.
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
  # 5. Prepare Regional Data for Combination
  #    - Remove rows with "Production" from data_4region.
  # ---------------------------------------------------------------------------
  data_4region <- data_4region[data_4region$Type != "Production", ]
  
  # ---------------------------------------------------------------------------
  # 6. Combine Datasets
  #    - Combine the processed regional data, capacity data, and demand data.
  # ---------------------------------------------------------------------------
  data <- rbind(data_4region, data_capacity_add, data_demand_add)
  
  # ---------------------------------------------------------------------------
  # 7. Reshape Data to Long Format
  #    - Pivot the data from wide to long format, with "Year" as the variable.
  # ---------------------------------------------------------------------------
  data <- tidyr::pivot_longer(data, names_to = "Year", cols = c(3:13))
  
  # ---------------------------------------------------------------------------
  # 8. Convert to MagPIE Object and Finalize
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
