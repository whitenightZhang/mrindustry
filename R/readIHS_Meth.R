#' Read IHS_Meth
#'
#' Read-in 9TH RUSSIA & CIS OIL & GAS EXECUTIVE SUMMIT 2019 METHANOL INS .xlsx file as
#' magclass object
#'
#' @param subtype[1] Type of Methanol data that should be read. Available types are:
#' \itemize{ \item Production
#' \item Capacity
#' \item Demand}
#' @param subtype[1] Type of Methanol data that should be read. Available types are:
#' \itemize{ \item 2010-2020: Interpolated 2018 data to 2010-2020 using data on total global methanol production
#' \item 2018}
#'  
#' @return magpie object of the IHS_Meth data
#' @author Qianzhi Zhang
#' @seealso [readSource()]
#' \dontrun{
#' a <- readSource(type = "IHS_Meth", subtype = "Production_2018")
#' }
#'
#' @importFrom readxl read_excel

readIHS_Meth <- function(subtype) {
  # Split the subtype string into components
  subtype <- unlist(strsplit(subtype, "_"))
  
  # Define parameters for reading the Excel file
  sheet_name <- "Methanol P&D Region METH INS"
  ranges <- c(Production = "B1:B13", Capacity = "C1:C13", Demand = "D1:D13")
  countrylist <- "A1:A13"
  
  # File name of the Excel file
  filename <- "9TH RUSSIA & CIS OIL & GAS EXECUTIVE SUMMIT 2019 METHANOL INS.xlsx"
  
  # Select the range based on the first component of the subtype
  range <- toolSubtypeSelect(subtype[1], ranges)
  
  # Read country data from the Excel file
  country_data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = countrylist, skip = 5))
  colnames(country_data) <- "Country"  # Rename the column to "Country"
  
  # Read the main data based on the subtype range
  data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = range, skip = 0))
  
  # Combine country data with the main data
  data <- cbind(country_data, data)
  
  # Read total capacity data for scaling ratios
  Total_capacity <- as.data.frame(read_excel(filename, sheet = "Methanol P&D Region IHS", range = "A1:L12", skip = 0))
  Total_capacity <- Total_capacity[Total_capacity$Region == "TOTAL", ]  # Filter rows where Region is "TOTAL"
  
  # Pivot total capacity data into a long format
  Total_capacity <- tidyr::pivot_longer(Total_capacity, names_to = "Year", cols = c(2:12))
  
  # Calculate scaling ratios based on the 2018 value
  Total_capacity <- Total_capacity %>%
    dplyr::group_by(Region) %>%
    dplyr::mutate(Ratio = value / value[Year == "2018"])
  
  # Expand 2018 data to include production data for the years 2010 to 2020
  data_2010_2020 <- data %>%
    tidyr::crossing(Year = Total_capacity$Year) %>%  # Generate all years for each country
    dplyr::left_join(Total_capacity, by = "Year") %>%  # Join with total capacity ratios
    dplyr::mutate(Adjusted_Value = .data[[subtype[1]]] * Ratio) %>%  # Scale data by ratio
    dplyr::select(Country, Year, Adjusted_Value)  # Keep only relevant columns
  
  # Convert data to a magpie object based on the subtype
  if (subtype[2] == "2010-2020") {
    # Convert expanded data for the years 2010 to 2020
    data <- as.magpie(data_2010_2020, spatial = 1, temporal = 2)
  } else if (subtype[2] == "2018") {
    # Add a "Year" column for 2018 and relocate it to the second position
    data <- dplyr::mutate(data, Year = "2018") %>%
      dplyr::relocate(Year, .after = 1)
    
    # Convert the data for 2018
    data <- as.magpie(data, spatial = 1, temporal = 2)
  }
  
  # Replace NA values with 0
  data[is.na(data)] <- 0
  
  # Add the subtype as a comment to the magpie object
  getComment(data) <- subtype
  
  # Return the processed magpie object
  return(data)
}

  
