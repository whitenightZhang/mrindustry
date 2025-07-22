#' Read IHS_Meth
#'
#' Read-in 9TH RUSSIA & CIS OIL & GAS EXECUTIVE SUMMIT 2019 METHANOL INS.xlsx file as a magclass object.
#' Regional data on Methanol Production, Capacity and Demand from 2018 is combined with temporal coverage of Methanol capacities 2010-2020
#' from IHS Markit to scale the 2018 data in order to estimate production, capacity and demand for 2010-2020.
#'
#' @param subtype[1] Type of Methanol data to read. Available types are:
#'                \itemize{
#'                  \item Production
#'                  \item Capacity
#'                  \item Demand
#'                }
#' @param subtype[2] Temporal coverage of Methanol data. Available types are:
#'                \itemize{
#'                  \item 2010-2020: Interpolated 2018 data to 2010-2020 using total global methanol production data.
#'                  \item 2018: Data for the year 2018.
#'                }
#'
#' @return Magpie object of the IHS_Meth data.
#' @author Qianzhi Zhang
#' @seealso [readSource()]
#' \dontrun{
#'   a <- readSource(type = "IHS_Meth", subtype = "Production_2018")
#' }
#'
#' @importFrom readxl read_excel
readIHS_Meth <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Parse the Input Subtype
  #    - Split the input string into components.
  # ---------------------------------------------------------------------------
  subtype <- unlist(strsplit(subtype, "_"))
  
  # ---------------------------------------------------------------------------
  # Define File Parameters and Read Country Data
  #    - Set the file name, sheet name, and cell ranges for production, capacity, or demand.
  #    - Read the list of countries from the specified range.
  # ---------------------------------------------------------------------------
  filename <- "9TH RUSSIA & CIS OIL & GAS EXECUTIVE SUMMIT 2019 METHANOL INS.xlsx"
  sheet_name <- "Methanol P&D Region METH INS"
  ranges <- c(Production = "B1:B13", Capacity = "C1:C13", Demand = "D1:D13")
  countrylist <- "A1:A13"
  
  # Select the range based on the first component of subtype (e.g., "Production")
  range <- toolSubtypeSelect(subtype[1], ranges)

  # Read country names and rename the column to "Country"
  country_data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = countrylist))
  colnames(country_data) <- "Country"
  
  # ---------------------------------------------------------------------------
  # Read Main Data and Combine with Country Data
  #    - Read the main data from the selected range.
  #    - Combine the country names with the main data.
  # ---------------------------------------------------------------------------
  data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = range))
  data <- cbind(country_data, data)
  
  # ---------------------------------------------------------------------------
  # Read and Process Total Capacity Data for Scaling
  #    - Read the total capacity data from a separate sheet.
  #    - Filter for the row where Region equals "TOTAL" and pivot the data to long format.
  #    - Compute scaling ratios relative to the 2018 value.
  # ---------------------------------------------------------------------------
  Total_capacity <- as.data.frame(read_excel(filename, sheet = "Methanol P&D Region IHS", range = "A1:L12"))
  Total_capacity <- Total_capacity[Total_capacity$Region == "TOTAL", ]
  Total_capacity <- tidyr::pivot_longer(Total_capacity, names_to = "Year", cols = c(2:12))
  
  Total_capacity <- Total_capacity %>%
    dplyr::group_by(.data$Region) %>%
    dplyr::mutate(Ratio = .data$value / .data$value[.data$Year == "2018"])
  
  # ---------------------------------------------------------------------------
  # Expand Data for the Years 2010-2020 Using Scaling Ratios
  #    - Generate a complete dataset for each country and year using crossing.
  #    - Join with the capacity data to obtain scaling ratios.
  #    - Compute Adjusted_Value by scaling the 2018 data.
  # ---------------------------------------------------------------------------
  data_2010_2020 <- data %>%
    tidyr::crossing(Year = Total_capacity$Year) %>%
    dplyr::left_join(Total_capacity, by = "Year") %>%
    dplyr::mutate(Adjusted_Value = .data[[subtype[1]]] * .data$Ratio) %>%
    dplyr::select(Country, Year, Adjusted_Value)
  
  # ---------------------------------------------------------------------------
  # Convert Data to a Magpie Object Based on Temporal Coverage
  #    - If subtype[2] is "2010-2020", use the expanded dataset.
  #    - Otherwise, if subtype[2] is "2018", force all data to the year 2018.
  # ---------------------------------------------------------------------------
  if (subtype[2] == "2010-2020") {
    data <- as.magpie(data_2010_2020, spatial = 1, temporal = 2)
  } else if (subtype[2] == "2018") {
    data <- dplyr::mutate(data, Year = "2018") %>%
      dplyr::relocate(Year, .after = 1)
    data <- as.magpie(data, spatial = 1, temporal = 2)
  }
  
  # ---------------------------------------------------------------------------
  # Final Cleanup
  #    - Replace NA values with 0.
  #    - Add the concatenated subtype string as a comment.
  # ---------------------------------------------------------------------------
  data[is.na(data)] <- 0
  subtype <- paste(subtype, collapse = "_")
  getComment(data) <- subtype
  
  # ---------------------------------------------------------------------------
  # Return the Processed Magpie Object
  # ---------------------------------------------------------------------------
  return(data)
}
