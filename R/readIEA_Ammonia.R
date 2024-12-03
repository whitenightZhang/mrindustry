#' Read IEA_Ammonia
#'
#' Read-in IEA Ammonia Technology Roadmap 2021 Fig 2.9 Ammonia production by process route and 
#' scenario in major ammonia producing regions data as magclass object
#' 
#' @param subtype Different scenarios of Ammonia data that should be read. Available types are:
#' \itemize{ \item BaseYear_2020: Base year data in 2020
#' \item STEPS_2050: IEA STEPS scenario in 2050
#' \item SDS_2050: IEA SDS scenario in 2050 }
#' 
#' @return magpie object of the IEA Ammonia data
#' @author Qianzhi Zhang
#' @seealso [readSource()]
#' \dontrun{
#' a <- readSource(type = "IEA_Ammonia", subtype = "BaseYear_2020")
#' }
#'
#' @importFrom readxl read_excel tidyr pivot_longer pivot_wider

readIEA_Ammonia <- function(subtype) {
  # Define the file name
  filename <- "IEA Ammonia Technology Roadmap 2021.xlsx"
  
  # Read the specified sheet and range into a data frame
  data <- read_excel(filename, sheet = "Fig 2.9 Ammonia prod route", range = "A1:Y12", skip = 0, col_names = FALSE)
  
  # Transpose the data to make rows into columns and vice versa
  data <- as.data.frame(t(data))
  
  # Set the first row as column names
  colnames(data) <- as.character(unlist(data[1, ]))
  
  # Ensure column names are syntactically valid and unique
  colnames(data) <- make.names(colnames(data), unique = TRUE)
  
  # Rename the first column to "Country"
  colnames(data)[1] <- "Country"
  
  # Filter data based on the provided subtype
  if (subtype == "BaseYear_2020") {
    # Keep rows corresponding to the year 2020
    data <- data %>% dplyr::filter(Year == "2020")
  } else if (subtype == "STEPS_2050") {
    # Keep rows corresponding to the year 2050 under the STEPS scenario
    data <- data %>% dplyr::filter(Year == "2050 STEPS")
  } else if (subtype == "SDS_2050") {
    # Keep rows corresponding to the year 2050 under the SDS scenario
    data <- data %>% dplyr::filter(Year == "2050 SDS")
  }
  
  # Remove the "SUM" column from the data
  data <- subset(data, select = -SUM)
  
  # Convert the wide-format data into long format
  # "Route" will store column names, and corresponding values go into "value"
  data <- tidyr::pivot_longer(data, names_to = "Route", cols = c(3:11))
  
  # Convert the values in the "value" column to numeric
  data$value <- as.numeric(data$value)
  
  # Convert the data into a magpie object for further processing
  data <- as.magpie(data, spatial = 1, temporal = 2)
  
  # Replace NA values with 0
  data[is.na(data)] <- 0
  
  # Return the processed magpie object
  return(data)
}

