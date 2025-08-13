#' Read IEA_Ammonia
#'
#' Read-in IEA Ammonia Technology Roadmap 2021 Fig 2.9 Ammonia production by process route and
#' scenario in major ammonia producing regions data as a magclass object.
#'
#' @param subtype Different scenarios of Ammonia data that should be read. Available types are:
#'                \itemize{
#'                  \item BaseYear_2020: Base year data in 2020
#'                  \item STEPS_2050: IEA STEPS scenario in 2050
#'                  \item SDS_2050: IEA SDS scenario in 2050
#'                }
#'
#' @return Magpie object of the IEA Ammonia data.
#' @author Qianzhi Zhang
#' @seealso [readSource()]
#' \dontrun{
#'   a <- readSource(type = "IEA_Ammonia", subtype = "BaseYear_2020")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer pivot_wider
readIEA_Ammonia <- function(subtype) {
  
  # ---------------------------------------------------------------------------
  # Read Data from Excel
  #    - Define the file name and read the specified sheet and range.
  #    - The data is read without column names.
  # ---------------------------------------------------------------------------
  filename <- "IEA Ammonia Technology Roadmap 2021.xlsx"
  data <- read_excel(filename, sheet = "Fig 2.9 Ammonia prod route", range = "A1:Y12", 
                     skip = 0, col_names = FALSE)

  # ---------------------------------------------------------------------------
  # Transpose and Set Up Data Frame
  #    - Transpose the data so that rows become columns.
  #    - Set the first row as column names and make them syntactically valid.
  # ---------------------------------------------------------------------------
  data <- as.data.frame(t(data))
  colnames(data) <- as.character(unlist(data[1, ]))
  colnames(data) <- make.names(colnames(data), unique = TRUE)
  
  # Rename the first column to "Country"
  colnames(data)[1] <- "Country"
  
  # ---------------------------------------------------------------------------
  # Filter Data Based on Subtype (Scenario)
  #    - Depending on the scenario specified in subtype, filter the rows by Year.
  # ---------------------------------------------------------------------------
  if (subtype == "BaseYear_2020") {
    data <- data %>% dplyr::filter(.data$Year == "2020")
  } else if (subtype == "STEPS_2050") {
    data <- data %>% dplyr::filter(.data$Year == "2050 STEPS")
  } else if (subtype == "SDS_2050") {
    data <- data %>% dplyr::filter(.data$Year == "2050 SDS")
  }
  
  # ---------------------------------------------------------------------------
  # Clean and Reshape the Data
  #    - Remove the "SUM" column.
  #    - Pivot the data from wide to long format with "Route" and "value" columns.
  # ---------------------------------------------------------------------------
  data <- data[, !(names(data) %in% "SUM")]
  data <- tidyr::pivot_longer(data, 
                              cols = c(3:11), 
                              names_to = "Route", 
                              values_to = "value")
  
  # Convert the "value" column to numeric
  data$value <- as.numeric(data$value)
  
  # ---------------------------------------------------------------------------
  # Convert to Magpie Object and Finalize
  #    - Convert the reshaped data frame to a magpie object.
  #    - Replace any NA values with 0.
  # ---------------------------------------------------------------------------
  data <- as.magpie(data, spatial = 1, temporal = 2)
  data[is.na(data)] <- 0
  
  # ---------------------------------------------------------------------------
  # Return the Processed Magpie Object
  #    - The function returns the final magpie object.
  # ---------------------------------------------------------------------------
  return(data)
}
