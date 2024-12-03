#' Read RMI_China
#'
#' Read-in RMI (Rocky Mountain Institute) Transforming China’s Chemicals Industry Pathways and Outlook under the Carbon Neutrality Goal 2022.xlsx 
#' "ES1-3 China Chemical Demand", "ES29 China Chemical Structure" files as
#' magclass object
#'
#' 
#' @param subtype[1] Type of RMI_China data sheets that should be read. Available types are:
#' \itemize{ \item ChemDemand: ES1-3 China Chemical Demand
#' \item ChemStructure:ES29 China Chemical Structure }
#' 
#' #' @param subtype[2] Different products from RMI_China data that should be read. Available types are:
#' \itemize{ \item Ammonia
#' \item Methanol
#' \item Ethylene}
#' 
#' @return magpie object of the RMI_China data
#' @author Qianzhi Zhang
#' @seealso [readSource()]
#' \dontrun{
#' a <- readSource(type = "RMI_China", subtype = "ChemDemand_Ammonia")
#' }
#'
#' @importFrom readxl read_excel tidyr pivot_longer pivot_wider

readRMI_China <- function(subtype) {
  # Parse the subtype to determine processing details
  subtype <- unlist(strsplit(subtype, "_"))
  
  # Define the filename for the RMI report
  filename <- "RMI Transforming China’s Chemicals Industry Pathways and Outlook under the Carbon Neutrality Goal 2022.xlsx"
  
  # Define the configuration for each subtype
  File <- switch(subtype[1],
                 "ChemDemand" = list(
                   sheet_name = "ES1-3 China Chemical Demand",  # Sheet name
                   ranges = c(
                     Ammonia = "A2:AG5", 
                     Methanol = "A6:AG9", 
                     Ethylene = "A10:AG10"
                   ),  # Data ranges for specific chemicals
                   countrylist = "A1:AG1",  # Range for the country list
                   ColumnsRange = c(3:33)  # Range for the columns to pivot
                 ),
                 "ChemStructure" = list(
                   sheet_name = "ES29 China Chemical Structure",  # Sheet name
                   ranges = c(
                     Ammonia = "A1:E7", 
                     Methanol = "A8:E16", 
                     Ethylene = "A17:E25"
                   ),  # Data ranges for specific chemicals
                   ColumnsRange = c(2:5)  # Range for the columns to pivot
                 ),
                 stop("Invalid subtype combination")  # Error handling for invalid subtypes
  )
  
  # Extract relevant details from the File configuration
  sheet_name <- File$sheet_name
  ranges <- File$ranges
  countrylist <- File$countrylist
  ColumnsRange <- File$ColumnsRange
  
  # Determine the data range to use based on the subtype
  range <- toolSubtypeSelect(subtype[2], ranges)
  
  # Process the data based on the subtype
  if (subtype[1] == "ChemDemand") {
    # Read the data for chemical demand
    data <- read_excel(filename, sheet = sheet_name, range = range, skip = 0, col_names = FALSE)
    
    # Read the country list and set as column names
    country_data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = countrylist, skip = 0, col_names = FALSE))
    colnames(data) <- country_data[1, ]  # Use the first row of country_data as column names
    
    # Convert the data from wide to long format
    data <- tidyr::pivot_longer(data, names_to = "Year", cols = ColumnsRange)
    
    # Add a "Region" column with the value "China"
    data[[1]] <- "China"
    colnames(data)[1] <- "Region"
    
  } else if (subtype[1] == "ChemStructure") {
    # Read the data for chemical structure
    data <- read_excel(filename, sheet = sheet_name, range = range, skip = 0)
    
    # Convert the data from wide to long format
    data <- tidyr::pivot_longer(data, names_to = "Year", cols = ColumnsRange)
    
    # Add a "Region" column with the value "China"
    data <- cbind(Region = "China", data)
  }
  
  # Convert the processed data to a magpie object
  data <- as.magpie(data, spatial = 1, temporal = 3)
  
  # Replace NA values with 0 for consistency
  data[is.na(data)] <- 0
  
  # Return the processed magpie object
  return(data)
}

