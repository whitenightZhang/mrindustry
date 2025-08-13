#' Read RMI_China
#'
#' Read-in RMI (Rocky Mountain Institute) "Transforming China Chemicals Industry Pathways and Outlook under the Carbon Neutrality Goal 2022.xlsx"
#' data from either "ES1-3 China Chemical Demand" or "ES29 China Chemical Structure" sheets as a magclass object.
#' Data contains chemical demand projections 2020-2050 for ammonia, methanol and ethylene and the feedstock structure for the production of ammonia, methanol and ethylene
#' in the zero-carbon scenario.
#'
#' @param subtype[1] Type of RMI_China data sheet to read. Available types are:
#'   \itemize{
#'     \item ChemDemand: ES1-3 China Chemical Demand
#'     \item ChemStructure: ES10 China Chemical Structure
#'   }
#' @param subtype[2] The specific product from the RMI_China data to read. Available types are:
#'   \itemize{
#'     \item Ammonia
#'     \item Methanol
#'     \item Ethylene
#'   }
#' @return MagPIE object of the RMI_China data.
#' @author Qianzhi Zhang
#' @seealso [readSource()]
#' \dontrun{
#'   a <- readSource(type = "RMI_China", subtype = "ChemDemand_Ammonia")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer pivot_wider
readRMI_China <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Parse the Input Subtype
  #    - Split the provided subtype string (e.g., "ChemDemand_Ammonia")
  #      into its components.
  # ---------------------------------------------------------------------------
  subtype <- unlist(strsplit(subtype, "_"))

  # ---------------------------------------------------------------------------
  # Define File and Parameter Settings Based on the First Subtype Component
  #    - For "ChemDemand", we use the "ES1-3 China Chemical Demand" sheet.
  #    - For "ChemStructure", we use the "ES29 China Chemical Structure" sheet.
  # ---------------------------------------------------------------------------
  filename <- "RMI Transforming China Chemicals Industry Pathways and Outlook under the Carbon Neutrality Goal 2022.xlsx"
  if (subtype[1] == "ChemDemand") {
    sheet_name <- "ES1-3 China Chemical Demand"
    ranges <- c(
      Ammonia = "A2:AG5",
      Methanol = "A6:AG9",
      Ethylene = "A10:AG10"
    )
    colnameslist <- "A1:AG1"
    ColumnsRange <- c(3:33)
  } else if (subtype[1] == "ChemStructure") {
    sheet_name <- "ES29 China Chemical Structure"
    ranges <- c(
      Ammonia = "A1:E7",
      Methanol = "A8:E16",
      Ethylene = "A17:E25"
    )
    # No colnameslist is provided for ChemStructure; we'll rely on pivoting.
    ColumnsRange <- c(2:5)
  } else {
    stop("Invalid subtype combination")
  }
  
  # ---------------------------------------------------------------------------
  # Select the Data Range Based on the Second Subtype Component
  # ---------------------------------------------------------------------------
  range <- toolSubtypeSelect(subtype[2], ranges)
  
  # ---------------------------------------------------------------------------
  # Process Data Differently for ChemDemand and ChemStructure
  # ---------------------------------------------------------------------------
  if (subtype[1] == "ChemDemand") {
    
    data <- read_excel(filename, sheet = sheet_name, range = range, skip = 0, col_names = FALSE)
    
    # Read first line colnames data and use it as column names.
    colnames_data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = colnameslist, skip = 0, col_names = FALSE))
    colnames(data) <- colnames_data[1, ]
    
    # Pivot the data to long format using the specified columns.
    data <- tidyr::pivot_longer(data, names_to = "Year", cols = ColumnsRange)
    
    # Set the "Region" column to "China" for all rows.
    data[[1]] <- "China"
    colnames(data)[1] <- "Region"
    
  } else if (subtype[1] == "ChemStructure") {
    
    data <- read_excel(filename, sheet = sheet_name, range = range, skip = 0, col_names = TRUE)
    
    # For ChemStructure, simply pivot the data into long format.
    data <- tidyr::pivot_longer(data, names_to = "Year", cols = ColumnsRange)
    
    # Add a constant "Region" column with value "China"
    data <- cbind(Region = "China", data)
  }
  
  # ---------------------------------------------------------------------------
  # Convert the Data into a MagPIE Object
  #    - Convert the data frame to a magpie object with appropriate spatial and temporal dimensions.
  #    - Replace NA values with 0.
  # ---------------------------------------------------------------------------
  data <- as.magpie(data, spatial = 1, temporal = 3)
  data[is.na(data)] <- 0
  
  # ---------------------------------------------------------------------------
  # Return the Processed MagPIE Object
  # ---------------------------------------------------------------------------
  return(data)
}
