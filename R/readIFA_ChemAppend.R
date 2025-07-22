#' Read IFA
#'
#' Read-in IFA (International fertilizer Association) data .xlsx file containing
#' production volumes and/or capacities for ammonium nitrate (AN), ammonium sulphate (AS),
#' calcium ammonium nitrate (CAN) and urea ammonium nitrate (UAN) as a magclass object.
#'
#' @param subtype Character string indicating the IFA product and data type to read.
#'                Available combinations include:
#'                \itemize{
#'                  \item AN_statistics_production
#'                  \item AS_statistics_production
#'                  \item CAN_statistics_production
#'                  \item AN_capacities_capacities, AS_capacities_capacities, UAN_capacities_capacities
#'                }
#'
#' @return Magpie object of the IFA data.
#'
#' @author Qianzhi Zhang
#'
#' @seealso [readSource()]
#'
#' \dontrun{
#'   a <- readSource(type = "IFA_ChemAppend", subtype = "AN_statistics_production")
#' }
#'
#' @importFrom readxl read_excel
readIFA_ChemAppend <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Parse the Input Subtype
  #    - Split the subtype string (e.g., "AN_statistics_production") into its components.
  #    - Construct a key using the first two components to select file parameters.
  # ---------------------------------------------------------------------------
  subtype <- unlist(strsplit(subtype, "_"))
  key <- paste(subtype[1], subtype[2], sep = "_")
  
  # ---------------------------------------------------------------------------
  # Select File Parameters Based on the Key
  #    - Use a switch statement to choose the appropriate file name, sheet,
  #      data ranges, and country list based on the key.
  # ---------------------------------------------------------------------------
  File <- switch(key,
                 "AN_statistics" = list(
                   filename = "ifa_an_public_2012_2023.xlsx",
                   sheet_name = "product",
                   ranges = c(
                     production  = "E7:P18",
                     export      = "Q7:AB18",
                     import      = "AC7:AN18",
                     consumption = "AO7:AZ18"
                   ),
                   countrylist = "A7:A18"
                 ),
                 "AS_statistics" = list(
                   filename = "ifa_as_public_2012_2023.xlsx",
                   sheet_name = "product",
                   ranges = c(
                     production  = "E7:P18",
                     export      = "Q7:AB18",
                     import      = "AC7:AN18",
                     consumption = "AO7:AZ18"
                   ),
                   countrylist = "A7:A18"
                 ),
                 "CAN_statistics" = list(
                   filename = "ifa_can_public_2012_2023.xlsx",
                   sheet_name = "product",
                   ranges = c(
                     production  = "E7:P18",
                     export      = "Q7:AB18",
                     import      = "AC7:AN18",
                     consumption = "AO7:AZ18"
                   ),
                   countrylist = "A7:A18"
                 ),
                 "AN_capacities" = list(
                   filename = "AN Capacities Excel File.xlsx",
                   sheet_name = "AN N",
                   ranges = c(capacities = "A7:K17")
                 ),
                 "AS_capacities" = list(
                   filename = "AS Capacities Excel Files.xlsx",
                   sheet_name = "AS N",
                   ranges = c(capacities = "A7:K17")
                 ),
                 "UAN_capacities" = list(
                   filename = "UAN Capacities Excel Files.xlsx",
                   sheet_name = "UAN N ",
                   ranges = c(capacities = "A7:K17")
                 ),
                 stop("Invalid subtype combination")
  )
  
  # Extract filename, sheet name, and data ranges from the File list.
  filename <- File$filename
  sheet_name <- File$sheet_name
  ranges <- File$ranges
  # Select the appropriate range based on the third component (e.g., "production")
  range <- toolSubtypeSelect(subtype[3], ranges)
  
  # ---------------------------------------------------------------------------
  # Read Data from Excel Based on the Data Sheet Type
  #    - For "statistics" subtypes:
  #         * Read the country list.
  #         * Read the specified data (production, export, import, consumption).
  #         * Combine the country list with the data.
  #    - For "capacities" subtypes:
  #         * Read the data directly.
  # ---------------------------------------------------------------------------
  if (subtype[2] == "statistics") {
    # Read country names from the defined range and rename the column.
    country_data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = File$countrylist, skip = 5))
    colnames(country_data) <- "Country"
    
    # Read the corresponding data.
    data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = range, skip = 5))
    
    # Combine country names with the corresponding data.
    data <- cbind(country_data, data)
    
    # Reshape the data from wide to long format.
    data <- tidyr::pivot_longer(
      data,
      cols = c(2:13),       # Assuming columns 2 to 13 correspond to years.
      names_to = "Year",    # New column for year.
      values_to = "value"   # New column for values.
    )
  } else if (subtype[2] == "capacities") {
    # For capacities, read the data directly.
    data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = range, skip = 5))
    
    # Reshape the data from wide to long format.
    data <- tidyr::pivot_longer(
      data,
      cols = c(2:11),       # Assuming columns 2 to 11 correspond to years.
      names_to = "Year",    # New column for year.
      values_to = "value"   # New column for values.
    )
  }
  
  # ---------------------------------------------------------------------------
  # Convert the Data to a MagPIE Object and Finalize
  #    - Convert the reshaped data to a magpie object with spatial and temporal dimensions.
  #    - Replace NA values with 0.
  #    - Set a comment (dimension name) based on the subtype key.
  # ---------------------------------------------------------------------------
  data <- as.magpie(data, spatial = 1, temporal = 2)
  data[is.na(data)] <- 0
  getComment(data) <- paste(subtype[1], subtype[2], subtype[3], sep = "_")
  
  # ---------------------------------------------------------------------------
  # Return the Processed Data
  # ---------------------------------------------------------------------------
  return(data)
}
