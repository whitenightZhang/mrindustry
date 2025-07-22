#' Read IFA
#'
#' Read-in IFA (International fertilizer Association) data .xlsx file containing 
#' production, consumption, export and import volumes as well as capacities for 
#' ammonia and urea as a magclass object.
#'
#' @param subtype Character string specifying the type of IFA product data to read.
#'                Available types are:
#'                \itemize{
#'                  \item For product: ammonia, urea
#'                  \item For data sheet: statistics, capacities
#'                  \item For product characteristics: consumption, production, export, import, capacities
#'                }
#'
#' @return magpie object of the IFA data
#'
#' @author Qianzhi Zhang
#'
#' @seealso [readSource()]
#'
#' \dontrun{
#' a <- readSource(type = "IFA_Chem", subtype = "ammonia_statistics_production")
#' }
#'
#' @importFrom readxl read_excel
readIFA_Chem <- function(subtype) {
  # ---------------------------------------------------------------------------
  # Parse Input and Determine File Parameters
  #    - Split the input subtype (e.g., "ammonia_statistics_production") into components.
  #    - Construct a key using the first two components.
  # ---------------------------------------------------------------------------
  subtype <- unlist(strsplit(subtype, "_"))
  key <- paste(subtype[1], subtype[2], sep = "_")

  # ---------------------------------------------------------------------------
  # Select File Parameters Based on Key
  #    - Use a switch to choose the appropriate file, sheet name, cell ranges, and country list.
  # ---------------------------------------------------------------------------
  File <- switch(key,
                 "ammonia_statistics" = list(
                   filename = "ifa_member_ammonia_statistics_2011_2022.xlsx",
                   sheet_name = "product",
                   ranges = c(
                     production  = "C7:N144",
                     export      = "O7:Z144",
                     import      = "AA7:AL144",
                     consumption = "AM7:AX144"
                   ),
                   countrylist = "B7:B144"
                 ),
                 "urea_statistics" = list(
                   filename = "ifa_member_urea_statistics_2011_2022.xlsx",
                   sheet_name = "product",
                   ranges = c(
                     production  = "C7:N187",
                     export      = "O7:Z187",
                     import      = "AA7:AL187",
                     consumption = "AM7:AX187"
                   ),
                   countrylist = "B7:B187"
                 ),
                 "ammonia_capacities" = list(
                   filename = "ifa_member_ammonia_capacities_2011_2022.xlsx",
                   sheet_name = "Ammonia Cap. by Country (ktp)",
                   ranges = c(capacities = "B2:N84")
                 ),
                 "urea_capacities" = list(
                   filename = "ifa_member_urea_capacities_2011_2022.xlsx",
                   sheet_name = "Urea Cap. by Country (ktp)",
                   ranges = c(capacities = "B2:N73")
                 ),
                 stop("Invalid subtype combination")  # Stop if an invalid subtype is provided
  )
  
  # Extract parameters from the File list
  filename <- File$filename
  sheet_name <- File$sheet_name
  ranges <- File$ranges
  # toolSubtypeSelect selects the appropriate range based on the third component of subtype
  range <- toolSubtypeSelect(subtype[3], ranges)
  
  # ---------------------------------------------------------------------------
  # Read Data from Excel Based on the Selected Subtype
  #    - For "statistics" type: read country names and data, then combine them.
  #    - For "capacities" type: read the data directly.
  # ---------------------------------------------------------------------------
  if (subtype[2] == "statistics") {
    # Read country names
    country_data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = File$countrylist, skip = 5))
    colnames(country_data) <- "Country"  # Rename column to "Country"
    
    # Read data (production, export, import, or consumption) using the selected range
    data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = range, skip = 5))
    
    # Combine country names with data
    data <- cbind(country_data, data)
  } else if (subtype[2] == "capacities") {
    # For capacities, read the data directly (skip header rows if needed)
    data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = range, skip = 1))
  }
  
  # ---------------------------------------------------------------------------
  # Clean and Reshape Data
  #    - Remove rows with missing country names.
  #    - Reshape the data from wide to long format.
  # ---------------------------------------------------------------------------
  data <- data[!is.na(data$Country), ]
  data <- tidyr::pivot_longer(
    data,
    cols = c(2:13),      # Pivot columns 2 through 13 (assumed to be year columns)
    names_to = "Year",   # New column for year
    values_to = "value"  # New column for values
  )
  
  # ---------------------------------------------------------------------------
  # Convert Data to Magpie Object and Final Adjustments
  #    - Convert the cleaned data frame into a magpie object for further processing.
  #    - Replace NA values with 0.
  #    - Set the dimension name based on the key.
  # ---------------------------------------------------------------------------
  data <- as.magpie(data, spatial = 1, temporal = 2)
  data[is.na(data)] <- 0
  getNames(data, dim = "data") <- key
  
  # Return the final magpie object
  return(data)
}
