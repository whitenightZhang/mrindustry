#' Read IFA
#'
#' Read-in IFA (International Fertilizer Association) data .xlsx file as
#' magclass object
#'
#'
#' @param subtype[1] Type of IFA product that should be read. Available types are:
#' \itemize{ \item ammonia
#' \item urea}
#'
#' #' @param subtype[2] Different types of IFA data sheets that should be read. Available types are:
#' \itemize{ \item statistics: including production, export, import, consumption
#' \item capacities: including capacities}
#'
#' #' @param subtype[3] Characteristics of different products that should be read. Available types are:
#' \itemize{ \item consumption
#' \item production
#' \item export
#' \item import
#' \item capacities}
#'
#' @return magpie object of the IFA data
#' @author Qianzhi Zhang
#' @seealso [readSource()]
#' \dontrun{
#' a <- readSource(type = "IFA_Chem", subtype = "consumption")
#' }
#'
#' @importFrom readxl read_excel

readIFA_Chem <- function(subtype) {
  # Split the subtype string into components
  subtype <- unlist(strsplit(subtype, "_"))

  # Create a key based on the first two components of the subtype
  key <- paste(subtype[1], subtype[2], sep = "_")

  # Select the appropriate file, sheet, ranges, and other parameters based on the key
  File <- switch(key,
                 "ammonia_statistics" = list(
                   filename = "ifa_member_ammonia_statistics_2011_2022.xlsx",
                   sheet_name = "product",
                   ranges = c(
                     production = "C7:N144",
                     export = "O7:Z144",
                     import = "AA7:AL144",
                     consumption = "AM7:AX144"
                   ),
                   countrylist = "B7:B144"
                 ),
                 "urea_statistics" = list(
                   filename = "ifa_member_urea_statistics_2011_2022.xlsx",
                   sheet_name = "product",
                   ranges = c(
                     production = "C7:N187",
                     export = "O7:Z187",
                     import = "AA7:AL187",
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
                 stop("Invalid subtype combination") # Stop if an invalid subtype is provided
  )

  # Extract filename, sheet name, and ranges from the File list
  filename <- File$filename
  sheet_name <- File$sheet_name
  ranges <- File$ranges
  range <- toolSubtypeSelect(subtype[3], ranges) # Select the range based on the third subtype

  # Process "statistics" subtypes
  if (subtype[2] == "statistics") {
    # Read country names
    countrylist <- File$countrylist
    country_data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = countrylist, skip = 5))
    colnames(country_data) <- "Country" # Rename the column to "Country"

    # Read production/export/import/consumption data
    data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = range, skip = 5))

    # Combine country names with the corresponding data
    data <- cbind(country_data, data)

  } else if (subtype[2] == "capacities") {
    # Process "capacities" subtypes: Read data directly
    data <- as.data.frame(read_excel(filename, sheet = sheet_name, range = range, skip = 1))
  }

  # Remove rows where "Country" is NA (invalid data rows)
  data <- data[!is.na(data$Country), ]

  # Reshape the data from wide to long format
  data <- tidyr::pivot_longer(
    data,
    names_to = "Year",  # Name for the new "Year" column
    cols = c(2:13)      # Specify columns to pivot
  )

  # Convert the data to a magpie object for further processing
  data <- as.magpie(data, spatial = 1, temporal = 2)

  # Replace NA values with 0
  data[is.na(data)] <- 0

  # Assign the key as the dimension name
  getNames(data, dim = "data") <- key

  # Return the processed data
  return(data)
}
