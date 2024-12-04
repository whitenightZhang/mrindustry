#' Read IEA_Petrochem
#'
#' Read-in IEA The Future of  Petrochemicals 2018 “Fig 4.1 Petrochem Production”, "Fig A.1 Petrochem Prod Region",
#' "Fig 4.5 Petrochem Feedstock","Fig 4.9 Petro Prod Route RTS","Fig 5.10 Petro Prod Route CTS" data as magclass object
#'
#' magclass object
#'
#' @param subtype[1] Different data sheets that should be read. Available types are:
#' \itemize{ \item Feedstock: Fig 4.5 Petrochem Feedstock for HVCs, Ammonia, Methanol
#' \item RouteRTS: Fig 4.9 Petro Prod Route RTS (Reference Technology Scenario) for HVCs, Ammonia, Methanol
#' \item RouteCTS: Fig 5.10 Petro Prod Route CTS (Clean Technology Scenario) for HVCs, Ammonia, Methanol
#' \item production3type: Fig 4.1 Petrochem Production for HVCs, Ammonia, Methanol
#' \item production5type: Fig A.1 Petrochem Prod Region for Ethylene, Propylene, BTX, Ammonia, Methanol}
#'
#' #' @param subtype[2] Different products that should be read. Available types are:
#' \itemize{ \item HVCs
#' \item Ammonia
#' \item Methanol
#' \item Ethylene
#' \item Propylene
#' \item BTX}
#'
#' @return magpie object of the IEA Petrochem data
#' @author Qianzhi Zhang
#' @seealso [readSource()]
#' \dontrun{
#' a <- readSource(type = "IEA_Petrochem", subtype = "Feedstock_HVCs")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer pivot_wider

readIEA_Petrochem <- function(subtype) {
  # Split the subtype into components
  subtype <- unlist(strsplit(subtype, "_"))

  # Define the filename for the IEA petrochemicals report
  filename <- "IEA The Future of  Petrochemicals 2018.xlsx"

  # Select parameters based on the first part of the subtype
  File <- switch(subtype[1],
                 "production3type" = list(
                   sheet_name = "Fig 4.1 Petrochem Production",  # Sheet name
                   ranges = c(All = "A1:V5"),                   # Data range
                   ColumnsName = "Route",                       # Name of the column to pivot
                   ColumnsRange = c(3:5)                        # Columns to pivot
                 ),
                 "production5type" = list(
                   sheet_name = "Fig A.1 Petrochem Prod Region",
                   ranges = c(
                     Ethylene = "A1:I8",
                     Propylene = "A13:I20",
                     BTX = "A25:I32",
                     Ammonia = "A37:I44",
                     Methanol = "A49:I56"
                   ),
                   ColumnsName = "Year",
                   ColumnsRange = c(2:9)
                 ),
                 "Feedstock" = list(
                   sheet_name = "Fig 4.5 Petrochem Feedstock",
                   ranges = c(
                     HVCs = "A1:P6",
                     Ammonia = "A7:P12",
                     Methanol = "A13:P18"
                   ),
                   ColumnsName = "Route",
                   ColumnsRange = c(3:6)
                 ),
                 "RouteRTS" = list(
                   sheet_name = "Fig 4.9 Petro Prod Route RTS",
                   ranges = c(
                     HVCs = "A1:P7",
                     Ammonia = "A8:P14",
                     Methanol = "A15:P21"
                   ),
                   ColumnsName = "Route",
                   ColumnsRange = c(3:7)
                 ),
                 "RouteCTS" = list(
                   sheet_name = "Fig 5.10 Petro Prod Route CTS",
                   ranges = c(
                     HVCs = "A1:P7",
                     Ammonia = "A8:P14",
                     Methanol = "A15:P21"
                   ),
                   ColumnsName = "Route",
                   ColumnsRange = c(3:7)
                 ),
                 stop("Invalid subtype combination")  # Error handling for invalid subtypes
  )

  # Extract parameters from the File list
  sheet_name <- File$sheet_name
  ranges <- File$ranges
  ColumnsName <- File$ColumnsName
  ColumnsRange <- File$ColumnsRange

  # Determine the range to use based on the second part of the subtype
  range <- toolSubtypeSelect(subtype[2], ranges)

  # Read the data from the specified Excel file, sheet, and range
  data <- read_excel(filename, sheet = sheet_name, range = range, skip = 0, col_names = FALSE)

  # If the subtype is not "production5type", transpose the data
  if (subtype[1] != "production5type") {
    data <- as.data.frame(t(data))  # Transpose the data
  } else {
    data <- as.data.frame(data)  # Keep the data as is
  }

  # Assign the first row as column names
  colnames(data) <- as.character(unlist(data[1, ]))

  # Ensure column names are syntactically valid and unique
  colnames(data) <- make.names(colnames(data), unique = TRUE)

  # Rename the first column to "Country"
  colnames(data)[1] <- "Country"

  # Remove the first row (now used as column names)
  data <- data[-1, ]

  # Reshape the data into a long format using tidyr::pivot_longer
  data <- tidyr::pivot_longer(data, names_to = ColumnsName, cols = ColumnsRange)

  # Convert the "value" column to numeric
  data$value <- as.numeric(data$value)

  # Convert the data into a magpie object for further processing
  data <- as.magpie(data, spatial = 1, temporal = 2)

  # Replace NA values with 0
  data[is.na(data)] <- 0

  # Add the subtype as a comment to the magpie object
  getComment(data) <- subtype[1]

  # Return the processed magpie object
  return(data)
}
