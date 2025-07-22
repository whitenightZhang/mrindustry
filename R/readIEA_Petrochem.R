#' Read IEA_Petrochem
#'
#' Read-in IEA The Future of Petrochemicals 2018 data from several figures 
#' (e.g., "Fig 4.1 Petrochem Production", "Fig A.1 Petrochem Prod Region", 
#' "Fig 4.5 Petrochem Feedstock", "Fig 4.9 Petro Prod Route RTS", 
#' "Fig 5.10 Petro Prod Route CTS") as a MagPIE object.
#'
#' @param subtype[1] Different data sheets to read. Available types are:
#'   \itemize{
#'     \item Feedstock: Fig 4.5 Petrochem Feedstock for HVCs, Ammonia, Methanol
#'     \item RouteRTS: Fig 4.9 Petro Prod Route RTS (Reference Technology Scenario) for HVCs, Ammonia, Methanol
#'     \item RouteCTS: Fig 5.10 Petro Prod Route CTS (Clean Technology Scenario) for HVCs, Ammonia, Methanol
#'     \item production3type: Fig 4.1 Petrochem Production for HVCs, Ammonia, Methanol
#'     \item production5type: Fig A.1 Petrochem Prod Region for Ethylene, Propylene, BTX, Ammonia, Methanol
#'   }
#'
#' @param subtype[2] Different products to read. Available types are:
#'   \itemize{
#'     \item HVCs
#'     \item Ammonia
#'     \item Methanol
#'     \item Ethylene
#'     \item Propylene
#'     \item BTX
#'   }
#'
#' @return MagPIE object of the IEA Petrochem data.
#' @author Qianzhi Zhang
#' @seealso [readSource()]
#' \dontrun{
#'   a <- readSource(type = "IEA_Petrochem", subtype = "Feedstock_HVCs")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer pivot_wider
readIEA_Petrochem <- function(subtype) {
  
  # ---------------------------------------------------------------------------
  # Parse the Input Subtype
  #    - Split the subtype string (e.g., "Feedstock_HVCs") into components.
  # ---------------------------------------------------------------------------
  subtype <- unlist(strsplit(subtype, "_"))
  
  # ---------------------------------------------------------------------------
  # Define File and Parameters Based on Data Sheet Type
  #    - Select the appropriate Excel sheet, data ranges, column names, and pivot columns
  #      based on the first component of subtype.
  # ---------------------------------------------------------------------------
  filename <- "IEA The Future of  Petrochemicals 2018.xlsx"
  File <- switch(subtype[1],
                 "production3type" = list(
                   sheet_name = "Fig 4.1 Petrochem Production",
                   ranges = c(All = "A1:V5"),
                   ColumnsName = "Route",
                   ColumnsRange = c(3:5)
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
                 stop("Invalid subtype combination")
  )
  
  # Extract parameters from the File list
  sheet_name <- File$sheet_name
  ranges <- File$ranges
  ColumnsName <- File$ColumnsName
  ColumnsRange <- File$ColumnsRange
  
  # ---------------------------------------------------------------------------
  # Select the Appropriate Range Based on the Second Subtype Component
  # ---------------------------------------------------------------------------
  range <- toolSubtypeSelect(subtype[2], ranges)
  
  # ---------------------------------------------------------------------------
  # Read Data from Excel
  #    - Read the specified sheet and range from the file.
  #    - For production5type, keep the data as is; for others, transpose the data.
  # ---------------------------------------------------------------------------
  data <- read_excel(filename, sheet = sheet_name, range = range, skip = 0, col_names = FALSE)
  if (subtype[1] != "production5type") {
    data <- as.data.frame(t(data))  # Transpose data for non-production5type sheets
  } else {
    data <- as.data.frame(data)
  }
  
  # ---------------------------------------------------------------------------
  # Set Column Names and Clean Data
  #    - Use the first row as column names, ensure they are unique and syntactically valid,
  #      and rename the first column to "Country". Remove the first row afterwards.
  # ---------------------------------------------------------------------------
  colnames(data) <- as.character(unlist(data[1, ]))
  colnames(data) <- make.names(colnames(data), unique = TRUE)
  colnames(data)[1] <- "Country"
  data <- data[-1, ]
  
  # ---------------------------------------------------------------------------
  # Reshape Data from Wide to Long Format
  #    - Pivot the specified columns into long format with a new column name defined in ColumnsName.
  # ---------------------------------------------------------------------------
  data <- tidyr::pivot_longer(data, names_to = ColumnsName, cols = ColumnsRange)
  
  # Convert the values column to numeric
  data$value <- as.numeric(data$value)
  
  # ---------------------------------------------------------------------------
  # Convert the Data to a MagPIE Object and Finalize
  #    - Convert the data frame to a magpie object with spatial and temporal dimensions.
  #    - Replace NA values with 0.
  #    - Add the concatenated subtype string as a comment to the object.
  # ---------------------------------------------------------------------------
  data <- as.magpie(data, spatial = 1, temporal = 2)
  data[is.na(data)] <- 0
  subtype <- paste(subtype, collapse = "_")
  getComment(data) <- subtype
  
  # ---------------------------------------------------------------------------
  # Return the Processed MagPIE Object
  # ---------------------------------------------------------------------------
  return(data)
}
