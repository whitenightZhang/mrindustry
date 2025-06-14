#' Read OECD Plastic Data into a magpie Object
#'
#' This function reads OECD plastic use or waste data from Excel files,
#' based on a specified subtype, and returns a cleaned magpie object.
#'
#' @param subtype Character string specifying the dataset and scope.
#'        Valid formats include:
#'        - "Use_2019_region"
#'        - "Use_1990-2019_region"
#'        - "Use_1990-2019_world"
#'        - "WasteEOL_1990-2019_region"
#'        - "WasteType_2019_region"
#'
#' @return magpie object of the OECD Plastic data
#'
#' @author Qianzhi Zhang
#'
#' @seealso [readSource()]
#'
#' \dontrun{
#' a <- readSource(type = "OECD_Plastic", subtype = "Use_2019")
#' }
#' @importFrom readxl read_excel
#' @importFrom dplyr select filter
#' @importFrom magclass as.magpie getComment<-
#' 
readOECD_Plastic <- function(subtype) {
  # ---------------------------------------------------------------------------
  # 1. Parse input and validate
  parts <- strsplit(subtype, "_")[[1]]
  if (length(parts) < 3) {
    stop("Subtype must have at least three components, e.g. 'Use_2019_region'.")
  }
  key <- paste(parts[1], parts[2], sep = "_")  # e.g., "Use_2019"
  level <- parts[3]                             # e.g., "region" or "world"
  
  # ---------------------------------------------------------------------------
  # 2. Map key to Excel file parameters
  params <- switch(key,
                   "Use_2019" = list(
                     file   = "Plastics Use.xlsx",
                     sheet  = "2019",
                     range  = "A1:X1399"
                   ),
                   "Use_1990-2019" = list(
                     file   = "Plastics Use.xlsx",
                     sheet  = "1990-2019",
                     range  = "A1:AB1411"
                   ),
                   "WasteEOL_1990-2019" = list(
                     file   = "Plastics Waste.xlsx",
                     sheet  = "1990-2019 EOL",
                     range  = "A1:Z2232"
                   ),
                   "WasteType_2019" = list(
                     file   = "Plastics Waste.xlsx",
                     sheet  = "2019 WasteType",
                     range  = "A1:X1411"
                   ),
                   stop("Invalid subtype combination: ", key)
  )
  
  # ---------------------------------------------------------------------------
  # 3. Read raw data from Excel
  raw_df <- read_excel(
    path  = params$file,
    sheet = params$sheet,
    range = params$range,
    skip  = 1
  )
  
  # ---------------------------------------------------------------------------
  # 4. Select and filter columns based on subtype
  df <- switch(
    subtype,
    # Plastic use or waste by region
    "Use_2019_region" = raw_df %>%
      select(`Reference area`, `Plastic polymer`, `Plastics application`,
             TIME_PERIOD, OBS_VALUE),
    "WasteType_2019_region" = raw_df %>%
      select(`Reference area`, `Plastic polymer`, `Plastics application`,
             TIME_PERIOD, OBS_VALUE),
    "WasteEOL_1990-2019_region" = raw_df %>%
      select(`Reference area`, `Plastic end-of-life fate`, `Plastic recycling`,
             TIME_PERIOD, OBS_VALUE),
    # Trend across time, filtered by scope
    "Use_1990-2019_region" = raw_df %>%
      select(`Reference area`, `Plastic polymer`, `Plastics application`,
             TIME_PERIOD, OBS_VALUE) %>%
      filter(`Reference area` != "World"),
    "Use_1990-2019_world" = raw_df %>%
      select(`Reference area`, `Plastic polymer`, `Plastics application`,
             TIME_PERIOD, OBS_VALUE) %>%
      filter(`Reference area` == "World"),
    stop("Unsupported subtype: ", subtype)
  )
  
  # ---------------------------------------------------------------------------
  # 5. Convert to magpie object and clean missing values
  # ---------------------------------------------------------------------------
  magpie_data <- as.magpie(df, spatial = 1, temporal = 4)
  magpie_data[is.na(magpie_data)] <- 0
  getComment(magpie_data) <- subtype
  
  return(magpie_data)
}

