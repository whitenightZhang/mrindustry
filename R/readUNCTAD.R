#' Read UNCTAD
#'
#' Read-in US_PlasticsTradebyPartner file as
#' a magclass object.
#' 
#' UNCTAD is United Nations Conference on Trade and Development.
#'
#' @return magpie object of the CUNCTAD data
#'
#' @author Qianzhi Zhang
#'
#' @seealso [readSource()]
#'
#' \dontrun{
#' a <- readSource(type = "UNCTAD")
#' }
#'
#' @importFrom readxl read_excel
#' 
readUNCTAD <- function() {
  browser()
  # ---------------------------------------------------------------------------
  # 1. Read Data from Excel
  #    - Load the "ChinaBaogao Methanol 2023.xlsx" file from the "China Methanol P&D" sheet
  #      using the specified cell range.
  # ---------------------------------------------------------------------------
  data <- read_csv("US_PlasticsTradebyPartner.csv") %>%
    select(1,3,5,7,9,10,13)
  data <- as.magpie(data, spatial = 2, temporal = 1)
  # ---------------------------------------------------------------------------
  # 2. Add Region Information
  #    - Since the data is for China, add a "Region" column with the value "China".
  # ---------------------------------------------------------------------------
  data <- cbind(Region = "China", data)
  
  # ---------------------------------------------------------------------------
  # 3. Reshape Data to Long Format
  #    - Pivot the data so that the years (from 2015 to 2023) become a single "Year" column,
  #      and the corresponding values go into a "Value" column.
  # ---------------------------------------------------------------------------
  data <- tidyr::pivot_longer(
    data,
    cols = "2015":"2023",
    names_to = "Year",
    values_to = "Value"
  )
  
  # ---------------------------------------------------------------------------
  # 4. Convert to MagPIE Object
  #    - Convert the reshaped data frame to a magpie object with the appropriate spatial and temporal dimensions.
  # ---------------------------------------------------------------------------
  data <- as.magpie(data, spatial = 1, temporal = 3)
  
  # ---------------------------------------------------------------------------
  # 5. Final Cleanup
  #    - Replace any NA values in the magpie object with 0.
  # ---------------------------------------------------------------------------
  data[is.na(data)] <- 0
  
  # ---------------------------------------------------------------------------
  # 6. Return the Processed MagPIE Object
  # ---------------------------------------------------------------------------
  return(data)
}
