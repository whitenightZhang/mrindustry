#' Read ChinaBaogao
#'
#' Read-in ChinaBaogao(This is a website containing research reports on different Chinese industries) Methanol 2023.xlsx file as
#' magclass object
#'
#'
#' @return magpie object of the China Methanol data
#' @author Qianzhi Zhang
#' @seealso [readSource()]
#' \dontrun{
#' a <- readSource(type = "ChinaBaogao")
#' }
#'
#' @importFrom readxl read_excel
#' @importFrom tidyr pivot_longer pivot_wider

readChinaBaogao <- function() {
  # Read the data from the specified Excel sheet and range
  data <- read_excel("ChinaBaogao Methanol 2023.xlsx",
                     sheet = "China Methanol P&D",
                     range = "A1:J4",
                     skip = 0)

  # Add a "Region" column with the value "China" to indicate the dataset's region
  data <- cbind(Region = "China", data)

  # Transform the data from wide to long format
  data <- tidyr::pivot_longer(
    data,
    cols = "2015":"2023",  # Specify the range of years to pivot
    names_to = "Year",     # Name of the new column that will store year values
    values_to = "Value"    # Name of the new column that will store corresponding values
  )

  # Convert the data to a magpie object
  data <- as.magpie(data, spatial = 1, temporal = 3)

  # Replace NA values with 0
  data[is.na(data)] <- 0

  # Return the processed magpie object
  return(data)
}
