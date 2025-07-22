#' Read-in specific energy consumption (SEC) for the production of key chemicals (2006)
#' from IEA Information Paper Chemical and Petrochemical Sector 2009 (Table 12)
#' 
#' @author Qianzhi Zhang
#'
#' @export
readIEA_PetrochemEI <- function() {
  # ---------------------------------------------------------------------------
  # Read Data from Excel
  #    - Load data from the "Table 12. Petro Regional Coef" sheet of the 
  #      "IEA Chemical and Petrochemical Sector 2009.xlsx" file.
  #    - Remove unwanted columns (Benelux, Canada, France, Italy, Korea, Taiwan):
  #       Only retain countries representative for H12 regions, all regions without 
  #       representative values get assigned the "World value" (e.g. Germany for EUR, World for OAS)
  # ---------------------------------------------------------------------------
  data <- read_excel("IEA Chemical and Petrochemical Sector 2009.xlsx",
                     sheet = "Table 12. Petro Regional Coef",
                     range = "A1:O4",
                     skip = 0) %>%
    select(-Benelux, -Canada, -France, -Italy, -Korea, -Taiwan)
  
  # ---------------------------------------------------------------------------
  # Clean and Convert Data
  #    - Replace any "N/A" entries with 0 and convert the affected columns to numeric.
  # ---------------------------------------------------------------------------
  data <- data %>%
    mutate(across(
      where(~ any(. == "N/A", na.rm = TRUE)),
      ~ as.numeric(ifelse(. == "N/A", 0, .))
    ))
  
  # ---------------------------------------------------------------------------
  # Reshape Data from Wide to Long Format
  #    - Rename the first column from "Region" to "Product".
  #    - Pivot the columns (2:9) into long format, where the new column "Country"
  #      holds the original column names and "Value" holds the corresponding values.
  # ---------------------------------------------------------------------------
  data <- data %>%
    rename(Product = Region) %>%
    tidyr::pivot_longer(
      cols = 2:9,
      names_to = "Country",
      values_to = "Value"
    )
  
  # ---------------------------------------------------------------------------
  # Convert Data to a MagPIE Object and Finalize
  #    - Convert the long-format data frame into a magpie object (using spatial dim 2).
  #    - Replace any remaining NA values with 0.
  # ---------------------------------------------------------------------------
  data <- as.magpie(data, spatial = 2)
  data[is.na(data)] <- 0
  
  # ---------------------------------------------------------------------------
  # Return the Processed MagPIE Object
  # ---------------------------------------------------------------------------
  return(data)
}
