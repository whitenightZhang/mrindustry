readChinaBaogao <- function() {
  # ---------------------------------------------------------------------------
  # 1. Read Data from Excel
  #    - Load the "ChinaBaogao Methanol 2023.xlsx" file from the "China Methanol P&D" sheet
  #      using the specified cell range.
  # ---------------------------------------------------------------------------
  data <- read_excel("ChinaBaogao Methanol 2023.xlsx",
                     sheet = "China Methanol P&D",
                     range = "A1:J4",
                     skip = 0)
  
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
