#' Calculate different feedstocks for the chemical sector from IEA energy balances
#'
#' @author Qianzhi Zhang
#'
#' @export
#' @importFrom dplyr inner_join
#'
calcIndustryFE <- function() {
  # Read in Final Energy (FE) data from IEA and convert from ktoe to EJ
  iea <- readSource("IEA", subtype = "EnergyBalances", convert = TRUE)[, , "INDPROD"] %>%
    collapseDim() * 4.1868e-5  # Conversion factor from ktoe to EJ
  
  # Replace NA values with 0 for consistency
  iea[is.na(iea)] <- 0
  
  # Create a new magpie object to store chemical feedstocks
  TOTIND <- new.magpie(
    cells_and_regions = getItems(iea, dim = 1),  # Spatial dimension (regions)
    years = getItems(iea, dim = 2),             # Temporal dimension (years)
    names = c("solids", "liquids", "gases")     # Data categories (feedstock types)
  )
  
  # Load the product mapping file
  product_mapping <- toolGetMapping(
    name = "structuremappingIO_outputs_NECHEM.csv",
    type = "sectoral",
    where = "mrremind"
  ) %>%
    dplyr::as_tibble() %>%  # Convert mapping to a tibble for easier manipulation
    dplyr::right_join( # Map REMIND categories to feedstock groups
      tibble::tribble(
        ~remind, ~group,
        "pecoal", "solids",   # Coal-based products
        "peoil", "liquids",  # Oil-based products
        "pegas", "gases"     # Gas-based products
      ),
      "remind"
    ) %>%
    dplyr::select("products", "group") %>%  # Keep only products and group columns
    dplyr::filter(!!sym("products") %in% getItems(iea, dim = 3.1))  # Filter products that exist in the IEA data
  
  # Loop through each feedstock group (solids, liquids, gases)
  for (g in unique(product_mapping$group)) {
    # Filter products that belong to the current group
    products <- product_mapping %>%
      dplyr::filter(g == .data$group) %>%
      dplyr::select("products") %>%
      dplyr::pull()
    
    # Aggregate IEA data for the selected products and assign it to the feedstock group
    TOTIND[, , g] <- dimSums(iea[, , products], dim = 3, na.rm = TRUE)
  }
  
  # Return the results as a list with additional metadata
  return(list(
    x = TOTIND,
    weight = NULL,
    unit = "EJ",
    description = "Different feedstocks for the chemical sector from IEA energy balances"
  ))
}
