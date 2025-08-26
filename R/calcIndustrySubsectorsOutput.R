#' Calculate Industry Subsectors Output
#' Additional corrections are applied to the IEA data in [`mrindustry::tool_fix_IEA_data_for_Industry_subsectors`].
#'
#' @author Michaja Pehl, Falk Benke
calcIndustrySubsectorsOutput <- function() {

  ieamatch <- toolGetMapping(type = "sectoral",
                             name = "structuremappingIO_outputs_Industry_subsectors.csv",
                             where = "mrindustry",
                             returnPathOnly = FALSE)


  data <- readSource("IEA", subtype = "EnergyBalances") * 4.1868e-5

  # apply corrections to IEA data to cope with fragmentary time series
  namesBefore <- getNames(data)
  data <- tool_fix_IEA_data_for_Industry_subsectors(data, ieamatch, threshold = 1e-2)

  # warn if dimensions not present in the mapping have been added to the data
  newProductFlows <- tibble(
    text = setdiff(getNames(data), namesBefore)
  ) %>%
    tidyr::separate("text", c("product", "flow"), sep = "\\.") %>%
    dplyr::anti_join(
      ieamatch %>%
        as_tibble() %>%
        select(product = "iea_product", flow = "iea_flows"),
      c("product", "flow")
    ) %>%
    tidyr::unite("text", c("product", "flow"), sep = ".") %>%
    pull("text")

  if (!rlang::is_empty(newProductFlows)) {
    message("Product/flow combinations not present in mapping added by ",
            "fix_IEA_data_for_Industry_subsectors():\n",
            paste(newProductFlows, collapse = "\n")
    )
  }

  # FIXME remove product/flow combinations from the mapping that have been
  # removed from the data while replacing coke oven and blast furnace outputs
  ieamatch <- ieamatch %>%
    as_tibble() %>%
    filter(paste(.data$iea_product, .data$iea_flows, sep = ".")
           %in% getNames(data))


  target <- c("REMINDitems_in", "REMINDitems_out", "REMINDitems_tech")

  ieamatch <- ieamatch %>%
    as_tibble() %>%
    select(tidyselect::all_of(c("iea_product", "iea_flows", "Weight", target))) %>%
    stats::na.omit() %>%
    tidyr::unite("target", tidyselect::all_of(target), sep = ".", remove = FALSE) %>%
    tidyr::unite("product.flow", c("iea_product", "iea_flows"), sep = ".") %>%
    filter(.data$`product.flow` %in% getNames(data))

  reminditems <-  do.call(
    mbind,
    lapply(unique(ieamatch$target),
           function(item) {
             product_flow <- ieamatch %>%
               filter(item == .data$target) %>%
               pull("product.flow")

             weights <- ieamatch %>%
               filter(item == .data$target) %>%
               pull("Weight") %>%
               as.numeric()

             tmp <- dimSums(data[, , product_flow]
                            * setNames(as.magpie(weights), product_flow),
                            dim = 3, na.rm = TRUE)
             getNames(tmp) <- item

             return(tmp)
           })
  )


  return(list(x = reminditems,
              weight = NULL,
              unit = "EJ",
              description = "IEA Industry Subsector Output Data based on IEA World Energy Balances")
  )

}
