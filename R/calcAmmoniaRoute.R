
calcAmmoniaRoute <- function() { # nolint: object_name_linter.
  x <- calcOutput("IEAAmmonia", subtype = "ammonia_statistics_production", aggregate = FALSE)
  y <- calcOutput("IFAChem_Production", subtype = "ammonia_statistics_production", aggregate = FALSE)

  # x <- readSource("IEA_Ammonia", subtype = subtype)
  return(list(
    x = x,
    weight = NULL,
    unit = "TODO ???",
    description = "TODO ???"
  ))
}
