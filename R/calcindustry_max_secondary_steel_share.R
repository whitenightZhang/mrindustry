#' Calculate Maximum Secondary Steel Production Share
#'
#' Reads ExpertGuess/industry_max_secondary_steel_share and expands to all
#' `scenarios`/`regions` using default data.  See [`tool_expand_tibble()`] for
#' details.
#'
#' @param scenarios A character vector of scenarios to expand data to.
#' @param regions A character vector of regions to expand data to.
#'
#' @return A list with a [`magpie`][magclass::magclass] object `x`.
#'

#' @export
calcindustry_max_secondary_steel_share <- function(scenarios = NULL,
                                                   regions = NULL) {
  if (is.null(scenarios)) {
    stop('Scenario definitions missing.')
  }

  if (is.null(regions)) {
    stop('Region definitions missing.')
  }


  x <- toolGetSecondarySteelShare() %>%
    tool_expand_tibble(scenarios, regions) %>%
    pivot_longer(
      !all_of(names(which('character' == unlist(lapply(., typeof)))))) %>%
    as.magpie(spatial = 0, temporal = 0, datacol = ncol(.))

  . <- NULL

  return(list(
    x = x,
    weight = NULL, unit = '', description = ''))
}

#' Maximum share of secondary steel production in total steel production and
#' years between which a linear convergence from historic to target shares is
#' to be applied. (Michaja Pehl)
toolGetSecondarySteelShare <- function() {
  out <- readr::read_csv(
    file = system.file("extdata", "industry_max_secondary_steel_share_v1.csv",
                       package = "mrindustry"
    ),
    comment = "#",
    show_col_types = FALSE
  )


}
