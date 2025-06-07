#'
#' @author Qianzhi Zhang
#'
#' @export
calcOECD_Plastic <- function(subtype) {
  # ---------------------------------------------------------------------------
  # 1. Read source data
  # ---------------------------------------------------------------------------
  x <- readSource("OECD_Plastic", subtype = subtype)
  
  # ---------------------------------------------------------------------------
  # 2. Return packaged output
  # ---------------------------------------------------------------------------
  return(list(
    x = x,
    weight = NULL,
    unit = "t Plastic",
    description = "Data on Plastic MFA from OECD Plastic Outlook"
  ))
}

