#' @author Bennet Weiss
calcCementProduction <- function(x) {
  x <- readSource("Andrew2019", convert=TRUE)
  x[is.na(x)] <- 0
  output <- list(x = x, weight = NULL, unit = "tonne (t)", description = "Annual cement production")
  return(output)
}

plotCementProduction <- function(x) {
  df <- as.data.frame(x)
  regions <- unique(df$Region)
  for(region in regions) {
    print(region)
    dfr <- filter(df, df$Region == region)
    png(filename = paste("figures/", region, "cement_production.png", sep = ""))
    plot(dfr$Year, dfr$Value, type = "p", col = "blue", pch = 16,
         xlab = "Time", ylab = "Cement Production (t)", main = region)
    dev.off()
  }  
}