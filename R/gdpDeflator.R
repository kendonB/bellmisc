#' @export
#' @title Generates a GDP deflator from the World Bank
#' @description Generates a GDP deflator from the World Bank
#' @param country country (ISO-2 character codes, e.g. "BR", "US", "CA") 
#' for which the deflator is needed.
#' @param start first year of data
#' @param end last year of data
#' @return a data.frame containing the world bank output and a
#' gdpDeflator index.
gdpDeflator <- function(country, start, end){
  indicator <- WDIsearch()[which(WDIsearch()[,"name"] == "Inflation, GDP deflator (annual %)"), "indicator"]
  gdpDeflator <- WDI(country = country, indicator = indicator, start = start, end = end)
  gdpDeflator$gdpDeflatorIndex <- rep(NA, length(gdpDeflator[,1]))
  
  for (i in 1:length(gdpDeflator[,1])){
    if (i == 1){
      gdpDeflator$gdpDeflatorIndex[i] <- 1000
    } else {
      gdpDeflator$gdpDeflatorIndex[i] <- gdpDeflator$gdpDeflatorIndex[i-1]/(1 + gdpDeflator[i  - 1,3]/100)
    }
  }
  gdpDeflator
}