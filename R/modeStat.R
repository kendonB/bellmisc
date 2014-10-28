#' @export
#' @title
#' Mode statistic
#' @description
#' Calculates the mode
#' @param x any vector
#' @return the mode
modeStat <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}