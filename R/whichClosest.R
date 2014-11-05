#' @export
#' @title Find index(ices) of closest value(s).
#' @param x vector to search through
#' @param y number to find closed value to
#' @return index(ices) of closest value(s).
whichClosest <- function(x, y){
  which(abs(x-y)==min(abs(x-y)))
}