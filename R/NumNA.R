#' @export
#' @title 
#' Counts the number of NAs in a vector
#' @description
#' Counts the number of NAs in a vector
#' @param vec vector
#' @return \code{numeric}
NumNA <- function(vec) {
  length(vec[which(is.na(vec))])
}
