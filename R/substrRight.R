#' @export
#' @title 
#' Substring from the right of a \code{character}
#' 
#' @description
#' Substring from the right of a \code{character}
#' 
#' @param x \code{character} to be extracted from
#' @param n number of characters to extract
#' 
#' @return \code{character}
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}