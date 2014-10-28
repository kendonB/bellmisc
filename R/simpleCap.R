#' @export
#' @title 
#' Takes a \code{character} and makes the first letter of each word upper case.
#' 
#' @description
#' Takes a \code{character} and makes the first letter of each word upper case.
#' Taken from \link{http://stat.ethz.ch/R-manual/R-patched/library/base/html/chartr.html}
#' 
#' @param x \code{character} to be capitalized
#' 
#' @return \code{character} with capitalized first letter.
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  if (!is.na(s[1])) {
    return(paste(toupper(substring(s, 1, 1)), substring(s, 2), sep = "", collapse = " "))
  } else {
    return(NA)
  }
}