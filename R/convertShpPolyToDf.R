#' @export
#' @title 
#' Converts a \code{SpatialPolygonsDataFrame} to a dataframe () 
#' for plotting in ggplot2.
#' @description
#' Converts a \code{SpatialPolygonsDataFrame} to a dataframe () 
#' for plotting in ggplot2.
#' @param vec vector
#' @return \code{numeric}
convertShpPolyToDf <- function(shpPoly) {
  shpPoly@data$id = rownames(shpPoly@data)
  shpPolypoints = fortify(shpPoly, region = "id")
  shpPolyDf = join(shpPolypoints, shpPoly@data, by = "id")
  return(shpPolyDf)
}