#' @export
#' @title Collapse a SpatialPolygonsDataFrame
#' @description Collapse a SpatialPolygonsDataFrame by a variable.
#' @param poly \code{SpatialPolygonsDataFrame} to be collapsed
#' @param collapseVarString variable to be collapsed by as a character
#' @return a \code{SpatialPolygonsDataFrame}
collapseSpatialPoly <- function(poly, collapseVarString){
  if (rgeosStatus()) {
    polya <- unionSpatialPolygons(poly, IDs = poly@data[,collapseVarString])
  }
  
  if (rgeosStatus()) {
    poly_df <- as(poly, "data.frame")[!duplicated(poly@data[,collapseVarString]),]
    row.names(poly_df) <- poly_df[,collapseVarString]
    polyb <- SpatialPolygonsDataFrame(polya, poly_df)
    return(polyb)
  }
}