#' @export
#' @title
#' Plot a polygon with colour fill over a polygon with black lines.
#' @description
#' Plot a polygon with colour fill over a polygon with black lines.
#' @param colourVar numeric variable that defines the gradient
#'   colour. Same length and order as dataPoly.
#' @param dataPoly SpatialPolygonsDataFrame to be plotted.
#' @param fileName filename (not with directory or extension) as character.
#' @param legendTitle legend title as character.
#' @param directoryPlots directory to store plots
#' @param backPolygon optional background polygon to be
#' plotted on top of - fortified or raw (prefortifying will be faster if
#' several maps are being generated).
#' @param brewerPalette brewer palette name as character. Default is "Spectral".
#' @param rev logical to indicate if Brewer colours should be reversed.
#' @return a ggplot2 object
graphPolygonWithGradient <- function(colourVar, dataPoly, labels = NULL,
                                     legendTitle = "test", plotTitle = "test",
                                     directoryPlots=getwd(),
                                     backPolygon=NULL, brewerPalette="Spectral",
                                     fontFam = NULL,
                                     rev=TRUE,
                                     limits=NULL){
  if (is.null(limits)) {
    limits <- c(min(colourVar, na.rm=TRUE), max(colourVar, na.rm=TRUE))
  }
  
  dataPoly$tmpColour <- colourVar
  if (!(class(backPolygon) == "data.frame") & !is.null(backPolygon)) {
    backPolygon <- convertShpPolyToDf(backPolygon)
  }
  
  if (!(class(dataPoly) == "data.frame")) {
    dataPoly <- convertShpPolyToDf(dataPoly)
  }
  
  centroids.df <- as.data.frame(coordinates(dataPoly))
  names(centroids.df) <- c("Longitude", "Latitude")  #more sensible column names
  # This shapefile contained population data, let's plot it.
  popList <- worldMap@data$POP2005
  
  pop.df <- data.frame(id = idList, population = popList, centroids.df)
  
  ggplot(pop.df, aes(map_id = id)) + #"id" is col in your df, not in the map object 
    geom_map(aes(fill = population), colour= "grey", map = worldMap.fort) +
    expand_limits(x = worldMap.fort$long, y = worldMap.fort$lat) +
    scale_fill_gradient(high = "red", low = "white", guide = "colorbar", labels = comma) +
    geom_text(aes(label = id, x = Longitude, y = Latitude)) + #add labels at centroids
    coord_equal(xlim = c(-90,-30), ylim = c(-60, 20)) + #let's view South America
    labs(x = "Longitude", y = "Latitude", title = "World Population") +
    theme_bw() 
  
  plotMap <- ggplot(dataPoly, aes(x=long, y=lat, group=group))+
    geom_polygon(aes(fill=tmpColour))+
    geom_polygon(data = backPolygon, aes(group = group), alpha = 1, colour = "black", fill = NA)
  
  if (rev){
    plotMap <- plotMap + scale_fill_gradientn(legendTitle, na.value = NA, limits=limits,
                                              colours=rev(brewer.pal(8,brewerPalette)))
  } else {
    plotMap <- plotMap + scale_fill_gradientn(legendTitle, na.value = NA, limits=limits,
                                              colours=brewer.pal(8,brewerPalette))
  }
  
  
  return(plotMap)
}