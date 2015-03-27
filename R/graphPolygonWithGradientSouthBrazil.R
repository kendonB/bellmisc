#' @export
#' @title
#' Plot a polygon with colour fill over a polygon with black lines.
#' @description
#' Plot a polygon with colour fill over a polygon with black lines. Saves to a large pdf
#' and a small png
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
#' @param myrev logical to indicate if Brewer colours should be reversed.
#' @return None. A pdf file is written.
graphPolygonWithGradientSouthBrazil <- function(colourVar, dataPoly,
                                                legendTitle, fileName, plotTitle,
                                                directoryPlots=getwd(),
                                                backPolygon=NULL, brewerPalette="RdYlGn",
                                                fontFam = NULL,
                                                myrev=TRUE,
                                                limits=NULL,
                                                heightAdj = 1,
                                                widthAdj = 1){
  if (is.null(limits)) {
    limits <- c(min(colourVar, na.rm=TRUE), max(colourVar, na.rm=TRUE))
  }
  
  dataPoly$tmpColour <- colourVar
  if (!(class(backPolygon) == "data.frame")) {
    backPolygon <- convertShpPolyToDf(backPolygon)
  }
  
  if (!(class(dataPoly) == "data.frame")) {
    dataPoly <- convertShpPolyToDf(dataPoly)
  }
  
  plotMap <- ggplot(dataPoly, aes(x=long, y=lat, group=group))+
    geom_polygon(aes(fill=tmpColour))+
    geom_polygon(data = backPolygon, aes(group = group), alpha = 1, colour = "black", fill = NA)
  
  if (limits[1] < 0 & limits[2] > 0) {
    # Get the middle one
    tmp <- rescale(c(limits[1], 0, limits[2]))[2]
    values <- c(rescale(seq(limits[1], 0, length.out = 5),to = c(0, tmp)),
                rescale(seq(0, limits[2], length.out = 5),to = c(tmp, 1)))
    values <- values[-5]
  } else {
    # The range doesn't cover zero so just use the top/bottom half
    values <- seq(-1, 1, length.out = 9)
  }
  
  if (myrev){
    plotMap <- plotMap + scale_fill_gradientn(legendTitle, na.value = NA, limits=limits,
                                              colours=rev(brewer.pal(9,brewerPalette)),
                                              values=values)
  } else {
    plotMap <- plotMap + scale_fill_gradientn(legendTitle, na.value = NA, limits=limits,
                                              colours=brewer.pal(9,brewerPalette),
                                              values=values)
  }
  
  plotMap <- plotMap + theme(axis.text=element_blank(), 
                             axis.ticks=element_blank(),
                             axis.title=element_blank(),
                             panel.grid.major = element_blank(),
                             panel.grid.minor = element_blank(),
                             panel.background = element_blank(),
                             panel.margin = unit(c(0,0,0,0),"cm"),
                             plot.margin=unit(c(-1,0,-1.5,0),"cm")) +
    theme(plot.title=element_text(size=20, family = fontFam)) +
    theme(legend.title=element_text(size=16, family = fontFam)) +
    scale_x_continuous(limits = c(-62.5, -37)) +
    scale_y_continuous(limits = c(min(dataPoly$lat), max(dataPoly$lat))) +
    ggtitle(plotTitle) +
    coord_fixed()
  
  fileName1 <- paste0(file.path(directoryPlots, fileName), ".pdf")
  ggsave(filename = fileName1, plot = plotMap, width = 7*widthAdj, height = 4.5*heightAdj)
  fileName2 <- paste0(file.path(directoryPlots, fileName), ".png")
  ggsave(filename = fileName2, plot = plotMap, width = 7*widthAdj, height = 4.5*heightAdj, dpi = 72)
  fileName3 <- paste0(file.path(directoryPlots, fileName), ".jpg")
  ggsave(filename = fileName3, plot = plotMap, width = 7*widthAdj, height = 4.5*heightAdj, dpi = 144)
  return(plotMap)
}