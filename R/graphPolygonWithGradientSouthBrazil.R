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
#' @param rev logical to indicate if Brewer colours should be reversed.
#' @return None. A pdf file is written.
graphPolygonWithGradientSouthBrazil <- function(colourVar, dataPoly, 
                                           legendTitle, fileName, plotTitle,
                                           directoryPlots=getwd(),
                                           backPolygon=NULL, brewerPalette="Spectral",
                                           fontFam = NULL,
                                           rev=TRUE,
                                           limits=NULL){
  if (is.null(limits)) {
    limits <- c(min(colourVar, na.rm=TRUE), max(colourVar, na.rm=TRUE))
  }
    
  dataPoly$tmpColour <- colourVar
  if (!(class(backPolygon) == "data.frame")) {
    backPolygon <- convertShpPolyToDf(backPolygon)
  }
  #test
  2+2
  if (!(class(dataPoly) == "data.frame")) {
    dataPoly <- convertShpPolyToDf(dataPoly)
  }

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
  
  plotMap <- plotMap + theme(axis.text=element_blank(), 
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          plot.margin=unit(c(-1.5,0,-1.5,0),"cm")) +
    theme(plot.title=element_text(size=20, family = fontFam)) +
    theme(legend.title=element_text(size=12, family = fontFam)) +
    scale_x_continuous(limits = c(-62.5, -37)) +
    ggtitle(plotTitle) +
    coord_fixed()
  
  owd <- getwd()
  setwd(dir=directoryPlots)

  fileName1 <- paste0(fileName, ".pdf")
  ggsave(filename = fileName1, plot = plotMap, width = 7, height = 5.2)
  fileName2 <- paste0(fileName, ".png")
  ggsave(filename = fileName2, plot = plotMap, width = 7, height = 5.2, dpi = 72)
  fileName3 <- paste0(fileName, ".jpg")
  ggsave(filename = fileName3, plot = plotMap, width = 7, height = 5.2, dpi = 144)
  
  setwd(owd)
  return(plotMap)
}