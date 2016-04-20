#' @export
#' @title
#' Plot a polygon with discrete colour fill over a polygon with black lines.
#' @description
#' Plot a polygon with colour fill over a polygon with black lines. Saves to a large pdf
#' and a small png
#' @param colourVar categorical variable that defines the gradient
#'   colour. Same length and order as dataPoly.
#' @param dataPoly SpatialPolygonsDataFrame to be plotted.
#' @param fileName filename (not with directory or extension) as character.
#' @param legendTitle legend title as character.
#' @param directoryPlots directory to store plots
#' @param backPolygon optional background polygon to be
#' plotted on top of - fortified or raw (prefortifying will be faster if
#' several maps are being generated).
#' @param brewerPalette brewer palette name as character. Default is "BuGn".
#' @return A ggplot object. In addition, a pdf file is written with name \code{fileName}
#' in directory \code{directoryPlots}.
graphPolygonWithCategoriesSouthBrazil <- function(colourVar, dataPoly, 
                                                legendTitle, fileName, plotTitle,
                                                directoryPlots=getwd(),
                                                backPolygon=NULL, brewerPalette="BuGn",
                                                fontFam = NULL){
  dataPoly$tmpColour <- colourVar
  
  if (!(class(backPolygon) == "data.frame")) {
    backPolygon <- convertShpPolyToDf(backPolygon)
  }
  if (!(class(dataPoly) == "data.frame")) {
    dataPoly <- convertShpPolyToDf(dataPoly)
  }
  
  plotMap <- ggplot(dataPoly, aes(x = long, y = lat, fill = tmpColour)) + geom_polygon(aes(group = group)) + 
    geom_polygon(data = backPolygon, aes(group = group), alpha = 1, colour = "black", fill = NA, size = 1) + 
    coord_equal() + scale_fill_brewer(legendTitle, palette = brewerPalette, na.value = NA) +
    theme(axis.text=element_blank(),
          axis.ticks=element_blank(),
          axis.title=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          panel.margin = unit(c(0,0,0,0),"cm"),
          plot.margin=unit(c(0,0,-0.5,0),"cm")) +
    theme(plot.title=element_text(size=20, family = fontFam)) +
    theme(legend.title=element_text(size=12, family = fontFam)) +
    ggtitle(plotTitle) +
    scale_x_continuous(limits = c(-62.5, -37)) +
    coord_fixed(ylim = c(-33.740, -7.355))
  
  fileName1 <- paste0(file.path(directoryPlots, fileName), ".pdf")
  ggsave(filename = fileName1, plot = plotMap, width = 12, height = 6)
  fileName2 <- paste0(file.path(directoryPlots, fileName), ".png")
  ggsave(filename = fileName2, plot = plotMap, width = 12, height = 6, dpi = 72)
  fileName3 <- paste0(file.path(directoryPlots, fileName), ".jpg")
  ggsave(filename = fileName3, plot = plotMap, width = 12, height = 6, dpi = 144)
  return(plotMap)
}