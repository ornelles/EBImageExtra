#' Add Scale Bar to Image
#' 
#' Place a single horizontal scale bar of length \code{distance} as per
#' scale factor \code{scale} with unit of measure \code{units}.
#' 
#' @param x,y Central coordinates of the scale bar in pixels
#' @param distance Distance represented by the scale bar in microns
#' @param scale Image scale as µm per pixel
#' @param units Unit of measure, default of "µm". Use \code{NA} to exclude
#'   the label
#' @param col Default color (white) for \code{col.text} and \code{col.line}
#' @param col.text,cex,adj,xpd,... Parameters passed to the \code{\link{text}}
#'   function used to add the label
#' @param col.line,lwd,lend Parameters passed to the \code{\link{lines}}
#'   function used to draw the horizontal scale bar
#' 
#' @details
#' This is a utility to place a labeled scale bar on a raster image. 
#' A default label will include the distance in µm if \code{unit} is not
#' \code{NA}. The label is centered at the position specified by \code{x,y}
#' according to \code{adj}. The default value for \code{adj} of
#' \code{c(0.5,-0.5)} centers the label and places it 0.5 character widths
#' above \code{x,y}.
#'  
#' The \code{x,y} coordinates can be passed in a plotting structure
#' appropriate for \code{\link{xy.coords}}. This function attempts to parse
#' \code{distance} and \code{scale} from the remaining arguments if they are
#' not named and if \code{x} is a list with components named "x" and "y". This 
#' allows one to interactively place a scale bar with a function
#' call such as in the following example. In this example, \code{scale} is
#' the scale factor as "µm per pixel."
#' 
#' This call will place a labeled 25 µm scale bar on the image as per the 
#' value in \code{scale}
#' \preformatted{
#' > scaleBar(locator(), 25, scale)
#' }
#' 
#' Note that this is \strong{not} a vectorized function. Only the last
#' value of \code{(x, y)} will be used to place the scale bar. This allows
#' multiple clicks to \code{locator()} to get the position just right
#' with only the last click being used.
#'
#' Micrometers-per-pixels calibration values for the Nikon TE300 at 1x1
#' binning with the QImaging camera are:
#' \preformatted{
#' Objective   scale factor
#'  20X ELWD    0.3228
#'  40X ELWD    0.1624
#'  60X ELWD    0.1062
#'  60X Apo     0.1064
#' 100X Apo     0.0640
#'   4X Apo     1.613 (estimated)
#' }
#'
#' @return
#' This function is called for side effects but the location is returned
#' invisibly as a list.
#' 
#' @export
#' 
scaleBar2 <- function(x, y = NULL, distance, scale, units = "µm",
	col = "white", col.text, cex = 3/4, adj = c(0.5, -0.5), col.line,
	lwd = 1, lend = 1, xpd = NA, ...)
{
	
	if (missing(x)) {
		cat(c(
			"Usage: scaleBar2(x, [y], distance, scale, ...)",
			" Objective  scale (µm per pixel)",
			"  20X ELWD  0.3228",
			"  40X ELWD  0.1624",
			"  60X ELWD  0.1062",
			"  60X Apo   0.1064",
			" 100X Apo   0.0640",
			"   4X Apo   1.613 (estimated)"), sep = "\n")
		return(invisible(NULL))
	}
		
	if (missing(scale) && is.list(x)) {
		scale <- distance; distance <- y; y <- NULL
	}
	distance <- distance[1]
	scale <- scale[1]
	p <- xy.coords(x, y, recycle = TRUE, setLab = FALSE)
	x <- tail(p$x, 1)
	y <- tail(p$y, 1)
	if (missing(col.text)) col.text <- col
	if (missing(col.line)) col.line <- col
	xx <- x + c(-0.5, 0.5)*distance/scale
	yy <- c(y, y)
	lab <- if(is.na(units)) "" else paste(distance, units)
	lines(xx, yy, lwd = lwd, lend = lend, col = col.line)
	if (lab != "")
		text(x, y, labels = lab, adj = adj, col = col.text,
			cex = cex, xpd = xpd, ...)
	invisible(list(x = x, y = y))
}
