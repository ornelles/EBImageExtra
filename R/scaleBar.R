#' Add Scale Bar to Image
#' 
#' Place a single horizontal scale bar \code{pixels} pixels wide corresponding
#' to a physical distance \code{distance} with an optional \code{label}.
#' 
#' @param x,y Central coordinates of the scale bar in pixels
#' @param pixels Width of the scale bar in pixels
#' @param distance Distance represented by the scale bar in microns
#' @param label A suitable label that can be coerced to \code{character} or
#'   an \code{expression}. If \code{NULL}, a label will be generated from
#'   the \code{distance} argument. To exclude any label, set this argument to
#'   \code{""} or to \code{NA}
#' @param col Default color (white) for \code{col.text} and \code{col.line}
#' @param col.text,cex,adj,xpd,... Parameters passed to the \code{\link{text}}
#'   function used to add the label
#' @param col.line,lwd,lend Parameters passed to the \code{\link{lines}}
#'   function used to draw the horizontal scale bar
#' 
#' @details
#' This is a utility to place a labeled scale bar on a raster image. 
#' A default label in microns will
#' be added if \code{label} is \code{NULL}. The label is centered at the
#' position specified by \code{x,y} according to the values in \code{adj}.
#' The default value for \code{adj} of \code{c(0.5,-0.5)} centers the label
#' and places it 0.5 character widths above \code{x,y}.
#'  
#' The \code{x,y} coordinates can be passed in a plotting structure
#' appropriate for \code{\link{xy.coords}}. This function attempts to parse
#' \code{distance} and \code{pixels} from the remaining arguments if they are
#' not named and if \code{x} is a list with components named "x" and "y". This 
#' allows one to interactively place a scale bar with a function
#' call such as in the following example. In this example, \code{ppm} stands
#' for "pixels per micrometer" and is the scale factor appropriate for the
#' given micrograph.
#' This call will place a labeled 25 micron scale bar on the image.  
#' \preformatted{
#' > scaleBar(locator(), 25*ppm, 25)
#' }
#' 
#' Note that this is \strong{not} a vectorized function. Only the last
#' value of \code{(x, y)} will be used to place the scale bar. This allows
#' multiple clicks to \code{locator()} to get the position just right
#' with only the last click being used.
#'
#' Pixels-per-micrometer calibration values for the Nikon TE300 at 1x1
#' binning with the QImaging camera are:
#' \preformatted{
#' Objective   PPM value
#'  20X ELWD    3.098
#'  40X ELWD    6.157
#'  60X ELWD    9.414
#'  60X Apo     9.400
#' 100X Apo    15.563
#'   4X Apo     0.62 (estimated)
#' }
#'
#' @return
#' This function is called for side effects but the location is returned
#' invisibly as a list.
#' 
#' @export
#' 
scaleBar <- function(x, y = NULL, pixels, distance, label = NULL,
	col = "white", col.text, cex = 3/4, adj = c(0.5, -0.5), col.line,
	lwd = 1, lend = 1, xpd = NA, ...)
{
	
	if (missing(x)) {
		cat(c(
			"Usage: scaleBar(x, [y], pixels, distance, ...)",
			" Objective    PPM (pixels per micron)",
			"  20X ELWD  3.098",
			"  40X ELWD  6.157",
			"  60X ELWD  9.414",
			"  60X Apo   9.400",
			" 100X Apo  15.563",
			"   4X Apo   0.62 (estimated)"), sep = "\n")
		return(invisible(NULL))
	}
		
	if (missing(distance) && is.list(x)) {
		distance <- pixels; pixels <- y; y <- NULL
	}
	distance <- distance[1]
	pixels <- pixels[1]
	p <- xy.coords(x, y, recycle = TRUE, setLab = FALSE)
	x <- tail(p$x, 1)
	y <- tail(p$y, 1)
	if (missing(col.text)) col.text <- col
	if (missing(col.line)) col.line <- col
	xx <- x + c(-0.5, 0.5)*pixels
	yy <- c(y, y)
	if (is.null(label)) lab <- bquote(.(distance)~~mu*m)
	else if (is.na(label) || label == "") lab <- ""
	else lab <- label
	lines(xx, yy, lwd = lwd, lend = lend, col = col.line)
	if (lab != "")
		text(x, y, labels = lab, adj = adj, col = col.text,
			cex = cex, xpd = xpd, ...)
	invisible(list(x = x, y = y))
}
