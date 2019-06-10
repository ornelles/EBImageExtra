#' Add scale bar to image
#' 
#' Place a horizontal scale bar \code{width} pixels wide corresponding to a
#' physical distance \code{distance} with an optional \code{label}.
#' 
#' @param x,y Coordinate vector of scale par
#' @param width Width of scale bar in pixels
#' @param distance Corresponding distance to the scale bar in microns
#' @param label A suitable text label of \code{expression} for the scale bar. If
#'   \code{NULL}, a label will be generated from \code{distance}. Use \code{""}
#'   or \code{NA} to add no text label
#' @param col,col.text,cex,adj,... Parameters passed to \code{\link{text}}
#'   used to place the label
#' @param col.line,lwd Parameters passed to \code{\link{lines}} used to place
#'   the horizontal line
#' 
#' @details
#' Place a labeled scale bar on a raster image. A default label in microns will
#' be added if \code{label} is \code{NULL}. The label is centered above the
#' position in \code{x,y} as specified by the value of \code{adj = c(0.5,-0.5)}.
#' The \code{x,y} coordinates can be passed in a plotting structure
#' appropriate for \code{\link{xy.coords}}. The function attempts to parse
#' \code{distance} and \code{width} from the remaining positions if they are not
#' named and \code{x} is a list with \code{x} and \code{y} components. 
#' 
#' If \code{col.text} and \code{col.line} are missing, these will be assigned
#' the color specified in \code{col}. 
#' 
#' To interactively place a scale bar, call the function with
#' \code{\link{locator}} such as \code{scaleBar(locator(1), 100, 12)} to invoke
#' \code{locator()} to place a 100 pixel-wide bar corresponding to a distance of
#' 12 um.
#'
#' @return
#' This function is called for side effects. No value is returned.
#' 
#' @export
#' 
scaleBar <- function(x, y = NULL, width, distance, label = NULL,
	col = "white", col.text, cex = 3/4, adj = c(0.5, -0.5), col.line, lwd = 1,
	...)
{
	if (missing(distance) && is.list(x)) {
		distance <- width; width <- y; y <- NULL
	}
	p <- xy.coords(x, y, recycle = TRUE, setLab = FALSE)
	if (missing(col.text)) col.text <- col
	if (missing(col.line)) col.line <- col
	xx <- p$x +c(-1,1)*width/2
	yy <- rep(p$y, 2)
	if (is.null(label)) lab <- bquote(.(distance)~~mu*m)
	else if (is.na(label) || label == "") lab <- ""
	else lab <- label
	lines(xx, yy, lwd = lwd, col = col.line)
	if (lab != "")
		text(p, labels = lab, adj = adj, col = col.text, cex = cex, ...)
}
