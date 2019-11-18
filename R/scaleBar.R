#' Add Scale Bar to Image
#' 
#' Place a single horizontal scale bar \code{width} pixels wide corresponding
#' to a physical distance \code{distance} with an optional \code{label}.
#' 
#' @param x,y Central coordinates of the scale bar in pixels
#' @param width Width of scale bar in pixels
#' @param distance Corresponding distance to the scale bar in microns
#' @param label A suitable label that can be coerced to \code{character} or
#'   an \code{expression}. If \code{NULL}, a label will be generated from
#'   \code{distance}. Use \code{""} or \code{NA} to use no label
#' @param col,col.text,cex,adj,... Parameters passed to the \code{\link{text}}
#'   function to draw the label
#' @param col.line,lwd,lend Parameters passed to \code{\link{lines}} function
#'   to draw the horizontal line
#' 
#' @details
#' This is a utility to place a labeled scale bar on a raster image. 
#' A default label in microns will
#' be added if \code{label} is \code{NULL}. The label is centered above the
#' position in \code{x,y} according to the values in \code{adj}. The default
#' value of \code{c(0.5,-0.5)} centers the label and places it 0.5 character
#' widths above the \code{x,y} position. 
#' The \code{x,y} coordinates can be passed in a plotting structure
#' appropriate for \code{\link{xy.coords}}. This function attempts to parse
#' \code{distance} and \code{width} from the remaining positions if they are not
#' named and \code{x} is a list with \code{x} and \code{y} components. This 
#' allows one to interactively place a scale bar with a function call such as
#' the following:
#' \preformatted{
#' > scaleBar(locator(), 100, 12)
#' }
#' 
#' Note that this is \strong{not a vectorized function.} Only the last
#' value(s) \code{x,y} will be used to place the scale bar. This allows multiple
#' clicks to \code{locator()} with the last one being used. If \code{col.text}
#' and \code{col.line} are missing, these will be assigned the color specified
#' in \code{col}. 
#'
#' @return
#' This function is called for side effects. No value is returned.
#' 
#' @export
#' 
scaleBar <- function(x, y = NULL, width, distance, label = NULL,
	col = "white", col.text, cex = 3/4, adj = c(0.5, -0.5), col.line,
	lwd = 1, lend = 1, ...)
{
	if (missing(distance) && is.list(x)) {
		distance <- width; width <- y; y <- NULL
	}
	distance <- distance[1]
	width <- width[1]
	p <- xy.coords(x, y, recycle = TRUE, setLab = FALSE)
	x <- tail(p$x, 1)
	y <- tail(p$y, 1)
	if (missing(col.text)) col.text <- col
	if (missing(col.line)) col.line <- col
	xx <- x + c(-0.5, 0.5)*width
	yy <- c(y, y)
	if (is.null(label)) lab <- bquote(.(distance)~~mu*m)
	else if (is.na(label) || label == "") lab <- ""
	else lab <- label
	lines(xx, yy, lwd = lwd, col = col.line)
	if (lab != "")
		text(x, y, labels = lab, adj = adj, col = col.text, cex = cex, ...)
}
