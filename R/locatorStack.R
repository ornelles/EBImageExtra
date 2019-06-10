#' Interact with plotted image stack to select frames
#' 
#' The function \code{\link{locator}} is called to identify and select frames
#' on an image stack.
#' 
#' @param x An \code{Image} object of \code{array} of such objects
#' @param labels Optional labels to be added to each found frame. If
#'   \code{missing}, the number of each frame will serve as the label
#' @param nx An \code{integer} of length 1 specifying the width of the image
#'   stack. If missing, a value will be chosen to create an equal number of
#'   frames in the x- and y-directions
#' @param col Color of the label to be added
#' @param offset A numeric vector fo length 2 specifying the relative position
#'   of the label in each frame where the default values of \code{c(0.05, 0.05)}
#'   place the label at the top left corner
#' @param adj One or two values in [0, 1] which specify the \code{x} (and
#'   optionally \code{y}) adjustment of the labels. See \code{\link{text}} for
#'   details.
#' @param ... Additional values passed to \code{\link{text}} for labeling
#' 
#' @seealso \code{\link{labelStack}}, \code{\link{plotStack}}
#' 
#' @details
#' The base function \code{\link{locator}} will be called until canceled by the
#' user to identify frames in the *previously* plotted image stack. The
#' selected frame will be labeled and a count of the (unique) selected frames
#' will be reported in a running tally. The selected frames, in the order that
#' they were selected, will be returned as a vector of integers.
#' 
#' @return
#' A vector of selected frames
#' 
#' @import EBImage
#' 
#' @export
#'
locatorStack <- function(x, labels, nx, col = "red",
		offset = c(0.05, 0.05), adj = c(0, 1), ...)
{
	if (class(x) != "Image") stop("'x' must be an Image object")
	if (length(offset) == 1) offset <- rep(offset, 2)
	if (length(adj) == 1) adj <- rep(adj, 2)
	nf <- numberOfFrames(x, type = "render")
	if (missing(nx)) nx <- ceiling(sqrt(nf))
	ny <- ceiling(nf/nx)
	ss <- seq_len(nf)
	dm <- dim(x)
	xx <- dm[1]*((ss - 1)%%nx + offset[1])
	yy <- dm[2]*((ss - 1)%/%nx + offset[2])
	if (missing(labels)) labels <- as.character(ss)

# intervals for binning mouse clicks
	vx <- seq(0, dm[1]*nx + 1, length = nx + 1)
	vy <- seq(0, dm[2]*ny + 1, length = ny + 1)

# interact with user
	found <- integer()
	while (TRUE) {
		p <- locator(1)
		if (is.null(p)) break
		ix <- findInterval(p$x, vx)
		iy <- findInterval(p$y, vy)
		i <- ix + nx*(iy - 1)
		if (!i %in% ss) next # only accept valid values
		text(xx[i], yy[i], labels[i], adj = adj[1:2], col = col, ...)
		found <- unique(c(found, i))
		cat("count:", length(found), "\r")
		flush.console()
	}
	cat("\n")
	return(found)
}
