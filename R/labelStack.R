#' Label frames of an image stack
#' 
#' Add labels to a plotted image stack
#' 
#' @param x A recently plotted \code{Image} object of \code{array} of such
#'   objects
#' @param labels Labels to be added to each frame of the image as a
#'   \code{character} vector, or an \code{expression}. If \code{NULL}, the number
#'   of each frame will serve as the label
#' @param nx An \code{integer} of length 1 specifying the width of the image
#'   stack. If missing, a value will be chosen to create an equal number of
#'   frames in the x- and y-directions
#' @param col Color of the label to be added
#' @param offset A numeric vector of length 2 specifying the relative position
#'   of the label in each frame where the default values of \code{c(0.05, 0.05)}
#'   place the label at the top left corner
#' @param adj One or two values in [0, 1] which specify the \code{x} (and
#'   optionally \code{y}) adjustment of the labels. See \code{\link{text}} for
#'   details.
#' @param ... Additional values passed to \code{\link{text}} for labeling
#' 
#' @details
#' Each frame of an image plotted as a raster object will be labeled as
#' specified by \code{offset} and \code{adj}.
#' 
#' @seealso \code{\link{plotStack}}, \code{\link{locatorStack}}
#' 
#' @return
#' This function is called to label a plotted image. No value is returned.
#' 
#' @examples
#' x <- readImage(system.file("images", "sample-color.png", package="EBImage"))
#' x <- EBImage::untile(x, c(4, 3), lwd = 0)
#' idx <- sample(1:12)
#' plotStack(x[,,,idx], label = paste("Frame", 1:12))
#' labelStack(x, labels = idx, col = "yellow", offset = 0.5, adj = 0.5, cex = 2)
#' 
#' @import EBImage
#' 
#' @export
#' 
labelStack <- function(x, labels = NULL, nx, col = "white",
		offset = c(0.05, 0.05), adj = c(0, 1), ...)
{
	if (class(x) != "Image") stop("'x' must be an Image object")
	if (length(offset) == 1) offset <- rep(offset, 2)
	if (length(adj) == 1) adj <- rep(adj, 2)
	nf <- numberOfFrames(x, type = "render")
	if (missing(nx)) nx <- ceiling(sqrt(nf))
	ss <- seq_len(nf)
	if (is.null(labels))
		labels <- as.character(ss)
	else if (is(labels, "logical") && labels == TRUE)
		labels <- as.character(ss)
	else if (is(labels, "logical") && labels == FALSE)
		labels <- ""
	ix <- (ss - 1)%%nx
	iy <- (ss - 1)%/%nx
	dm <- dim(x)
	xx <- dm[1]*(ix + offset[1])
	yy <- dm[2]*(iy + offset[2])
	text(xx, yy, labels, adj = adj[1:2], col = col, ...)
}
