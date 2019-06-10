#' Plot image stack with optional labels
#' 
#' This is a wrapper to plot all frames of an image as a raster object with the
#' option of labeling each frame.
#' 
#' @param x An \code{Image} object of \code{array} of such objects
#' @param labels A \code{logical} value, a \code{character} vector, or an
#'   \code{expression} specifying the addition of labels. No labels are added with
#'   the default value of \code{FALSE}
#' @param nx An \code{integer} of length 1 specifying the width of the image
#'   stack. If missing, a value will be chosen to create an equal number of
#'   frames in the x- and y-directions
#' @param ... Additional parameters passed to \code{\link{labelStack}}
#' 
#' @details
#' The image in \code{x} will be plotted by the command
#' \code{plot(x, all = TRUE, nx = nx)}. If \code{labels} is not \code{FALSE},
#' labels will be generated and added with \code{\link{labelStack}} where
#' \code{labels = TRUE} will simply number each frame. Other values of
#' \code{labels} will be used as a label, replicating the values in
#' \code{labels} as necessary.
#' 
#' @seealso \code{\link{labelStack}}, \code{\link{locatorStack}}
#' 
#' @return
#' No value is returned. This function is called to plot an entire \code{Image}
#' object.
#' 
#' @examples
#' x <- readImage(system.file("images", "sample-color.png", package="EBImage"))
#' x <- EBImage::untile(x, c(4, 3), lwd = 0)
#' plotStack(x, labels = paste("Frame", 1:12))
#' 
#' @import EBImage
#' 
#' @export
#' 
plotStack <- function(x, labels = FALSE, nx, ...)
{
	if (class(x) != "Image")
		stop("'x' must be an Image object")
	nf <- numberOfFrames(x, type = "render")
	if (missing(nx))
		nx <- ceiling(sqrt(nf))
	plot(x, all = TRUE, nx = nx)

	if (is.null(labels))
		labelStack(x, labels = NULL, nx = nx, ...)
	else if (is(labels, "logical") && labels == TRUE)
		labelStack(x, labels = NULL, nx = nx, ...)
	else if (is(labels, "expression") | is(labels, "character"))
		labelStack(x, labels = labels, nx = nx, ...)
	else if (!is(labels, "logical"))
		labelStack(x, labels = as.character(labels), nx = nx, ...)
}
