#' Plot image stack with optional labels
#' 
#' This is a wrapper to plot all frames of an image as a raster object with the
#' option of labeling each frame.
#' 
#' @param x An \code{Image} object of \code{array} of such objects
#' @param label A \code{logical} value, a \code{character} vector, or an
#'   \code{expression} specifying the addition of labels. No labels are added with
#'   the default value of \code{FALSE}
#' @param nx An \code{integer} of length 1 specifying the width of the image
#'   stack. If missing, a value will be chosen to create an equal number of
#'   frames in the x- and y-directions
#' @param ... Additional parameters passed to \code{\link{labelStack}}
#' 
#' @details
#' The image in \code{x} will be plotted by the command
#' \code{plot(x, all = TRUE, nx = nx)}. If \code{label} is not \code{FALSE},
#' labels will be generated and added with \code{\link{labelStack}} where
#' \code{labels = TRUE} will simply number each frame. Other values of
#' \code{label} will be used as a label, replicating the values in
#' \code{label} as necessary.
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
#' plotStack(x, label = paste("Frame", 1:12))
#' 
#' @import EBImage
#' 
#' @export
#' 
plotStack <- function(x, label = FALSE, nx, ...)
{
	if (class(x) != "Image")
		stop("'x' must be an Image object")
	nf <- numberOfFrames(x, type = "render")
	if (missing(nx))
		nx <- ceiling(sqrt(nf))
	plot(x, all = TRUE, nx = nx)

	if (is.null(label))
		labelStack(x, labs = NULL, nx = nx, ...)
	else if (is(label, "logical") && label == TRUE)
		labelStack(x, labs = NULL, nx = nx, ...)
	else if (is(label, "expression") | is(label, "character"))
		labelStack(x, labs = label, nx = nx, ...)
	else if (!is(label, "logical"))
		labelStack(x, labs = as.character(label), nx = nx, ...)
}
