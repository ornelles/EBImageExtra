#' Select Frames from a Plotted Image Stack
#' 
#' Identify and select frames in a plotted image stack.
#' 
#' @param x An \code{Image} object or \code{array} of such objects
#' @param labels Optional labels to be added to each selected frame. If
#'   missing, the number of each frame will serve as the label otherwise
#'   the argument will be adjusted with \code{as.graphicsAnnot}
#' @param nx The number of frames in the x-direction of the image stack.
#'   If missing, a square tile of images will be assumed
#' @param col Color of the label to be added
#' @param asImage \code{logical} value (default of \code{FALSE}) indicating
#'   whether the function returns the selection as \code{Image}(s) or
#'   as a numeric vector of found frames (see Details)
#' @param offset A numeric vector of length 2 specifying the relative position
#'   of the label in each frame
#' @param adj One or two values in [0, 1] which specify the \code{x} (and
#'   optionally \code{y}) adjustment of the labels. See the function
#'   \code{\link{text}} for further details
#' @param combine \code{logical} value (default of \code{TRUE}) to return
#' the selected frames combined as a single image rather than a list
#' @param ... Additional values passed to \code{\link{text}} for labeling
#' 
#' @seealso
#' \code{\link{plotStack}}, which allows for automatically labeling the plotted
#' image and \code{\link{labelStack}}, that places custom labels on each 
#' frame of the image stack.
#' 
#' @details
#' The \code{\link{locator}} function will be called to interactively
#' to identify frames in the plotted
#' image stack. Selected frames will be labeled on the image and a count
#' of the unique selected frames will be reported in a running tally on the 
#' console. Selection is stopped by pressing any mouse button other than the
#' primary button or by pressing the \code{ESC} key. The selected frames, in
#' the order that they were selected, will be returned as a vector of integers
#' if \code{asImage = FALSE}. If \code{asImage = TRUE}, the selected images
#' will be returned as a list of separate frames if \code{combine = FALSE}
#' or as a single image if \code{combine = TRUE}.
#' 
#' @return
#' A numeric vector of the selected frames if \code{asImage = FALSE} or
#' either a list of selected \code{Images} frames or combined \code{Image}
#' frames in the order that they were selected.
#' 
#' @import EBImage
#' 
#' @export
#'
locatorStack <- function(x, labels, nx, col = "red", asImage = FALSE,
		offset = c(0.05, 0.05), adj = c(0, 1), combine = TRUE, ...)
{
	if(!is(x, "Image")) stop("'x' must be an Image object")
	if (length(offset) == 1) offset <- rep(offset, 2)
	if (length(adj) == 1) adj <- rep(adj, 2)
	nf <- numberOfFrames(x, type = "render")
	if (missing(nx)) nx <- ceiling(sqrt(nf))
	ny <- ceiling(nf/nx)
	ss <- seq_len(nf)
	dm <- dim(x)
	xx <- dm[1]*((ss - 1)%%nx + offset[1])
	yy <- dm[2]*((ss - 1)%/%nx + offset[2])
	if (missing(labels))
		labels <- as.character(ss)
	else
		labels <- as.graphicsAnnot(labels)

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
	if (asImage == FALSE)
		return(found)
	else if (combine == TRUE)
		return(combine(getFrames(x, found, type = "render")))
	else
		return(getFrames(x, found, type = "render"))
}
