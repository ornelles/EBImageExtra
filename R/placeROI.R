#' Place Image (ROI) into Another Image
#' 
#' Place a scaled image into another image as an inset
#' 
#' @param roi the smaller \code{Image} object to be placed in
#' \code{img} \emph{or} the filename (as \code{character} vector) of
#'   such an image
#' @param img the larger \code{Image} object to receive the roi
#' @param position a character string or integer in 1 to 9 indicating
#'   the position of the inset; If \code{missing}, \code{\link{locator}}
#'   will be used to determine where to place the inset; see Details
#' @param frac \code{numeric} value between 0 and 1 for the fractional
#'   width occupied by the inset; the default values of \code{NULL} uses the 
#'   value in \code{frac.default}
#' @param mag optional magnification factor for inset; if \code{NULL}
#'   (default), it will be determined by \code{frac}
#' @param frac.default default fractional width for the inset (\code{1/3})
#' @param showImage \code{logical} value indicated whether to plot the 
#'   image with inset; default (\code{TRUE})
#' 
#' @details
#' The image to be placed as an inset (\code{roi}) will be scaled 
#' and placed in \code{img} at the location specified by \code{position}. 
#' This argument can be one of "topleft", "top", "topright", "left", 
#' "center", "right", "bottomleft", "bottom", and "bottomright". The code 
#' will also accept the integers 1 to 9 corresponding to these
#' respective positions. If \code{position} is missing,
#' \code{\link{locator}} will be called to determine where (among the
#' nine choices) to place the inset. 
#' 
#' The image will be scaled according to either \code{frac} or \code{mag} 
#' where \code{frac} indicates the fractional width of the image to be 
#' occupied by the scaled inset. Alternatively, the magnification can be 
#' specified with \code{mag}, typically a number greater than 1. Values that
#'  create an inset greater than the dimensions of the \code{img} argument 
#' will cause an error. 
#' 
#' If \code{showImage = TRUE}, the image with inset will be plotted. In all
#' cases, the modified image will be invisibly returned. 
#' 
#' @seealso
#' \code{\link{getROI}}; \code{\link{frameROI}}
#' 
#' @examples
#' # To be done...
#' 
#' @return
#' An \code{Image} of the same dimensions as the second argument
#' (\code{img}) with \code{roi} inserted after appropriate scaling. 
#' 
#' @import EBImage
#' 
#' @export
#' 
placeROI <- function(roi, img, position, frac = NULL, mag = NULL,
	frac.default = 1/3, showImage = TRUE)
{
# Check arguments
	if(!is(roi, "Image"))
		stop ("'roi' must be an Image object")
	if (is.character(img)) {
		if (file.exists(img[1]))
			img <- readImage(img)
		else
			stop("'img' is not a valid image filename")
	}
	else if (!is(img, "Image"))
		stop("'img' must be an Image object or valid filename")

# Harmonize colorMode
	cmx <- colorMode(img)
	cm2 <- colorMode(roi)
	if (cmx != cm2) warning("grayscale image promoted to RGB")
	if (cmx > cm2) roi <- toRGB(roi)
	else if (cm2 > cmx) img <- toRGB(img)

# Common variables
	choices <- c("topleft", "top", "topright", "left", "center", "right",
		"bottomleft", "bottom", "bottomright")
	choices <- factor(choices, levels = choices)

	dm.img <- dim(img)[1:2]
	dm.roi <- dim(roi)[1:2]
	cmx <- colorMode(img)

# Determine magnification 'mag' from fractional size 'frac'
	if (is.null(frac) & is.null(mag)) { # given neither 'mag' nor 'frac'
		frac <- frac.default
		mag <- frac * dm.img[1]/dm.roi[1]
	}
	else if (is.null(frac) & !is.null(mag)) { # given 'mag'
		frac <- mag * dm.roi / dm.img
		if (any(frac[1:2] >= 1))
			stop("inset will exceed size of image with mag = ", signif(mag, 2))
		frac <- frac[1]
	}
	else if (!is.null(frac) & is.null(mag)) { # given 'frac'
		if (frac > 1 | frac < 0)
			stop("'frac' must be between 0 and 1")
		frac <- frac
		mag <- frac * dm.img[1]/dm.roi[1]
	}
	else if (!is.null(frac) & !is.null(mag)) { # given both
		if (signif(mag, 3) != signif(frac * dm.img[1]/dm.roi[1]))
			stop("incompatible 'mag' and 'frac' values provided, use one or the other")
		else
			mag <- mag
	}

# Resize inset by mag and recalculate dm.roi
  roi <- resize(roi, w = mag * dm.roi[1])
  dm.roi <- dim(roi)[1:2]

# Determine position for inset from 'position' and 'choices'
	if (!missing(position)) {
			if (is(position, "character")) {
				position <- sub("upper", "top", position)
				position <- sub("lower", "bottom", position)
				position <- match.arg(position, choices)
			}
			else if (is.numeric(position) && position > 0 && position <= length(choices))
				position <- levels(choices)[position]
			else
				stop("'position' must be a character or integer in 1 through 9")
	}
	else {
		nx <- 3 # three "zones" across and down
		vx <- seq(0, dm.img[1] + 1, length = nx + 1)
		vy <- seq(0, dm.img[2] + 1, length = nx + 1)
		i <- 0
		cat("Click near the corner or side to position the inset\n")
		flush.console()
		while (!i %in% seq_len(nx*nx)) {
			p <- locator(1)
			ix <- findInterval(p$x, vx)
			iy <- findInterval(p$y, vy)
			i <- ix + nx*(iy - 1)
		}
		position <- levels(choices)[i]
	}

# Calculate translation adjustment for given position
	pad <- dm.img - dm.roi
	offset <- switch(as.character(position),
		"topleft" = c(0, 0), "top" = c(0.5, 0), "topright" = c(1, 0),
		"left" = c(0, 0.5), "center" = c(0.5, 0.5), "right" = c(1, 0.5),
		"bottomleft" = c(0, 1), "bottom" = c(0.5, 1), "bottomright" = c(1, 1))
	
# Assemble image by creating masks and using array math
	mask <- Image("black", dim = dm.roi, colormode = 2)
	mask <- translate(mask, offset * pad, output.dim = dm.img, bg.col = "white")
	roi <- translate(roi, offset * pad, output.dim = dm.img, bg.col = "black")
	ans <- mask * img + roi
	if (showImage == TRUE)
		plot(ans)
	invisible(ans)
}
