#' Get Image Inset
#' 
#' Select and extract a rectangular inset from an image.
#' 
#' @param img An \code{Image} object
#' @param x,y the x and y coordinates (in pixels) of one or both corners of
#'   rectangular selection or the location of the center of this selection
#' @param x2,y2 optional second pair of x and y coordinates (in pixels) when
#'   needed to specify the other corner of the rectangular selection 
#' @param w,h optional width and height of the rectangular selection (in
#'   pixels); required if \code{x2,y2} are missing 
#' @param bycenter \code{logical}. If \code{TRUE}, the default, the
#'   location of the single \code{x,y} point is used as the
#'   \code{"center"} of the selection. If \code{FALSE}, the point corresponds
#'   to the corner of the selection named in \code{which.corner}
#' @param which.corner a character vector of length 1 identifying the
#'   corner of the rectangle specified by \code{x,y};
#'   applies only if \code{bycenter = FALSE}
#' @param pch plotting character used by \code{locator}, default of 3 (cross)
#' @param col color for plotting character used by \code{locator}, default of
#'   \code{"magenta"}; use \code{NA} for no plotting character
#' @param border border color of rectangle if \code{markup = TRUE}; if not
#'   specified, the value for \code{col} is used
#' @param lwd line width of rectangle if \code{markup = TRUE}
#' @param newplot \code{logical} value indicating if a new image should be
#'   plotted first
#' @param markup \code{logical} value indicating if the image should be
#'   redrawn with the outline drawn on the image
#' 
#' @seealso addInset; other stuff
#' 
#' @details
#' This allows extraction of a rectangular image inset by either specifying
#' coordinates that define the rectangle or by invoking \code{\link{locator}}
#' to allow the user to interactively select the inset. The \code{Image} inset
#' will be trimmed to the dimensions allowed by the original image and
#' invisibly returned. Options allow specifying the rectangle either by the
#' center or corner(s) as describe below. 
#' 
#' \describe{
#'   \item{A List of Length 2}{If \code{x} is a \code{list} of length 2, it
#'     is assumed to hold opposite corners of the rectangular selection. This
#'     is the typical result of a call to \code{locator(2)} with the image.}
#'   \item{Two Points}{If values are provided for each of \code{x,y} and
#'     \code{x2,y2}, these are treated as opposite corners of
#'     the rectangular selection.}
#'   \item{One Point (with width and height)}{If only two values are provided for
#'     \code{x,y} and \code{x2,y2}, they will be assigned to \code{x,y}. Values
#'     for \code{w,h} must be provided as named arguments as the width and
#'     height of the rectangular selection. The point \code{x,y} is
#'     interpreted as \emph{either} the center (\code{method="center"})
#'     \emph{or} the corner (\code{method="corner"}) of the selection. If
#'     \code{method="corner"} applies, the position of the corner is determined
#'     by the argument \code{which.corner} as one of \code{"bottomleft", 
#'     "topleft", "bottomright",} or \code{"topright"}.}
#'   \item{No Points (choose opposite corners)}{If all of \code{x,y,x2,y2,w,h}
#'     are missing, \code{locator} will allow the user to select two points
#'     that define opposing corners of the rectangular selection.}
#' }
#' 
#' @return
#' An \code{Image} object corresponding to the cropped image representing
#' the selected inset.
#' 
#' @examples
#' # example from EBImage package
#'   birds <- readImage(system.file("images", "sample-color.png", package="EBImage"))
#' # Use fixed width and height
#'   w <- 200
#'   h <- 240
#' # Example specifying one point for center with width and height
#'   ins1 <- getInset(birds, 160, 255, w = w, h = h)
#'   ins2 <- getInset(birds, 480, 200, w = w, h = h, newplot = FALSE)
#' # Show insets as a combined image
#'   plotStack(combine(ins1, ins2))
#' 
#' @import EBImage
#' 
#' @export
#' 
getInset <- function(img, x, y, x2, y2, w, h, method = c("center", "corner"),
	which.corner = c("bottomleft", "topleft", "bottomright", "topright"),
	pch = 3, col = "magenta", border = col, lwd = 2, markup = TRUE)
{
	if (!require("EBImage")) stop("This requires the EBImage package")

	if(!is(img, "Image")) stop("'img' must be an Image object")
	dm <- dim(img)[1:2]
	method <- match.arg(method)
	which.corner = match.arg(which.corner)

# list of flags for missing arguments
	F <- c(missing(x), missing(y), missing(x2), missing(y2),
			missing(w), missing(h))

#	process based on the arguments
	if (all(F[1:6])) { # nothing provided except image
		plot(img)
		pp <- locator(2, type = "p", pch = pch, col = col)
		pp <- lapply(pp, sort)
	}
	else if (!any(F[1:4])) {# inset specified, done
		pp <- list(x = sort(c(x, x2)), y = sort(c(y, y2)))
	}
	else if (!F[1] & all(F[2:4])) { # only 'x', must be list of corners
		if (is(x, "list") && length(x) == 2 && all(lengths(x) == 2))
			pp <- setNames(lapply(x, sort), c("x", "y"))
		else
			stop ("if only 'x' is provided, it must be a list of two points")
	}
	else if (!any(F[5:6])) { # 'w' and 'h' provided
		if (any(F[1:2])) { # need to get one point
			plot(img)
			pp <- locator(1, type = "p", pch = pch, col = col)
		}
		else # 'x' and 'y' provided as the one point
			pp <- list(x = x, y = y)
	# adjust the one point
		if (method == "corner") {
			if (which.corner == "bottomleft")
				pp <- list(x = c(pp$x, pp$x + w), y = c(pp$y, pp$y - h))
			else if (which.corner == "topleft")
				pp <- list(x = c(pp$x, pp$x + w), y = c(pp$y, pp$y + h))
			else if (which.corner == "bottomright")
				pp <- list(x = c(pp$x - w, pp$x), y = c(pp$y, pp$y - h))
			else # which.corner == "topright"
				pp <- list(x = c(pp$x - w, pp$x), y = c(pp$y, pp$y + h))
		}
		else # method == "center"
			pp <- list(x = pp$x + c(-1, 1)*w/2, y = pp$y + c(-1, 1)*h/2)
	}
	else
		stop("need 'w,h' values if only one pair of 'x,y' values is provided")

# adjust the inset coordinates to the limits of the image
	pp$x <- pmax(1, pmin(pp$x, dm[1]))
	pp$y <- pmax(1, pmin(pp$y, dm[2]))
	pp <- lapply(pp, round)
	pp <- lapply(pp, sort)

# highlight the inset in the image
	if (markup == TRUE) {
		plot(img)
		rect(pp$x[1], pp$y[2], pp$x[2], pp$y[1], border = border, lwd = lwd)
	}

# create coordinates to extract inset
	pp <- lapply(pp, function(v) seq.int(v[1], v[2]))
	if (colorMode(img) == Grayscale)
		ans <- img[pp$x, pp$y]
	else if (colorMode(img) == Color)
		ans <-img[pp$x, pp$y, ]
	else
		stop("what kind of image was this?")
	invisible(ans)
}
