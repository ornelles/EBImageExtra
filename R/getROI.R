#' Get Region of Interest
#' 
#' Get a rectangular region of interest from an image.
#' 
#' @param img An \code{Image} object
#' @param x,y \code{x,y} coordinates of one or both corners of
#'   rectangular selection \emph{or} the center of the
#'   rectangular selection \emph{or} a list of
#'   corners of the selection \emph{or} an object of \code{class}
#'   \code{"Roi"} with the \code{@loc} slot containing the 
#'   desired coordinates (see details in \strong{Selecting the ROI} below) 
#' @param x2,y2 optional second pair of x and y coordinates when
#'   needed to specify the other corner of the rectangular selection 
#' @param w,h optional width and height of the rectangular selection
#'   in pixels; required if \code{x2,y2} are missing 
#' @param show \code{logical} value to redraw the image the selection
#'   outlined by \code{\link{drawROI}}; if \code{missing} or \code{NULL}
#'   \code{show} will set to \code{TRUE} if the user interacts 
#'   with the image and \code{FALSE} if no interaction is required
#' @param asCorner \code{logical} value to use the
#'   point \code{x,y} as the corner of the selection or as the center of
#'   the selection
#' @param which.corner identifies the
#'   corner of the rectangle specified by \code{x,y};
#'   applies only if \code{asCorner = TRUE}
#' @param pch plotting character used by \code{link{locator}} to
#'    indicate mouse clicks, the default value of 3 shows a small cross
#' @param col color for plotting character used by \code{link{locator}};
#'   use \code{NA} for no plotting character
#' @param border border color of rectangle if \code{show = TRUE}; if not
#'   specified, the value for \code{col} is used
#' @param lwd line width of rectangle if \code{show = TRUE}
#' 
#' @seealso
#' \code{\link{putROI}} to place an ROI with scaling and
#' \code{\link{drawROI}} to draw a frame \emph{within} an image.
#' 
#' @section Selecting the ROI:
#'
#' A rectangular region of interest (ROI) can be selected programmatically
#' or interactively. The ROI is defined by a pair of points (in pixels)
#' that define a diagonal of the region of interest. The pair of points
#' can be specified by several means. If \code{x} is a previously defined ROI,
#' the coordinates of this ROI will be used to get the ROI. Otherwise, 
#' the function expected four values for specifying the corner of the ROI as
#' \code{x,y}, and \code{x2,y2} or a list of the two points. Without these
#' arguments, the function invokes \code{\link{locator}} to allow the
#' user to define the ROI. The ROI will be trimmed to the
#' dimensions allowed by the original image. The rectangle can be specified
#' either by the center or corner(s) as describe below. The first
#' four options require no interaction with the user and only produce an
#' image if \code{show = TRUE}. Options 5 and 6 below require interaction
#' with the user and produce an image if \code{show} is missing or if
#' \code{show = TRUE}. The returned object is an \code{Image} with
#' an additional \code{class} and \code{slot} as described in the below.
#' 
#' \enumerate{
#'   \item{\strong{Roi}.} If \code{x} is an object of \code{class = "Roi"},
#'     the corners of the selection will be the value in the \code{@loc} slot.
#'   \item{\strong{List}.} If \code{x} is a \code{list} of length 2, it
#'     is assumed to hold opposite corners of the rectangular selection. This
#'     is the typical result of a call to \code{locator(2)} after having
#'     plotted the image.
#'   \item{\strong{Two Points}.} If values are provided for each of \code{x,y} and
#'     \code{x2,y2}, these are treated as opposite corners of
#'     the rectangular selection.
#'   \item{\strong{One Point} (with width and height).} A single value
#'     can be provided for each of \code{x,y} with named values for
#'     for \code{w,h} as the width and height of the rectangular selection.
#'     The point \code{x,y} is interpreted as \emph{either} the center
#'     (\code{asCorner = FALSE}) \emph{or} the corner of the selection
#'     (\code{asCorner = TRUE}). If \code{asCorner = TRUE}, the position of the
#'     corner is determined by the argument \code{which.corner} which can be 
#'     one of \code{"bottomleft", "topleft", "topright",} or \code{"bottomright"}.
#'   \item{\strong{Only \code{width} and \code{height}}.} 
#'     If only \code{(w,h)} are provided
#'     as named arguments, \code{\link{locator}} will be used to interact
#'     with the user to identify the point needed to define the rectangular
#'     selection. The selected point is interpreted as \emph{either} the center
#'     (\code{asCorner = FALSE}) \emph{or} the corner of the selection
#'     (\code{asCorner = TRUE}) as described above.
#'   \item{\strong{No Points} (choose opposite corners).} If all of \code{x,y, x2,y2, w,h}
#'     are missing, \code{\link{locator}} will be called to let the user to
#'     select two points that define opposite corners of the rectangular selection.
#' }
#'
#' @section Class "Roi" and "loc" slot:
#'
#' \code{EBImage} uses the \code{Image} class to store and process images.
#' A region of interest is an \code{Image} object with the
#' additional class named "\code{Roi}" and an additional attribute using
#' the \code{slot} property of \code{S4} objects. The new \code{slot} or
#' attribute named "\code{loc}"
#' holds the location of the region of interest as \code{loc =
#' list(x = c(x,x2), y = c(y,y2))}.  The attribute \code{"loc"} can be
#' used to extract the equivalent region of interest from a related image
#' (\code{img2}) in the following manner. 
#' \preformatted{
#'  x <- getROI(img1)
#'  pp <- attr(x, "loc") ## or pp <- x@loc
#'  y <- getROI(img2, pp)
#' }
#' 
#' @return
#' The region of interest as an \code{Image} with the
#' added class of "\code{Roi}" and the slot "\code{loc}"
#' holding the location of the region of interest.
#' 
#' @examples
#' # Image from EBImage package
#'   birds <- readImage(system.file("images", "sample-color.png", package="EBImage"))
#' 
#' # Example specifying one point for center with fixed width and height
#'   roi1 <- getROI(birds, 160, 255, w = 200, h = 240)
#'   print(roi1@loc) # one way extract 'loc'
#'   roi2 <- getROI(birds, 480, 200, w = 200, h = 240)
#'   print(attr(roi2, "loc")) # an alternative way to extract 'loc'
#'
#' # Show insets as a combined image
#'   plotStack(combine(roi1, roi2))
#'
#' @import EBImage
#' 
#' @export
#' 
getROI <- function(img, x, y, x2, y2, w, h, show, asCorner = FALSE,
  which.corner = c("bottomleft", "topleft", "bottomright", "topright"),
  pch = 3, col = "magenta", border = col, lwd = 2)
{
# require EBImage
  if (!require("EBImage"))
		stop("This requires the EBImage package")

# require Image object
	if (missing(img)) {
		cat("Usage: getROI(img, x, [y, x2, y2, w, h, show, ...])",
				"  img is an Image",
				"  x, [y] specifies coordinates for the region of interest", sep = "\n")
		return(invisible(NULL))
	}
  if(!is(img, "Image")) stop("'img' must be an Image object")

# process missing arguments
  F <- c(missing(x), missing(y), missing(x2), missing(y2),
      missing(w), missing(h))
	if (missing(show)) show <- NULL

  dm <- base::dim(img)[1:2]
  which.corner <- sub("upper", "top", which.corner)
  which.corner <- sub("lower", "bottom", which.corner)
  which.corner = match.arg(which.corner)

# proceed based on the combination of arguments that are present
  if (all(F[1:6])) { # nothing provided except image
    plot(img)
    show <- if (is.null(show)) TRUE else show
    msg <- "Select opposite corners of the region of interest"
    cat(msg, "\n")
    flush.console()
    pp <- locator(2, type = "p", pch = pch, col = col)
    pp <- lapply(pp, sort)
  }
  else if (!any(F[1:4])) {# inset specified, done
    show <- if (is.null(show)) FALSE else show
    pp <- list(x = sort(c(x, x2)), y = sort(c(y, y2)))
  }
  else if (!F[1] & all(F[2:4])) { # only 'x', must be Roi or list of corners
    show <- if (is.null(show)) FALSE else show
		if (is(x, "Roi")) #inset specified with @loc slot
			pp <- x@loc
    else if (is(x, "list") && length(x) == 2 && all(lengths(x) == 2))
      pp <- setNames(lapply(x, sort), c("x", "y"))
    else
      stop ("if only 'x' is provided, it must be an Roi or a list of two points")
  }
  else if (!any(F[5:6])) { # 'w' and 'h' provided
  	w <- ceiling(w - 1)
  	h <- ceiling(h - 1)
    if (any(F[1:2])) { # need to get one point
      show <- if (is.null(show)) TRUE else show
      plot(img)
      if (asCorner)
        msg <- paste("Select the", which.corner, "corner of the region of interest")
      else
        msg <- paste("Select the center of the region of interest")
      cat(msg, "\n")
      flush.console()
      p <- locator(1, type = "p", pch = pch, col = col)
    }
    else { # 'x' and 'y' provided as the one point
			show <- if (is.null(show)) FALSE else show
      p <- list(x = x, y = y)
		}
  # adjust the one point
    if (asCorner == TRUE) {
      if (which.corner == "bottomleft")
        pp <- list(x = c(p$x, p$x + w), y = c(p$y, p$y - h))
      else if (which.corner == "topleft")
        pp <- list(x = c(p$x, p$x + w), y = c(p$y, p$y + h))
      else if (which.corner == "bottomright")
        pp <- list(x = c(p$x - w, p$x), y = c(p$y, p$y - h))
      else # which.corner == "topright"
        pp <- list(x = c(p$x - w, p$x), y = c(p$y, p$y + h))
    }
    else # asCorner == FALSE
      pp <- list(x = floor(p$x + c(-1, 1)*w/2), y = floor(p$y + c(-1, 1)*h/2))
  }
  else # 'w' and 'h' NOT provided
    stop("need 'w,h' values if only one pair of 'x,y' values is provided")

# adjust the inset coordinates to the limits of the image
  pp$x <- pmax(1, pmin(pp$x, dm[1]))
  pp$y <- pmax(1, pmin(pp$y, dm[2]))
  pp <- lapply(pp, round)
  pp <- lapply(pp, sort)

# highlight the inset in the image if requested
  if (is.null(show)) stop("oops...screwed up code somewhere...")
  if (show == TRUE) {
    plot(img)
    rect(pp$x[1], pp$y[2], pp$x[2], pp$y[1], border = border, lwd = lwd)
  }

# return image with added class and attribute
	loc <- pp # original coordinates
	pp <- lapply(pp, function(v) seq.int(v[1], v[2])) # expanded inset coordinates
  idx <- lapply(dim(img), seq.int) # expanded img coordinates
  idx[1:2] <- pp
  ans <- do.call("[", c(list(img), idx)) # replace 1st two dimensions
  return(EBImageExtra:::Roi(ans, loc = loc))
}
