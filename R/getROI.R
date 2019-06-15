#' Get Region of Interest
#' 
#' Get a rectangular region of interest from an image.
#' 
#' @param img An \code{Image} object
#' @param x,y \code{x,y} coordinates of one or both corners of
#'   rectangular selection \emph{or} the center of the
#'   rectangular selection \emph{or} a list of
#'   corners of the selection (see details in Selecting the ROI) 
#' @param x2,y2 optional second pair of x and y coordinates when
#'   needed to specify the other corner of the rectangular selection 
#' @param w,h optional width and height of the rectangular selection
#'   in pixels; required if \code{x2,y2} are missing 
#' @param markup \code{logical} value to redraw the image the selection
#'   outlined by \code{\link{drawROI}}; if \code{missing} or \code{NULL}
#'   \code{markup} will set to \code{TRUE} if the user interacts 
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
#' @param border border color of rectangle if \code{markup = TRUE}; if not
#'   specified, the value for \code{col} is used
#' @param lwd line width of rectangle if \code{markup = TRUE}
#' 
#' @seealso
#' \code{\link{putROI}} to place an ROI with scaling;
#' \code{\link{drawROI}} to draw a frame \emph{within} an image;
#' \code{\link{frameROI}} to draw a frame \emph{about} an image;
#' \code{\link{insertROI}} as a convenience function that
#'   combines calls to \code{getROI}, \code{putROI}
#'   and \code{drawROI} to place a framed inset in an image.
#' 
#' @section Selecting the ROI:
#' A rectangular region of interest (ROI) can be selected programmatically
#' or interactively. The ROI is defined by the position of the lower left
#' and upper right    coordinates of the rectangle in pixels   (actually
#' either pair of corners on the diagonal is adequate.) The pair of points
#' can be specified several means. The function will accept four values
#' \code{x,y}, and \code{x2,y2} or a list of the two points. Without these
#' arguments, the function     invokes \code{\link{locator}} to allow the
#' user to define the ROI. The ROI will be trimmed if necessary to the
#' dimensions allowed by the original image. Options allow specifying the
#' rectangle either by the center or corner(s) as describe below. The first
#' two options  require no interaction with the user and only produce an
#' image if \code{markup = TRUE}. Options 3 and 4 below require interaction
#' with the user and produce an image if \code{markup} is \code{TRUE} or
#' was not provided. The returned object is an image with additional
#' \code{class} and \code{attribute} as described in the section below.
#' 
#' \enumerate{
#'   \item{\strong{List}.} If \code{x} is a \code{list} of length 2, it
#'     is assumed to hold opposite corners of the rectangular selection. This
#'     is the typical result of a call to \code{locator(2)} after having
#'     plotted the image.
#'   \item{\strong{Two Points}.} If values are provided for each of \code{x,y} and
#'     \code{x2,y2}, these are treated as opposite corners of
#'     the rectangular selection.
#'   \item{\strong{One Point} (with width and height).} If only two values are
#'     provided for \code{x,y} and \code{x2,y2}, they will be assigned to
#'     \code{x,y}. In this case, values for \code{w,h} must be provided as
#'     named arguments for the width and height of the rectangular selection.
#'     The point \code{x,y} is interpreted as \emph{either} the center
#'     (\code{asCorner=FALSE})
#'     \emph{or} the corner of the selection (\code{asCorner=TRUE}). If
#'     \code{asCorner = TRUE}, the position of the corner is determined
#'     by the argument \code{which.corner} as one of \code{"bottomleft", 
#'     "topleft", "bottomright",} or \code{"topright"}.
#'   \item{\strong{No Points} (choose opposite corners).} If all of \code{x,y, x2,y2, w,h}
#'     are missing, \code{\link{locator}} will be called to let the user to
#'     select two points that define opposite corners of the rectangular selection.
#' }
#'
#' @section Class "roi" and "loc" attribute:
#' \code{EBImage} uses the \code{Image} class to store and process images.
#' A region of interest is an \code{Image} object with the
#' additional class of "\code{roi}" and an additional attribute. The new
#' attribute "\code{loc}"
#' holds the location of the region of interest as \code{roi =
#' list(x = c(x, x2), y = c(y, y2))}.  The attribute \code{"loc"} can be
#' used to extract the equivalent region of interest from a related image
#' (\code{img2}) in the following manner. 
#' \preformatted{
#'  x <- getROI(img1)
#'  pp <- attr(x, "loc") # or x@loc or slot(ins, "loc")
#'  y <- getROI(img2, pp)
#' }
#' 
#' @return
#' The region of interest as an \code{Image} with the
#' added class of "\code{roi}" and the attribute "\code{loc}"
#' holding the location of the region of interest.
#' 
#' @examples
#' # Example using fixed width and height to retrieve image
#'   lighthouse <- readImage(system.file("inst", "extdata", "lighthouse.jpg", package="EBImageExtra"))
#'
#' # Get region of interest, anchored by center
#'   ans1 <- getROI(lighthouse, 515, 315, w = 200, h = 150)
#'   print(ans@loc) # one way to extract 'loc'
#'
#' # Get region of interest, anchored by top left corner
#'   ans2 <- getROI(lighthouse, 515, 315, w = 200, h = 150, asCorner = TRUE)
#'   print(attr(ans, "loc")) # better way of extracting 'loc'
#'   plotStack(combine(ans1, ans2), nx = 1)
#'
#' @import EBImage
#' 
#' @export
#' 
getROI <- function(img, x, y, x2, y2, w, h, markup, asCorner = FALSE,
  which.corner = c("bottomleft", "topleft", "bottomright", "topright"),
  pch = 3, col = "magenta", border = col, lwd = 2)
{
  if (!require("EBImage")) stop("This requires the EBImage package")

  if(!is(img, "Image")) stop("'img' must be an Image object")
  dm <- dim(img)[1:2]
  which.corner <- sub("upper", "top", which.corner)
  which.corner <- sub("lower", "bottom", which.corner)
  which.corner = match.arg(which.corner)

# process missing arguments
  F <- c(missing(x), missing(y), missing(x2), missing(y2),
      missing(w), missing(h))
	if (missing(markup)) markup <- NULL

# process based on the arguments
  if (all(F[1:6])) { # nothing provided except image
    plot(img)
    markup <- if (is.null(markup)) TRUE else markup
    msg <- "Select opposite corners of the region of interest"
    cat(msg, "\n")
    flush.console()
    pp <- locator(2, type = "p", pch = pch, col = col)
    pp <- lapply(pp, sort)
  }
  else if (!any(F[1:4])) {# inset specified, done
    markup <- if (is.null(markup)) FALSE else markup
    pp <- list(x = sort(c(x, x2)), y = sort(c(y, y2)))
  }
  else if (!F[1] & all(F[2:4])) { # only 'x', must be list of corners
    markup <- if (is.null(markup)) FALSE else markup
    if (is(x, "list") && length(x) == 2 && all(lengths(x) == 2))
      pp <- setNames(lapply(x, sort), c("x", "y"))
    else
      stop ("if only 'x' is provided, it must be a list of two points")
  }
  else if (!any(F[5:6])) { # 'w' and 'h' provided
    if (any(F[1:2])) { # need to get one point
      markup <- if (is.null(markup)) TRUE else markup
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
			markup <- if (is.null(markup)) FALSE else markup
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
      pp <- list(x = p$x + c(-1, 1)*w/2, y = p$y + c(-1, 1)*h/2)
  }
  else # 'w' and 'h' NOT provided
    stop("need 'w,h' values if only one pair of 'x,y' values is provided")

# adjust the inset coordinates to the limits of the image
  pp$x <- pmax(1, pmin(pp$x, dm[1]))
  pp$y <- pmax(1, pmin(pp$y, dm[2]))
  pp <- lapply(pp, round)
  pp <- lapply(pp, sort)

# highlight the inset in the image if requested
  if (is.null(markup)) stop("oops...screwed up code somewhere...")
  if (markup == TRUE) {
    plot(img)
    rect(pp$x[1], pp$y[2], pp$x[2], pp$y[1], border = border, lwd = lwd)
  }

# return image with added class and attribute
	loc <- pp # original coordinates
	pp <- lapply(pp, function(v) seq.int(v[1], v[2])) # expanded inset coordinates
  idx <- lapply(dim(img), seq.int) # expanded img coordinates
  idx[1:2] <- pp
  ans <- do.call("[", c(list(img), idx)) # replace 1st two dimensions
  return(Roi(ans, loc = loc))
}
