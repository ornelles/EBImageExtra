#' Get Region of Interest
#' 
#' Get a rectangular region of interest from an image.
#' 
#' @param img An \code{Image} object
#' @param x,y \code{x,y} coordinates of one or both corners of
#'   rectangular selection \emph{or} the location of the center of the
#'   rectangular selection \emph{or} a list of length 2 specifying
#'   the corners of the selection (see details) 
#' @param x2,y2 optional second pair of x and y coordinates when
#'   needed to specify the other corner of the rectangular selection 
#' @param w,h optional width and height of the rectangular selection;
#'   required if \code{x2,y2} are missing 
#' @param asCorner \code{logical} value indicating whether to use the
#'   location of the point \code{x,y} as the corner of the selection identified
#'   in \code{which.corner} or as the center of the selection (default)
#' @param which.corner a character vector of length 1 identifying the
#'   corner of the rectangle specified by \code{x,y};
#'   applies only if \code{asCorner = TRUE}
#' @param pch plotting character used by \code{locator}, default of 3 (cross)
#'   when interacting with the image
#' @param col color for plotting character used by \code{locator}, default of
#'   \code{"magenta"}; use \code{NA} for no plotting character
#' @param border border color of rectangle if \code{markup = TRUE}; if not
#'   specified, the value for \code{col} is used
#' @param lwd line width of rectangle if \code{markup = TRUE}
#' @param markup \code{logical} value (default of \code{TRUE}) indicating
#'   that the image should be redrawn with the selection outline drawn
#' @param asImage \code{logical} value (default of \code{TRUE}) indicating
#'   whether the function returns the region of interest as an \code{Image}
#'   or a list containing the \code{x,y} coordinates of the region of interest
#' 
#' @seealso addInset; other stuff
#' 
#' @details
#' This permits the interactive or programmatic definition of a rectangular
#' region of interest in an image by either specifying
#' coordinates that define the rectangle or by invoking \code{\link{locator}}
#' to allow the user to select the region of interest. The region of interest
#' will be trimmed to the dimensions allowed by the original image.
#' Options allow specifying the rectangle either by
#' the center or corner(s) as describe below. The first two options require
#' no interaction with the user and only produce an image if
#' \code{markup = TRUE}. The second two options require interaction with the
#' user. In all cases, the selection coordinates are adjusted to conform to
#' the dimensions of the image. 
#' 
#' \describe{
#'   \item{List of Length 2}{If \code{x} is a \code{list} of length 2, it
#'     is assumed to hold opposite corners of the rectangular selection. This
#'     is the typical result of a call to \code{locator(2)} after having
#'     plotted the image.}
#'   \item{Two Points}{If values are provided for each of \code{x,y} and
#'     \code{x2,y2}, these are treated as opposite corners of
#'     the rectangular selection.}
#'   \item{One Point (with width and height)}{If only two values are provided for
#'     \code{x,y} and \code{x2,y2}, they will be assigned to \code{x,y}. Values
#'     for \code{w,h} must be provided as named arguments for the width and
#'     height of the rectangular selection. The point \code{x,y} is
#'     interpreted as \emph{either} the center (if \code{asCorner} is
#'     \code{FALSE}) \emph{or} the corner of the selection if 
#'     \code{asCorner} is \code{FALSE} (the default). If
#'     \code{asCorner = TRUE}, the position of the corner is determined
#'     by the argument \code{which.corner} which can be one of \code{"bottomleft", 
#'     "topleft", "bottomright",} or \code{"topright"}.}
#'   \item{No Points (choose opposite corners)}{If all of \code{x,y, x2,y2, w,h}
#'     are missing, \code{\link{locator}} will be called to let the user to
#'     select two points that define opposite corners of the rectangular selection.}
#' }
#'
#' If \code{asImage = FALSE}, the returned value can be used as to extract the
#' region of interest from the image or related images by code such as:
#' \preformatted{
#'    pp <- getROI(img, asImage = FALSE)
#'    pp <- lapply(pp, function(v) seq.int(v[1], v[2]))
#'    idx <- lapply(dim(img), seq.int)
#'    idx[1:2] <- pp
#'    cropped <- do.call("[", c(list(img), idx))
#' }
#' 
#' @return
#' Either an image an \code{Image} object corresponding to the region of
#' interest in the image \emph{or} a list of length two containing the
#' \code{x,y} coordinates of the region of interes.
#' 
#' @examples
#' # Example using fixed width and height to retrieve image
#'   lighthouse <- readImage(system.file("inst", "extdata", "lighthouse.jpg", package="EBImageExtra"))
#' # Get region of interest as an image
#'   roi <- getROI(lighthouse, 515, 275, w = 200, h = 300)
#'   plot(roi)
#' # Get region of interest as a pair of points, anchored by top left corner
#'   corners <- getROI(lighthouse, 515, 275, w = 200, h = 300, asCorner = TRUE,
#'      which.corner = "topleft", asImage = FALSE)
#'   print(corners)
#'
#' @import EBImage
#' 
#' @export
#' 
getROI <- function(img, x, y, x2, y2, w, h, asCorner = FALSE,
  which.corner = c("bottomleft", "topleft", "bottomright", "topright"),
  pch = 3, col = "magenta", border = col, lwd = 2, markup = TRUE,
  asImage = TRUE)
{
  if (!require("EBImage")) stop("This requires the EBImage package")

  if(!is(img, "Image")) stop("'img' must be an Image object")
  dm <- dim(img)[1:2]
  which.corner <- sub("upper", "top", which.corner)
  which.corner <- sub("lower", "bottom", which.corner)
  which.corner = match.arg(which.corner)

# list of flags for missing arguments
  F <- c(missing(x), missing(y), missing(x2), missing(y2),
      missing(w), missing(h))

# process based on the arguments
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
      p <- locator(1, type = "p", pch = pch, col = col)
    }
    else # 'x' and 'y' provided as the one point
      p <- list(x = x, y = y)
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
  if (markup == TRUE) {
    plot(img)
    rect(pp$x[1], pp$y[2], pp$x[2], pp$y[1], border = border, lwd = lwd)
  }

# return image or adjusted corners of roi
  if (asImage == TRUE) {
    pp <- lapply(pp, function(v) seq.int(v[1], v[2]))
    idx <- lapply(dim(img), seq.int)
    idx[1:2] <- pp
    ans <- do.call("[", c(list(img), idx))
  }
  else
    ans <- pp
  return(ans)
}
