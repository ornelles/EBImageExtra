#' Draw a Rectangular Frame within an Image
#' 
#' Draw a rectangular frame surrounding the region of
#' interest in an image or the entire image.
#' 
#' @param img An \code{Image} object
#' @param x,y \code{x,y} coordinates of one or both corners of
#'   rectangular selection \emph{or} the location of the center of the
#'   rectangular selection \emph{or} a list specifying
#'   the corners of the selection \emph{or} an \code{Roi} object
#'   with a \code{slot} named "\code{loc}" (See Details) 
#' @param x2,y2 optional second pair of x and y coordinates when
#'   needed to specify the other corner of the rectangular selection 
#' @param w,h optional width and height of the rectangular selection;
#'   required if \code{x2,y2} are missing 
#' @param show replot the image after adding the border if \code{TRUE}
#' @param asCorner \code{logical} value to use the point \code{x,y} 
#'   as the corner of the selection instead of the center of
#'   the selection
#' @param which.corner identifies the corner of the rectangle specified
#'    by \code{x,y}; applies only if \code{asCorner = TRUE}
#' @param lwd line width of the border in pixels; note that this is
#'   \strong{not} the standard definition of \code{lwd}
#' @param col border color of the rectangle 
#' @param sides which side(s) are to include the border (1=bottom, 2=left,
#'   3=top, 4=right)
#' @param pch plotting character used by \code{\link{locator}} when interacting
#'   with the image
#' @param col.pch color for plotting character used by \code{\link{locator}}
#' 
#' @details
#' A rectangular border of \code{lwd} pixels will drawn around the
#' region of interest along the sides specified by \code{sides}. The added
#' border will be drawn toward the interior of the region of interest to
#' preserve the dimensions of the modified image. This function
#' differs from \code{\link{rect}} by using \code{lwd} in an atypical
#' manner--here \code{lwd} refers to the width of the border in pixels.
#' This also differs from the base functions by directly changing pixels
#' in the image, in contrast to changing the image on the plot device. 
#' 
#' This region of interest can be specified with arguments or interactively. 
#' Options allow specifying the rectangle either by
#' the center or corner(s) as describe below. Options 1 through 5 below require
#' no interaction with the user and will display the revised image
#' only if \code{show = TRUE}. Option 6 below requires interaction
#' with the user. In all cases, the selected coordinates are adjusted to conform
#' to the dimensions of the image, which can override values in \code{w} or 
#' \code{h}.
#' 
#' \enumerate{
#'   \item{\strong{Two Points}.} If values are provided for each of \code{x,y}
#'     and \code{x2,y2}, these are treated as opposite corners of
#'     the rectangular selection.
#'   \item{\strong{Object with \code{loc} slot}.} If \code{x} is an object
#'     of class \code{Roi}, the coordinates in the \code{loc} slot
#'     will be extracted and applied to the \code{img} argument.
#'   \item{\strong{List}.} If \code{x} is a \code{list} of length 2, it
#'     must hold opposite corners of the rectangular selection. This
#'     could be the \code{loc} slot from a \code{Roi} object or the result
#'     of a call to \code{locator(2)}.
#'   \item{\strong{No Points} (use entire image).} If all of
#'     \code{x,y, x2,y2, w,h} are missing, a border will be generated for
#'     the entire image.
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
#' }
#'
#' @seealso
#' \code{\link{getROI}} to get a region of interest from an image;
#' \code{\link{putROI}} to place an ROI with scaling;
#' \code{\link{insertROI}} as a convenience function that
#'   combines calls to \code{getROI}, \code{putROI}
#'   and \code{drawROI} to place a framed inset in an image.
#'
#' @examples
#' 
#' # Example of adding arbitrary 90 pixel x 90 pixel "roi" highlights to one image
#'   img <- readImage(system.file("extdata", "lighthouse.jpg", package="EBImageExtra"))
#'   xc <- sample(100:(dim(img)[1] - 100), 8)
#'   yc <- sample(100:(dim(img)[2] - 100), 8)
#'   for (i in 1:8) img <- drawROI(img, xc[i], yc[i], w = 90, h = 90)
#'   plot(img)
#'
#' # Example of adding a frame to the image itself
#'   drawROI(img, as.Roi(img), col = "red", lwd = 12, show = TRUE) 
#' 
#' @return Modified image with region of interest outlined.
#'
#' @import EBImage
#'
#' @export
#'
drawROI <- function(img, x, y, x2, y2, w, h, show,
	lwd = 2, col = "white", asCorner = FALSE,
  which.corner = c("bottomleft", "topleft", "topright", "bottomright"),
  sides = 1:4, pch = 3, col.pch = col)
{
  if (!is(img, "Image"))
    stop("'img' must be an Image object")
  if (is(col, "numeric"))
    col <- palette()[col]
  dm <- base::dim(img)[1:2]
  
# vector of flags for missing arguments
  F <- c(missing(x),missing(y),missing(x2),missing(y2),missing(w),missing(h))

# parse 'x' and remaining arguments to find corners of roi 
	if (!F[1] && "loc" %in% slotNames(x)) { # 'x' is an Roi object
		if (missing(show)) show <- FALSE
		pp <- attr(x, "loc")
	}
	else if (!any(F[1:4])) { # all x, y, x2, y2 are specified
		if (missing(show)) show <- FALSE
		pp <- list(x = sort(c(x, x2)), y = sort(c(y, y2)))
  }
  else if (all(F[1:6])) { # no arguments other than the image
		if (missing(show)) show <- FALSE
    pp <- list(x = c(1, dm[1]), y = c(1, dm[2]))
  }
  else if (!F[1] & all(F[2:4])) { # only 'x', must be list of corners
		if (missing(show)) show <- FALSE
    if (is(x, "list") && length(x) == 2 && all(lengths(x) == 2))
      pp <- setNames(lapply(x, sort), c("x", "y"))
    else
      stop ("if only 'x' is provided, it must be a list of two points")
  }
  else if (!any(F[5:6])) { # 'w' and 'h' provided
  	w <- ceiling(w - 1)
  	h <- ceiling(h - 1)
    if (any(F[1:2])) { # no 'x' and 'y', get one point
			if (missing(show)) show <- TRUE
      plot(img)
      if (asCorner)
        msg <- paste("Select the", which.corner, "corner of the region of interest")
      else
        msg <- paste("Select the center of the region of interest")
      cat(msg, "\n")
      flush.console()
      p <- locator(1, type = "p", pch = pch, col = col)
    }
    else {# 'x' and 'y' provided as the one point
			if (missing(show)) show <- FALSE
      p <- list(x = x, y = y)
		}
  # adjust the one point that goes with 'w' and 'h'
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
  else if (!any(F[1:2])) { # values for 'x' and 'y' without 'w' and 'h'
		if (missing(show)) show <- FALSE
    if (length(x) == 2 && length(y) == 2)
      pp <- list(x = x, y = y)
    else
			stop("need 'w,h' values if only one pair of 'x,y' values is provided")
  }
  else # 'w' and 'h' NOT provided
			stop("I have NO idea how we got here...")

# adjust roi coordinates to the limits of the image
  pp$x <- pmax(1, pmin(pp$x, dm[1]))
  pp$y <- pmax(1, pmin(pp$y, dm[2]))
  pp <- lapply(pp, round)
  pp <- lapply(pp, sort)

# identify borders and check new dimension
  nx <- sum(c(2, 4) %in% sides)
  ny <- sum(c(1, 3) %in% sides)
  dm2 <- dm - c(nx, ny)*lwd
  if (any(dm2 < 1))
    stop("'lwd' is too large for the size of the image")

# create two list of border coordinates
  pp1 <- lapply(pp, function(v) list(seq(v[1], len = lwd),
    seq(v[2] - lwd + 1, len = lwd)))
  pp2 <- lapply(pp, function(v) v[1]:v[2])

# create a logical mask that define each side of the border
  m <- Image(TRUE, dim(img)[1:2])
  S1 <- col(m) %in% pp1$y[[2]] & row(m) %in% pp2$x
  S3 <- col(m) %in% pp1$y[[1]] & row(m) %in% pp2$x
  S2 <- row(m) %in% pp1$x[[1]] & col(m) %in% pp2$y
  S4 <- row(m) %in% pp1$x[[2]] & col(m) %in% pp2$y
  S <- list(S1, S2, S3, S4)

# paint border areas as FALSE (0) in the logical mask
  m[Reduce(`|`, S[1:4 %in% sides])] <- FALSE

# convert the logical mask to a de-facto binary image  
  if (colorMode(img) == Color)
    M <- abind(m, m, m, along = 3)
  else
    M <- m

# combine with solid colored image, replace appropriate pixels in image
  mask <- Image(col, dim = dim(img)[1:2], colormode = colorMode(img))
  ans <- img * M + mask * !M
  if (show) plot(ans)
  invisible(ans)
}
