#' Draw a Rectangular Frame within an Image
#' 
#' Draw a rectangular frame surrounding the region of
#' interest in an image.
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
#' @param showImage replot the image with border if \code{TRUE}
#' @param asCorner \code{logical} value to use the
#'   point \code{x,y} as the corner of the selection or as the center of
#'   the selection
#' @param which.corner identifies the
#'   corner of the rectangle specified by \code{x,y};
#'   applies only if \code{asCorner = TRUE}
#' @param lwd 'line' width of the border in pixels; note that this is
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
#' region of interest with borders specified by \code{sides}. The added
#' border will be drawn toward the interior of the region of interest. This
#' preserves the dimensions of the marked-up image. This function
#' differs from \code{\link{rect}} by using \code{lwd} in an atypical
#' manner. Here \code{lwd} refers to the width of the border in pixels.
#' This also differs from the base functions by directly changing pixels
#' in the image, in contrast to change the plot device. 
#' 
#' This region of interest can be specified with arguments or interactively. 
#' Options allow specifying the rectangle either by
#' the center or corner(s) as describe below. Options 1, 2 and 3 below require
#' no interaction with the user and will display the revised image
#' only if \code{showImage = TRUE}. Options 4 and 5 below require interaction
#' with the user. In all cases, the selected coordinates are adjusted to conform
#' to the dimensions of the image, which can override values in \code{w} or 
#' \code{h}.
#' 
#' \enumerate{
#'   \item{\strong{Object with \code{loc} slot}.} If \code{x} is an object
#'     of class \code{Roi}, the coordinates in the \code{loc} slot
#'     will be extracted and used. To add a border around the entire image,
#'     use the \code{\link{as.Roi}} function in as call such as
#'     \code{drawROI(img, as.Roi(img))}.
#'   \item{\strong{List}.} If \code{x} is a \code{list} of length 2, it
#'     is assumed to hold opposite corners of the rectangular selection. This
#'     could be the \code{loc} slot from a \code{Roi} object or the return
#'     value of \code{locator(2)}.
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
#'     by the argument \code{which.corner} which can be one of \code{"bottomleft", 
#'     "topleft", "topright",} or \code{"bottomright"}.
#'   \item{\strong{No Points} (choose opposite corners).} If all of \code{x,y, x2,y2, w,h}
#'     are missing, \code{\link{locator}} will be called to let the user to
#'     select two points that define opposite corners of the rectangular selection.
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
#' # Example of adding arbitrary "roi" highlights to one image
#'   img <- readImage(system.file("inst", "extdata", "lighthouse.jpg", package="EBImageExtra"))
#'   xc <- sample(50:dim(img)[1] - 50, 6)
#'   yc <- sample(50:dim(img)[2] - 50, 6)
#'   for (i in 1:6) img <- drawROI(img, xc[i], yc[i], w = 90, h = sample(60:120, 1))
#'   plot(img)
#'
#' # Example of adding a frame to the image itself
#'   drawROI(img, as.Roi(img), col = "red", lwd = 12, showImage = TRUE) 
#' 
#' @return Modified image with region of interest outlined.
#'
#' @import EBImage
#'
#' @export
#'
drawROI <- function(img, x, y, x2, y2, w, h, showImage,
	lwd = 2, col = "white", asCorner = FALSE,
  which.corner = c("bottomleft", "topleft", "topright", "bottomright"),
  sides = 1:4, pch = 3, col.pch = col)
{
  if (!is(img, "Image"))
    stop("'img' must be an Image object")
  if (is(col, "numeric"))
    col <- palette()[col]
  dm <- dim(img)[1:2]
  
# vector of flags for missing arguments
  F <- c(missing(x),missing(y),missing(x2),missing(y2),missing(w),missing(h))

# need corners of roi from given arguments
	if (!F[1] && "loc" %in% slotNames(x)) { # given Roi object
		if (missing(showImage)) showImage <- FALSE
		pp <- attr(x, "loc")
	}
	else if (!any(F[1:4])) { # specified as x, y, x2, y2
		if (missing(showImage)) showImage <- FALSE
		pp <- list(x = sort(c(x, x2)), y = sort(c(y, y2)))
  }
  else if (all(F[1:6])) { # nothing provided except image
		if (missing(showImage)) showImage <- TRUE
    plot(img)
    cat("Select opposite corners to define the region of interest\n")
    flush.console()
    pp <- locator(2, type = "p", pch = pch, col = col.pch)
    pp <- lapply(pp, sort)
  }
  else if (!F[1] & all(F[2:4])) { # only 'x', must be list of corners
		if (missing(showImage)) showImage <- FALSE
    if (is(x, "list") && length(x) == 2 && all(lengths(x) == 2))
      pp <- setNames(lapply(x, sort), c("x", "y"))
    else
      stop ("if only 'x' is provided, it must be a list of two points")
  }
  else if (!any(F[5:6])) { # 'w' and 'h' provided
    if (any(F[1:2])) { # need to get one point
			if (missing(showImage)) showImage <- TRUE
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
			if (missing(showImage)) showImage <- FALSE
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
  else if (!any(F[1:2])) { # only 'x' and 'y' provided
		if (missing(showImage)) showImage <- FALSE
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

# combine with solid colored image, replace appropriate pixesl in image
  mask <- Image(col, dim = dim(img)[1:2], colormode = colorMode(img))
  ans <- img * M + mask * !M
  if (showImage) plot(ans)
  invisible(ans)
}
