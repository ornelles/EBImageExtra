#' Draw a Frame/Border around an Image
#'
#' Draw a rectangular frame about the border of an image along
#' the indicated sides
#'
#' @param img An \code{Image} object
#' @param lwd 'line' width of the border in pixels; note that this is
#'   \strong{not} the standard definition of \code{lwd}
#' @param col border color of the rectangle 
#' @param sides which side(s) are to include the border (1=bottom, 2=left,
#'   3=top, 4=right)
#' 
#' @details
#' A rectangular border of \code{lwd} pixels will drawn around the
#' image along the sides identified in \code{sides}. The added
#' border will be drawn toward the interior of the image. This
#' differs from \code{\link{rect}} by using \code{lwd} used in an atypical
#' manner. Here it defines the width of the border in pixels. This also differs
#' from the base functions by directly changing pixels in the image instead
#' of plotting to the graphics device. 
#' 
#' @examples
#'  lighthouse <- readImage(system.file("inst", "extdata", "lighthouse.jpg", package="EBImageExtra"))
#'  plot(frameROI(lighthouse)) # with defaults (not obvious)
#'  plot(frameROI(lighthouse, lwd = 36, col = "yellow", sides = c(1,2)))
#'
#' @seealso
#' \code{\link{getROI}} to get a region of interest from an image;
#' \code{\link{putROI}} to place an ROI with scaling;
#' \code{\link{drawROI}} to draw a frame \emph{within} an image;
#' \code{\link{insertROI}} as a convenience function that
#'   combines calls to \code{getROI}, \code{putROI}
#'   and \code{drawROI} to place a framed inset in an image.
#'
#' @return Modified image with border
#'
#' @import EBImage
#'
#' @export
#'
frameROI <- function(img, lwd = 2, col = "white", sides = 1:4)
{
  if (!is(img, "Image"))
    stop("'img' must be an Image object")
  if (is(col, "numeric"))
    col <- palette()[col]
  
# Allow for NA or NULL values in 'lwd'
  if (is(lwd, "NULL") || is.na(lwd))
    lwd <- 0

# Identify borders and be sure size is appropriate
  dm <- dim(img)[1:2]
  nx <- sum(c(2,4) %in% sides)
  ny <- sum(c(1,3) %in% sides)
  dm2 <- dm - c(nx, ny)*lwd
  if (any(dm2 < 1))
    stop("'lwd' is too large to use")

# extract corners of image 
  pp <- list(x = c(1, dm[1]), y = c(1, dm[2]))
  pp <- lapply(pp, sort)

# create two list of border coordinates
  pp1 <- lapply(pp, function(v) list(seq(v[1], len = lwd),
    seq(v[2] - lwd + 1, len = lwd)))
  pp2 <- lapply(pp, function(v) v[1]:v[2])

# create logical masks to define each side of the border
  m <- Image(TRUE, dim(img)[1:2])
  S1 <- col(m) %in% pp1$y[[2]] & row(m) %in% pp2$x
  S3 <- col(m) %in% pp1$y[[1]] & row(m) %in% pp2$x
  S2 <- row(m) %in% pp1$x[[1]] & col(m) %in% pp2$y
  S4 <- row(m) %in% pp1$x[[2]] & col(m) %in% pp2$y
  S <- list(S1, S2, S3, S4)

# paint border areas as FALSE (0) in the logical mask
  m[Reduce(`|`, S[1:4 %in% sides])] <- FALSE

# convert the logical mask to a de-facto binary image  
  if (colorMode(img) == Color) M <- abind(m, m, m, along = 3)
  else M <- m

# combine with solid colored image, replace appropriate pixesl in image
  mask <- Image(col, dim = dim(img)[1:2], colormode = colorMode(img))
  ans <- img * M + mask * !M
  return(ans)
}
