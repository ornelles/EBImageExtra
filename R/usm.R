#' Unsharp Mask Filter
#' 
#' Sharpen image with unsharp mask algorithm
#' 
#' @param x An \code{Image} object or array of \code{Image} objects
#' @param radius \code{Integer} value to determine Gaussian blur effect, 
#'  default value of 50
#' @param amount A \code{numeric} fraction between 0 and ~2 specifying
#'   the amount of blur to apply, default value of 0.2
#' @param inputRange value A optional \code{numeric} vector of 2 values handed to
#'   \code{\link[EBImage]{normalize}} to normalize the final image. If missing,
#'   the range of the input object will be used
#' 
#' @details
#' The \href{https://en.wikipedia.org/wiki/Unsharp_masking}{unsharp mask algorithm}
#' is applied. The value for \code{amount}
#' controls the magnitude of the contrast added at the edges.
#' The value for \code{radius} determines the size of the edges to be enhanced
#' where a smaller radius enhances details on a smaller scale. 
#' 
#' @return
#' A sharpened \code{Image} object of the same size as the input
#' 
#' @import EBImage
#' 
#' @examples
#' x <- readImage(system.file("images", "sample-color.png", package="EBImage"))
#' x <- x[156:265, 166:300,]
#' y <- usm(x, radius = 15, amount = 0.8)
#' plotStack(combine(x, y), label = c("Original", "Unsharp mask"), cex = 2)
#' 
#' @export
#' 
usm <- function(x, radius = 50, amount = 0.2, inputRange)
{
	lim <- ceiling(min(dim(x)[1:2])/7)
	if (radius < 1)
		stop("Radius must be greater than 1")
	if (radius > lim) {
		warning("Maximum recommend 'radius' of ", lim, " used")
		radius <- lim
	}
	sigma <- (radius - 1)/6
	if (missing(inputRange))
		inputRange <- c(0, max(x))
	xb <- EBImage::gblur(x, sigma)
	ans <- x + (x - xb) * amount
	return(EBImage::normalize(ans, inputRange = inputRange))
}
