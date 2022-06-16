#' High Pass Filter
#' 
#' Attenuate slowly varying signals in an \code{Image} object
#' 
#' @param x An \code{Image} object or array
#' @param sigma A numeric passed to \code{gblur} as the standard deviation
#'  of the Gaussian filter used for initial blurring. This should be
#'  approximately the minimum size of the feature to keep
#' @param min.value Numeric value for the minimum value returned by this
#'  function
#' 
#' @details
#' This function subtracts the blurred image from the original image and
#' adds the mean value of the original image. The resulting pixels are
#' trimmed so that the lowest value is `code{min.value}. 
#' 
#' @return
#' An image object of the same size with slowly varying signals removed.
#' 
#' @import EBImage
#' 
#' @export
#' 
hpf <- function(x, sigma = 21, min.value = 2^-12) {
	if (sigma == 0) return(x)
	xb <- EBImage::gblur(x, sigma)
	ans <- x - xb + mean(xb)
	ans[ans < min.value] <- min.value
	return(ans)
}
