#' Improved Circularity and Perimeter Calculation
#' @rdname circularity
#'
#' @param x An \code{Image} object mask with a single integer or binary
#' object to be analyzed.
#'
#' @details
#' These functions provide more precise measures of the circularity and
#' perimeter of an integer or binary object than the algorithm in
#'  \code{\link[EBImage]{computeFeatures}}. Values are computed
#' by the C++ code (\code{circularity_cpp} and \code{perimeter_cpp}).
#' Because of digitization, the circularity can be greater
#' than 1. This function truncates values greater than 1. Also,
#' line-like objects with no interior pixels return an inappropriately
#' high value for the circularity. The perimeter for such objects is
#' calculated differently by treating each pixel as a unit square and
#' counting their exposed sides.
#'
#' @return Circularity as a value between 0 (line) and 1 (circle)
#'
#' @export
#'
circularity <- function(x)
{
	# ensure integer mask
		if (!is.Image(x) && colorMode(x) != 0 && !is.integer(x))
			stop("integer or binary mask is expected")
	# return circularity
		circularity_cpp(x)
}

#' @name perimeter
#' @rdname circularity
#'
#' @return The perimeter (in pixels) of a binary object
#'
#' @export
#'
perimeter <- function(x)
{
	# ensure integer mask
		if (!is.Image(x) && colorMode(x) != 0 && !is.integer(x))
			stop("integer or binary mask is expected")
	# return perimeter
		perimeter_cpp(x)
}
