#' Crop an Image to Visible Pixels
#'
#' Crop a grayscale or binary \code{Image} object to contain only
#' non-zero values plus a given border
#'
#' @param img A grayscale or binary \code{Image} object of two dimensions
#' @param border Single integer value of border to add to cropped image
#' @param fill Background value for border
#'
#' @details
#' Non-zero pixels on the edge of the image are removed, leaving a border of
#' \code{border} pixels of \code{fill} color.
#'
#' @return
#' A cropped image with the specified border
#'
#' @import EBImage
#'
#' @export
#'
crop <- function(img, border = 1L, fill = 0L)
{
	if (colorMode(img) != Grayscale)
		stop("'img' must be a grayscale or binary object")
	if (length(dim(img)) > 2)
		stop("crop requires a single two-dimensional image")

# Convert the underlying array data structure directly into a matrix
  mat_data <- as.matrix(img@.Data)

# Pass the cleanly formatted matrix to C++
  res <- crop_cpp(mat_data, border, fill)

# return as image
	as.Image(res)

}
