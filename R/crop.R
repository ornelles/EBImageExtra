#' Crop a grayscale or binary image
#' 
#' Crop a grayscale or binary \code{Image} object to non-zero values plus a
#' given border
#' 
#' @param img A grayscale or binary \code{Image} object of two dimensions
#' @param border Single integer value of border to add to cropped image
#' @param bg Background value for border
#' 
#' @details
#' Non-zero pixels on the edge of the image are removed, leaving a border of
#' \code{border} pixels of \code{bg} color.
#' 
#' @return
#' A cropped image with the specified border
#' 
#' @import EBImage
#' 
#' @export
#' 
crop <- function(img, border = 1, fill = 0)
{
	if (colorMode(img) != Grayscale)
		stop("'img' must be a grayscale or binary object")
	if (length(dim(img)) > 2)
		stop("crop requires a single two-dimensional image")
 
	img <- cbind(0, img, 0)   # ensure an edge exists!
	img <- rbind(0, img, 0)
	mask <- img > 0

	xb <- apply(mask, 1, Negate(any))	# blank rows
	xr <- rle(xb)
	nx <- xr$lengths * xr$values
	ix <- seq_len(dim(mask)[1])
	ix <- intersect(tail(ix, -head(nx, 1)), head(ix, -tail(nx, 1)))

	yb <- apply(mask, 2, Negate(any))	# blank columns
	yr <- rle(yb)
	ny <- yr$lengths * yr$values
	iy <- seq_len(dim(mask)[2])
	iy <- intersect(tail(iy, -head(ny, 1)), head(iy, -tail(ny, 1)))

	dm <- c(length(ix), length(iy))
	z <- Image(fill, dim = dm + 2 * border) 
	z[border + seq_along(ix), border + seq_along(iy)] <- img[ix, iy]
	return(z)
}
