#' Vectorized Normalize Function for Images
#' 
#' Vectorized wrapper for \code{\link[EBImage]{normalize}} with a default
#' \code{inputRange} appropriate for 12-bit images.
#' 
#' @param x An \code{Image} object or array or a list of \code{Image} objects
#' @param ir A numeric vector or a list of vectors or a single numeric
#'   value indicating the bit depth
#'   to set the input intensity range. This value is passed to
#'   \code{\link[EBImage]{normalize}} as the \code{inputRange} parameter
#' @param separate If \code{TRUE} each frame will be normalized independently
#'   and \code{ir} will be ignored
#' @param ft A numeric vector of 2 values or \code{list} of vectors to be the
#'   target minimum and maximum intensity values after normalization
#' 
#' @details
#' See \code{\link[EBImage]{normalize}} for details. The default here differs
#' from \code{\link{normalize}} the default value for \code{ir} forces
#' the call to use \code{separate = FALSE}. The default value sets
#' \code{inputRange} to \code{c(0,2^-4)}, which is appropriate for
#' 12-bit images. This function provides an easier-to-type replacement for 
#' \code{\link{normalize}} while extending it to accept lists of images.
#' 
#' If \code{ir} is a \code{numeric} value of length 1 between 1 and 16,
#' it is interpreted as the effective bit depth of the input image and
#' the value for \code{ir} is given by \code{c(0, 2^-(16-ir)}. This allows
#' the use of values such as \code{11.5} to increase the brightness of a
#' 12-bit image.
#' 
#' @return
#' An \code{Image} object or array or a \code{list} of such objects containing
#' the transformed object(s).
#'
#' @import EBImage
#' 
#' @name ni
#' @aliases vni
#'
#' @export
#'  
ni <- function(x, ir = c(0, 2^-4), separate = FALSE, ft = c(0, 1))
{
# working function
	.fun <- function(x, ir, separate, ft) {
		if (is.null(x)) return(NULL)
		if (length(ir) == 1 && ir > 1 && ir <= 16)
			ir <- c(0, 2^(ir - 16))
		if (identical(separate, TRUE))
			EBImage::normalize(x, separate = separate, ft = ft)
		else
			EBImage::normalize(x, inputRange = ir, ft = ft)
	}
# process single image
	if (is(x, "Image"))
		ans <- .fun(x, ir, separate, ft)
# process list of images
	else if (is(x, "list") && all(sapply(x, is, "Image"))) {
		ir <- if(is(ir, "list")) ir else list(ir)
		separate <- if(is(separate, "list")) separate else list(separate)
		ft <- if(is(ft, "list")) ft else list(ft)
		ans <- Map(.fun, x, ir, separate, ft)
	}
	else
		stop("'x' must be an Image or list of Images")
	return(ans)
}

# alias
vni <- ni
