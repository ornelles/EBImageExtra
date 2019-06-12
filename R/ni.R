#' Vectorized Normalize Function for Images
#' 
#' Vectorized wrapper for \code{\link[EBImage]{normalize}} with a default
#' \code{inputRange} appropriate for 12-bit images.
#' 
#' @param x An \code{Image} object or array or a list of \code{Image} objects
#' @param ir A numeric vector or a list of vectors to set the input
#'   intensity range. This value is passed to \code{\link[EBImage]{normalize}}
#'   as the \code{inputRange} parameter
#' @param separate If \code{TRUE} or if \code{ir} is \code{NULL}, each frame
#'   will be normalized independently
#' @param ft A numeric vector of 2 values or \code{list} of vectors to be the
#'   target minimum and maximum intensity values after normalization
#' 
#' @details
#' See \code{\link[EBImage]{normalize}} for details. The default here is to use
#' an \code{inputRange} of \code{c(0,2^-4)}, which is appropriate for
#' 12-bit images. This function provides an easier-to-type replacement for 
#' \code{\link{normalize}} while extending it to accept lists of images.
#' 
#' @return
#' An \code{Image} object or array or a \code{list} of such objects containing
#' the transformed object(s).
#' 
#'
#' @import EBImage
#' 
#' @name ni
#' @aliases vni
#'
#' @export
#'  
ni <- function(x, ir = c(0, 2^-4), separate = TRUE, ft = c(0, 1))
{
# working function
	.fun <- function(x, ir, separate, ft) {
		if (is.null(x))
			x
		else
			normalize(x, separate = separate, ft = ft, inputRange = ir)
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
