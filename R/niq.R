#' Normalize Each Frame of an Image to an Upper "Quantile"
#' 
#' This function normalizes each frame of an image to the range specified
#' specified by \code{quant} (default quantile of 0.998.)
#' 
#' @param x An \code{Image} object or array or a list of \code{Image} objects
#' @param quant A numeric value between 0 and 1 specifying the upper
#'	quantile for the normalization. 
#' @param ft A numeric vector of 2 values or \code{list} of vectors to be the
#'   target minimum and maximum intensity values after normalization.
#' 
#' @details
#' See \code{\link[EBImage]{normalize}} for details as well as \code{ni}. 
#' 
#' @return
#' An \code{Image} object or array or a \code{list} of such objects containing
#' the transformed object(s).
#'
#' @import EBImage
#' 
#' @export
#'  
# niq - normalize each frame of image to an upper quantile

niq <- function(x, quant = 0.998, ft = c(0, 1))
{
# process arguments
	if (is(x, "Image"))
		x <- EBImage::getFrames(x, type = "render")
	else
		stop("'x' must be an Image object")
	if (length(quant) != 1 || quant < 0 || quant > 1)
		stop("'quant' must be a single number in [0,1]")

#	determine value at quantile for each image in list
	upr <- sapply(x, function(x) quantile(x[x > 0], quant))
	ir <- lapply(upr, function(x) c(0, x))
	
# working function
	.fun <- function(x, ir, ft) {
		if (is.null(x)) return(NULL)
		if (diff(range(x)) == 0) return(x)
		normalize(x, inputRange = ir, ft = ft)
	}
# apply working function appropriately - having already check for errors!
	if (is(x, "Image"))
		ans <- .fun(x, ir, ft)
	else { # multiple frames
		ir <- if(is(ir, "list")) ir else list(ir)
		ft <- if(is(ft, "list")) ft else list(ft)
		ans <- Map(.fun, x, ir, ft)
	}
	ans <- EBImage::abind(ans, rev.along = 0)
	return(drop(ans))
}
