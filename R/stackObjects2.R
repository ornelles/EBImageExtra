#' Place detected objects with bounding box in an image stack
#' 
#' Place objects in an image stack with the same logic as
#' \code{\link[EBImage]{stackObjects}} except the entire bounding box is
#' included for each object in the image stack.
#' 
#' @param mask An \code{Image} object or an \code{array} containing object
#'   masks. Object masks are sets of pixels with the same unique integer value.
#' @param combine If \code{mask} contains multiple images, should the resulting
#'   \code{list} of image stacks be combined using \code{\link[EBImage]{combine}}
#'   into a single image stack.
#' @param bg.col Background pixel color.
#' @param ext A \code{numeric} controlling the size of the output image. If
#'   missing, \code{ext} is estimated from data as described in
#'   \code{\link[EBImage]{stackObjects}}.
#' 
#' @details
#' Like \code{stackObjects}, \code{stackObjects2} creates a set of \code{n}
#' images of size \code{c(2*ext+1,2*ext+1)}, where \code{n} is the number of
#' objects in \code{mask}, and places each object of \code{mask} in this set.
#' 
#' If \code{ext} is missing, it is estimated with the 98\% quantile of the
#' \code{m.majoraxis/2} extracted from \code{computeFeatures.moment}.
#' 
#' @return
#' An \code{Image} object containing the stacked objects contained in
#' \code{mask}. If \code{mask} contains multiple images and if \code{combine}
#' is \code{TRUE}, \code{stackObjects2} returns a single combined image
#' otherwise a \code{list} of \code{Image} objects is returned.
#'
#' @import EBImage
#' 
#' @export
#' 
stackObjects2 <- function(mask, ref, combine = TRUE, bg.col = "black", ext)
{
	require(EBImage)

# error checks, try to manage color images
	if (colorMode(mask) != Grayscale)
		stop("mask 'mask' must be a Grayscale image or array")
	dm <- dim(mask)
	if (!identical(dm[1:2], dim(ref)[1:2]))
		stop("'mask' and 'ref' have different dimensions")

# return with NULL if nothing in the mask
	if (all(mask == 0)) return(NULL)

# extract center of mass from mask
	hf <- lapply(getFrames(mask), computeFeatures.moment)

# determine new dimension using logic of 'stackObjects'
	if (missing(ext)) {
		ext <- unlist(sapply(hf, function(h) h[, "m.majoraxis"]))/2
		ext <- quantile(ext, 0.98, na.rm = TRUE)
	}
	new.dim <- floor(c((2*ext + 1), (2*ext + 1)))

# replicate background color
	nf <- numberOfFrames(ref, type = "render")
	bg.col <- rep(bg.col, nf)[1:nf]
		
# determine center of mass and desired xy coordinate for each object
	xy <- lapply(hf, function(h) h[, c("m.cx", "m.cy"), drop = FALSE])
	sel <- !sapply(xy, is.null) # can't use 'apply' on NULL objects
	xy[sel] <- lapply(xy[sel], round)

# extract images in as a list
	ans <- list()
	for (nf in seq.int(numberOfFrames(mask))) { # for each frame
		myFrame <- getFrame(ref, nf, type = "render")
		mids <- xy[[nf]]
		res <- lapply(seq.int(nrow(mids)), function(i) EBImage::translate(myFrame,
			v = ext - mids[i, ], output.dim = new.dim, bg.col = bg.col[nf]))
		ans <- c(ans, res)
	}
	if (!combine || length(ans) == 1)
		return(ans[[1]])
	else
		return(combine(ans))
}
