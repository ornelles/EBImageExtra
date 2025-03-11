#' Accessory functions for class "Roi"
#'
#' Convert an \code{Image} object into an \code{Roi} object
#'
#' @param img object of class \code{Image}
#' @param resize re-assign \code{"loc" slot} if \code{TRUE} 
#'
#' @details
#' This adds the \code{class} "Roi" to \code{Image} objects and places the
#' dimensions of the image in the \code{slot} named "\code{loc}."
#' \code{Roi} objects passed as an argument will be considered an error
#' \strong{unless} \code{resize = TRUE}.
#'
#' @return \code{Roi} object of the same dimensions as the 
#'
#' @examples
#' 
#' # Region of interest from sample image in EBImage package
#'   birds <- readImage(system.file("images", "sample-color.png", package="EBImage"))
#'   roi <- getROI(birds, 160, 255, w = 200, h = 240)
#' 
#' # Examine effect on Image object
#'   attr(birds, "loc") # nothing here
#'   attr(as.Roi(birds), "loc") # returned an Roi
#' 
#' # Examine effect on 'Roi'
#'   pp <- attr(roi, "loc") # original
#'   roi2 <- resize(roi, w = 400)
#'   pp2 <- attr(roi2, "loc") 
#'   identical(pp, pp2) # re-sizing the image has no effect on 'loc'
#' 
#' # Compare with resize = TRUE
#'   attr(roi, "loc") 
#'   attr(as.Roi(roi, resize = TRUE), "loc") 
#' 
#' @import EBImage
#'
#' @export
#'
as.Roi <- function(img, ...) {
	UseMethod("as.Roi")
}
NULL
#' @export
#' 
as.Roi.Image <- function(img, resize = FALSE, ...)
{
# already an Roi
  if (is(img, "Roi") && resize == FALSE) {
		warning("already an Roi: use 'resize = TRUE' to resize the 'loc' slot")
		img
	}
  else if (is(img, "Roi") && resize == TRUE) {
		dm <- base::dim(img)
		img@loc <- list(x = c(1, dm[1]), y = c(1, dm[2]))
		img
	}
# coerce others to Roi superclass
  else {
    img <- as(img, "Roi")
		dm <- base::dim(img)
		img@loc <- list(x = c(1, dm[1]), y = c(1, dm[2]))
		img
	}
}

#' @export
as.Roi.default <- function(img, ...) as.Roi(img, ...)

#' @rdname as.Roi
#' @export
as.ROI <- as.Roi