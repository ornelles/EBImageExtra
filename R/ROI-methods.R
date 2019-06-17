#' Accessory functions for class "Roi"
#'
#' Presently only one function to convert an \code{Image} object
#' into an \code{Roi} object
#'
#' @param x object of class \code{Image}
#'
#' @return \code{Roi} object of the same dimensions as the 
#'   argument but with a \code{slot} named \code{loc}
#'   holding the dimension of the image as the region
#'   of interest. \strong{Note that this will reset
#'   the \code{loc} slot an \code{Roi} object.}
#'
#' @import EBImage
#'
#' @export
#'
as.Roi <- function(x) UseMethod("as.Roi")

as.Roi.Image = function(x)
{
# already an Roi
  if (class(x) == "Roi") {
		dm <- base::dim(x)
		x@loc <- list(x = c(1, dm[1]), y = c(1, dm[2]))
		x
	}
# coerce others to Roi superclass
  else {
    x <- as(x, "Roi")
		dm <- base::dim(x)
		x@loc <- list(x = c(1, dm[1]), y = c(1, dm[2]))
		x
	}
}

as.Roi.default = function(x) Roi(x)
