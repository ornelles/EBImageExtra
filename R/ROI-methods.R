#' Accessor functions for class "Roi"
#'
#' Presently, only one to extract the \code{loc} slot
#'
#' @param x object of class \code{roi} created by \code{Roi}
#'
#' @return list of original location in the slot \code{loc}
#'
#' @import EBImage
#'
#' @export
#'
loc <- function(x, ...) slot(x, "loc")
