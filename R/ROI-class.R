#' @include ROI-methods.R
NULL
#' Roi-constructor
#'
#' The function \code{Roi} is an S4 constructor to create
#' the extended S4 class named "Roi". 
#'
#' @description
#' This class extends \code{Image} object from \code{EBImage} 
#' to include a class named "Roi" and a slot named "loc" to hold the
#' original location of the region of interest. This slot can be
#' reached by the accessor function \code{loc} or simply by
#' \code{attr(obj, "loc"} or by \code{obj@loc}. Previously, \code{roxygen2}
#' seemed to require that the parent (\code{EBImage}) library be loaded
#' before it can recognize this directive...
#'
#' @param \code{object} \code{Image} object
#' @param loc \code{list} of two components, \code{x,y}
#'
#' @return \code{Image} object with added "Roi" class and "loc" slot
#'
#' @import EBImage
#'
Roi <- setClass("Roi",
	slots = c(loc = "list"),
	contains = "Image")

setMethod("initialize", "Roi", function(.Object, ...) {
	.Object <- callNextMethod()
	.Object
})
