#' Improved Circularity and Perimeter Calculation
#' @rdname circularity
#'
#' @param x An \code{Image} object mask with a single object to be analyzed.
#' 
#' @details
#' At the cost of increased computing time, these functions provide more precise
#' measures of the circularity and perimeter of a binary object than the
#' algorithm in \code{\link[EBImage]{computeFeatures}}. 
#' 
#' @return Circularity as a value between 0 (line) and 1 (circle)
#' 
#' @export
#' 
circularity <- function(x) 4*pi*sum(x)/perimeter(x)^2

#' @name perimeter
#' @rdname circularity
#' 
#' @return The perimeter (in pixels) of a binary object
#'
#' @export
#'
perimeter <- function(x)
{
# helper functions to shift the image in x in one direction
	S1 <- function(x) cbind(rep(0,nrow(x)), x[,-ncol(x)] )
	S2 <- function(x) cbind(x[,-1], rep(0,nrow(x)) )
	S3 <- function(x) rbind(rep(0,ncol(x)), x[-nrow(x),] )
	S4 <- function(x) rbind(x[-1,], rep(0,ncol(x)) )
# determine edges
	e <- x & !(S1(x) & S2(x) & S3(x) & S4(x))
# add horizontal and vertical segments
	segs1 <- sum(e & S1(e)) + sum(e & S2(e)) + sum(e & S3(e)) + sum(e & S4(e))
# add diagonal segments
	segs2 <- sqrt(2)*(sum(e & S1(S3(e))) + sum(e & S1(S4(e))) +
		sum(e & S2(S3(e))) + sum(e & S2(S4(e))))
# each segmented was counted twice, divide by 2
	return((segs1 + segs2)/2)
}
