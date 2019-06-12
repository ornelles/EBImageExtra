#' Sobel Filter for Edge Detection
#' 
#' Detect edges in an \code{Image} object
#' 
#' @param x An \code{Image} object or array
#' 
#' @details
#' The following matrix and its transpose are sequentially applied in
#' \code{\link{filter2}} to identify edges in the image as described
#' in this \href{https://en.wikipedia.org/wiki/Sobel_operator}{Wikipedia} page. 
#' 
#' \preformatted{
#'      [,1] [,2] [,3]
#' [1,]    1    0   -1
#' [2,]    2    0   -2
#' [3,]    1    0   -1
#' }
#' 
#' @return
#' An image object of the same size with edges highlighted.
#' 
#' @import EBImage
#' 
#' @export
#' 
sobel <- function(x) {
	sh <- matrix(c(1,2,1,0,0,0,-1,-2,-1), nrow = 3)
	sv <- t(sh)
	xh <- filter2(x, sh, boundary = "replicate")
	xv <- filter2(x, sv, boundary = "replicate")
	return(sqrt(xh^2 + xv^2))
}
