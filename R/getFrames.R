#' Corrected getFrames function from EBImage
#'
#' This corrects the frame argument error in the EBImage code and sets the 
#' default from "total" to "render".
#'
#' @param y An \code{Image} object or array.
#' @param i Number of frames as a single number of vector of numbers. If
#'		missing, all frames are returned.
#' @param type A character string with \code{total} or \code{render}. The
#'		\emph{new} default is \code{render}.
#' 
#' @seealso
#' \code{\link[EBImage]{getFrame}}
#'
#' @return
#' Returns i-th frame(s) containing the image \code{y}. If \code{type} is
#' \code{total}, the total number of frames is independent of the color mode
#' and equals the product of all dimensions except the first two.
#'
#' @import EBImage
#'
#' @export
#'
getFrames <- function (y, i, type = c("render", "total"))
{
  type <- match.arg(type)
  n <- EBImage::numberOfFrames(y, type)
  if (missing(i))
    i <- seq_len(n)
  else {
    i <- as.integer(i)
    if (any(i < 1L | i > n))
      stop("'i' must be a vector of numbers within 1 to ", n)
  }
  type <- switch(type, total = 0L, render = 1L)
  ans <- .Call(EBImage:::C_getFrames, y, i, type)
  EBImage::abind(ans, rev.along = 0)
}
