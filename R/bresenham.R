#' Bresenham's Integer Line Drawing Algorithm
#'
#' Generate integer x,y points between vertices by Bresenham's algorithm. 
#'
#' @param x,y the x and y coordinates a points to be joined where y can
#'  can be missing and the argument x is processed by \code{\link{xy.coords}}.
#' @param close \code{logical} value indicating if the points form a closed
#'  polygon (without duplicating the first and last points)
#'
#' @return
#'
#' A list of length 2 with \code{integer} \code{x,y} coordinates connecting
#'   the vertices
#' @examples
#' # simple line
#'   bresenham(x = c(1, 4), y = c(1, 12))
#' # closed and open polygon
#'   verts <- list(x = c(1, 9, 6, 4, 2), y = c(9, 9, 2, 3, 1))
#'   plot(verts, type = "l", ylim = c(0, 10), xlim = c(0, 10))
#'   points(bresenham(verts)) # closed
#'   points(bresenham(verts, close = FALSE), col = 2, pch = 16)
#'
#' @export
#'
bresenham <- function(x, y = NULL, close = TRUE)
{
# accept any coordinate structure
  v <- xy.coords(x = x, y = y, recycle = TRUE, setLab = FALSE)
  if (!all(is.finite(v$x), is.finite(v$y)))
    stop("finite coordinates required")

  v[1:2] <- lapply(v[1:2], round) # Bresenham's algorithm IS for integers
  nx <- length(v$x)
  if (nx == 1) return(list(x = v$x, y = v$y)) # just one point
  if (nx > 2 && close == TRUE) { # close polygon by replicating 1st point
    v$x <- c(v$x, v$x[1])
    v$y <- c(v$y, v$y[1])
    nx <- nx + 1
  }
# collect result in 'ans, staring with 1st point
  ans <- lapply(v[1:2], "[", 1)
  
# process all vertices in pairs
  for (i in seq.int(nx - 1)) {
    x <- v$x[i] # coordinates updated in x, y
    y <- v$y[i]
    x.end <- v$x[i + 1]
    y.end <- v$y[i + 1]

    dx <- abs(x.end - x); dy <- -abs(y.end - y)
    sx <- ifelse(x < x.end, 1, -1)
    sy <- ifelse(y < y.end, 1, -1)
    err <- dx + dy

  # process one segment
    while(!(isTRUE(all.equal(x, x.end)) && isTRUE(all.equal(y, y.end)))) {
      e2 <- 2 * err
      if (e2 >= dy) { # increment x
        err <- err + dy
        x <- x + sx
      }
      if (e2 <= dx) { # increment y
        err <- err + dx
        y <- y + sy
      }
      ans$x <- c(ans$x, x)
      ans$y <- c(ans$y, y)
    }
  }
# remove duplicated points (typically 1st and last)
  dups <- duplicated(do.call(cbind, ans), MARGIN = 1) 
  return(lapply(ans, "[", !dups))
}