#' Point Inclusion in Polygon Test
#'
#' Point inclusion test by
#' \href{https://wrf.ecse.rpi.edu/Research/Short_Notes/pnpoly.html}{W. Randolph Franklin}
#' to test for inclusion of
#' a set of points within a polygon defined by a set of vertices.
#'
#' @param points Points to be tested as any form appropriate for
#'   \code{\link{xy.coords}}.
#' @param vertices Points that define the vertices of the polygon, also
#'   processed by \code{\link{xy.coords}} where the order defines the polygon
#'   and the first and last points will be connected to close the polygon.
#'
#' @return
#'
#' Logical vector indicating whether each point lies in (\code{TRUE})
#' or out (\code{FALSE}) of the polygon. 
#' 
#' @examples
#'   vv <- list(x = c(4.7, 3.8, 2, 3.6, 6.9, 6.9),
#'     y = c(9, 5.8, 4.7, 2.1, 4.2, 7.5))
#'   pp <- list(x = c(3.3, 6, 3.2, 4.9),
#'     y = c(4.4, 4.6, 7.2, 6.9))
#'   plot(1:10, 1:10, type = "n")
#'   polygon(vv)
#'   text(pp$x, pp$y, 1:4, col = 2)
#'   data.frame(point = 1:4, inside = pnpoly(pp, vv))
#'
#' @export
#'
pnpoly <- function(points, vertices)
{
	pp <- xy.coords(points)
	vv <- xy.coords(vertices)
	nvert <- length(vv$x)
	npoints <- length(pp$x)

  # working function
	.fun <- function(x, y, vx, vy, nvert) {
		inside <- FALSE
		j <- nvert
		for (i in seq_len(nvert)) {
			if (((vy[i] > y) != (vy[j] > y)) &&
					(x < (vx[j] - vx[i]) * (y - vy[i]) / (vy[j] - vy[i]) + vx[i]))
				inside <- !inside
			j <- i
		}
		return(inside)
	}

  # apply to each pair of coordinates in points
	sapply(seq_len(npoints),
		function(k) .fun(pp$x[k], pp$y[k], vv$x, vv$y, nvert))
}