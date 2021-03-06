% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bresenham.R
\name{bresenham}
\alias{bresenham}
\title{Bresenham's Integer Line Drawing Algorithm}
\usage{
bresenham(x, y = NULL, close = TRUE)
}
\arguments{
\item{x, y}{the x and y coordinates a points to be joined where y can
can be missing and the argument x is processed by \code{\link{xy.coords}}.}

\item{close}{\code{logical} value indicating if the points form a closed
polygon (without duplicating the first and last points)}
}
\value{
A list of length 2 with \code{integer} \code{x,y} coordinates connecting
  the vertices
}
\description{
Generate integer x,y points between vertices by Bresenham's algorithm.
}
\examples{
# simple line
  bresenham(x = c(1, 4), y = c(1, 12))
# closed and open polygon
  verts <- list(x = c(1, 9, 6, 4, 2), y = c(9, 9, 2, 3, 1))
  plot(verts, type = "l", ylim = c(0, 10), xlim = c(0, 10))
  points(bresenham(verts)) # closed
  points(bresenham(verts, close = FALSE), col = 2, pch = 16)

}
