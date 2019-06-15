drawROI <- function(img, x, y, x2, y2, width = 2, col = "white",
	sides = 1:4, pch = 3, col.pch = col, showImage = TRUE)
{
	if (!is(img, "Image"))
		stop("'img' must be an Image object")
	if (is(col, "numeric"))
		col <- palette()[col]
	dm <- dim(img)[1:2]
	
# vector of flags for missing arguments
  F <- c(missing(x), missing(y), missing(x2), missing(y2))

# place corners of roi from given arguments in 'p'
  if (!any(F[1:4])) { # specified as x, y, x2, y2
    pp <- list(x = sort(c(x, x2)), y = sort(c(y, y2)))
  }
  else if (all(F[1:4])) { # nothing provided except image
    plot(img)
    pp <- locator(2, type = "p", pch = pch, col = col.pch)
    pp <- lapply(pp, sort)
  }
  else if (!F[1] & all(F[2:4])) { # only 'x', must be list of corners
    if (is(x, "list") && length(x) == 2 && all(lengths(x) == 2))
      pp <- setNames(lapply(x, sort), c("x", "y"))
    else
      stop ("if only 'x' is provided, it must be a list of two points")
  }
  else if (!any(F[1:2] & F[3:4])) { # 'x' and 'y' provided
		if (length(x) == 2 && length(y) == 2)
			pp <- list(x = x, y = y)
		else
			stop ("'x' and 'y' must be length 2 numeric vectors")
	}
	else
		stop("unable to use this combination of arguments")

# adjust roi coordinates to the limits of the image
  pp$x <- pmax(1, pmin(pp$x, dm[1]))
  pp$y <- pmax(1, pmin(pp$y, dm[2]))
  pp <- lapply(pp, round)
  pp <- lapply(pp, sort)

# identify borders and check new dimension
	nx <- sum(c(2, 4) %in% sides)
	ny <- sum(c(1, 3) %in% sides)
	dm2 <- dm - c(nx, ny)*width
	if (any(dm2 < 1))
		stop("'width' is too large to use")

# create two list of coordinates for border
	pp1 <- lapply(pp, function(v) list(seq(v[1], len = width),
		seq(v[2] - width + 1, len = width)))
	pp2 <- lapply(pp, function(v) v[1]:v[2])

# create logical masks for each side of the border
	m <- Image(TRUE, dim(img)[1:2])
	S1 <- col(m) %in% pp1$y[[2]] & row(m) %in% pp2$x
	S3 <- col(m) %in% pp1$y[[1]] & row(m) %in% pp2$x
	S2 <- row(m) %in% pp1$x[[1]] & col(m) %in% pp2$y
	S4 <- row(m) %in% pp1$x[[2]] & col(m) %in% pp2$y
	S <- list(S1, S2, S3, S4)
	m[Reduce(`|`, S)] <- FALSE

# remove the sides that are not requested
	m[Reduce(`|`, S[!1:4 %in% sides])] <- TRUE

	if (colorMode(img) == Color)
		M <- abind(m, m, m, along = 3)
	else
		M <- m

# combine with replacement color
	mask <- Image(col, dim = dim(img)[1:2], colormode = 2)
	ans <- img * M + mask * !M
	if (showImage)
		plot(ans)
	invisible(ans)
}
