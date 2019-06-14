drawROI <- function(img, x, y, x2, y2, width = 2, col = "white",
	sides = 1:4, pch = 3, col.pch = col)
{
	if (!is(img, "Image"))
		stop("'img' must be an Image object")
	if (is(col, "numeric"))
		col <- palette()[col]
	dm <- dim(img)[1:2]
	
# vector of flags for missing arguments
  M <- c(missing(x), missing(y), missing(x2), missing(y2))

# place corners of roi from given arguments in 'p'
  if (!any(M[1:4])) { # specified as x, y, x2, y2
    pp <- list(x = sort(c(x, x2)), y = sort(c(y, y2)))
  }
  else if (all(M[1:4])) { # nothing provided except image
    plot(img)
    pp <- locator(2, type = "p", pch = pch, col = col.pch)
    pp <- lapply(pp, sort)
  }
  else if (!M[1] & all(M[2:4])) { # only 'x', must be list of corners
    if (is(x, "list") && length(x) == 2 && all(lengths(x) == 2))
      pp <- setNames(lapply(x, sort), c("x", "y"))
    else
      stop ("if only 'x' is provided, it must be a list of two points")
  }
  else if (!any(M[1:2] & M[3:4])) { # 'x' and 'y' provided
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
	nx <- sum(c(2,4) %in% sides)
	ny <- sum(c(1,3) %in% sides)
	dm2 <- dm - c(nx, ny)*width
	if (any(dm2 < 1))
		stop("'width' is too large to use")
##
## in progress...
##
# create mask to accept annotation
	mask <- Image("black", dim = dm, colormode = colorMode(img))
	xc <- c(seq.int(pp$x[1], len = width), seq.int(pp$x[2] - width, len = width))
	yc <- c(seq.int(pp$y[1] - width, len = width), seq.int(pp$y[2], len = width))
	annot <- Image(
	mask[
	ans <- resize(img, w = dm2[1], h = dm2[2], output.dim = dm2)
	xborder <- Image(col, c(dm2[1], width), colormode = colorMode(img))
	yborder <- Image(col, c(width, dm[2]), colormode = colorMode(img))

##
## Use this logic above to paint pixels in rectangle...
###
	if (1 %in% sides)
		ans <- abind(ans, xborder, along = 2)
	if (3 %in% sides)
		ans <- abind(xborder, ans, along = 2)
	if (2 %in% sides)
		ans <- abind(yborder, ans, along = 1)
	if (4 %in% sides)
		ans <- abind(ans, yborder, along = 1)

	return(ans)
}
