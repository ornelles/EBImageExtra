getInset <- function(img, x, y, x2, y2, w, h, 
	method = c("center.and.dim", "corner.and.dim", "opposite.corners"), which.corner = c("bottomleft", "topleft", "bottomright", "topright"),
	bg.col = "black", pch = 3, col = "magenta", border = col, lwd = 2,
	plot = TRUE, markup = TRUE)
{
	require("EBImage")

	if(!is(img, "Image")) stop("'img' must be an Image object")
	dm <- dim(img)[1:2]
	method <- match.arg(method)
	which.corner = match.arg(which.corner)

	if (plot == TRUE) plot(img)

# list of flags for missing arguments
	F <- c(missing(x), missing(y), missing(x2), missing(y2),
			missing(w), missing(h))

#	process based on the arguments provided
	if (!any(F[1:4])) # inset specified, done
		pp <- list(x = sort(c(x, x2)), y = sort(c(y, y2)))
	else if (all(F[1:6])) { # nothing provided except image
		method <- "opposite.corners"
		pp <- locator(2, type = "p", pch = pch, col = col)
		pp <- lapply(pp, sort)
	}
	else if (!any(F[5:6])) { # 'w' and 'h' provided
		if (any(F[1:2])) # need to get one point
			pp <- locator(1, type = "p", pch = pch, col = col)
		else # c('x', 'y') is the one point
			pp <- list(x = x, y = y)
	# adjust the one point
		if (method == "corner.and.dim") {
			if (which.corner == "bottomleft")
				pp <- list(x = c(pp$x, pp$x + w), y = c(pp$y, pp$y - h))
			else if (which.corner == "topleft")
				pp <- list(x = c(pp$x, pp$x + w), y = c(pp$y, pp$y + h))
			else if (which.corner == "bottomright")
				pp <- list(x = c(pp$x - w, pp$x), y = c(pp$y, pp$y - h))
			else # which.corner == "topright"
				pp <- list(x = c(pp$x - w, pp$x), y = c(pp$y, pp$y + h))
		}
		else # method == "center.and.dim"
			pp <- list(x = pp$x + c(-1,1)*w/2, y = pp$y + c(-1,1)*h/2)
	}
	else
		stop("need 'w,h' values if only one pair of 'x,y' values are provided")

# adjust the desired inset coordinates
	pp$x <- pmax(1, pmin(pp$x, dm[1]))
	pp$y <- pmax(1, pmin(pp$y, dm[2]))
	pp <- lapply(pp, round)
	pp <- lapply(pp, sort)

# add frame
	if (markup == TRUE)
		rect(pp$x[1], pp$y[2], pp$x[2], pp$y[1], border = border, lwd = lwd)

# create coordinates to extract inset
	pp <- lapply(pp, function(v) seq.int(v[1], v[2]))
	if (colorMode(img) == Grayscale)
		ans <- img[pp$x, pp$y]
	else if (colorMode(img) == Color)
		ans <-img[pp$x, pp$y, ]
	else
		stop("what kind of image was this?")
	invisible(ans)
}
