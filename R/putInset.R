##
## putInset - add a scaled inset to the given image and return marked-up
##
putInset <- function(img, ins, position, frac = NULL, mag = NULL,
	col = "white", lwd = 2, lend = "square", res = 300, 
	...)
{
# argument checks
	if(!is(img, "Image") | !is(ins, "Image"))
		stop ("both 'img' and 'ins' must be Image objects")
	cm1 <- colorMode(img)
	cm2 <- colorMode(ins)
	if (cm1 != cm2)
		warning("grayscale image converted to RGB")
	if (cm1 > cm2)
		ins <- toRGB(ins)
	else if (cm2 > cm1)
		img <- toRGB(img)

	dm.img <- dim(img)
	dm.ins <- dim(ins)
	cmx <- colorMode(img)

# Determine magnification 'mag' and fractional size 'frac'
	if (is.null(frac) & is.null(mag)) {
		message("default value of 1/3 used for 'frac'")
		frac <- 1/3
		mag <- frac * dm.img[1]/dm.ins[1]
	}
	else if (is.null(frac) & !is.null(mag)) { # given 'mag'
		frac <- mag * dm.ins / dm.img
		if (any(frac[1:2] >= 1))
			stop("inset will exceed size of image with mag = ", signif(mag, 2))
		frac <- frac[1]
	}
	else if (!is.null(frac) & is.null(mag)) { # given 'frac'
		if (frac > 1 | frac < 0)
			stop("'frac' must be between 0 and 1")
		frac <- frac
		mag <- frac * dm.img[1]/dm.ins[1]
	}
	else if (!is.null(frac) & !is.null(mag)) { # given both
		if (signif(mag, 3) != signif(frac * dm.img[1]/dm.ins[1]))
			stop("incompatible 'mag' and 'frac' values provided, use one or the other")
		else
			frac <- frac
	}

# Resize inset by mag and recalculate dm.ins
  ins <- resize(ins, w = mag * dm.ins[1])
  dm.ins <- dim(ins)
	
# Determine position for inset
	CHOICES <- c("topleft", "top", "topright", "left", "center", "right",
		"bottomleft", "bottom", "bottomright")
	if (!missing(position))
			position <- match.arg(position, CHOICES)
	else {
		vx <- seq(0, dm.img[1] + 1, length = 4)
		vy <- seq(0, dm.img[2] + 1, length = 4)
		i <- 0
		while (!i %in% 1:9) {
			p <- locator(1)
			ix <- findInterval(p$x, vx)
			iy <- findInterval(p$y, vy)
			i <- ix + 3*(iy - 1)
		}
		position <- CHOICES[i]
	}
		
# Calculate coordinates of inset in image
	xpad <- floor((dm.img[1] - dm.ins[1])/2)
	ypad <- floor((dm.img[2] - dm.ins[2])/2)
	pp <- switch(position,
		"topleft" = list(x = c(1, dm.ins[1]), y = c(1, dm.ins[2])),
		"top" = list(x = xpad + c(1, dm.ins[1]), y = c(1, dm.ins[2])),
		"topright" = list(x = c(dm.img[1]-dm.ins[1]+1, dm.img[1]), y = c(1, dm.ins[2])),
		"left" = list(x = c(1, dm.ins[1]), y = ypad + c(1, dm.ins[2])),
		"center" = list(x = xpad + c(1, dm.ins[1]), y = c(dm.img[2]-dm.ins[2]+1, dm.img[2]) - ypad),
		"right" = list(x = c(dm.img[1]-dm.ins[1]+1, dm.img[1]), y = c(dm.img[2]-dm.ins[2]+1, dm.img[2]) - ypad),
		"bottomleft" = list(x = c(1, dm.ins[1]), y = c(dm.img[2]-dm.ins[2]+1, dm.img[2])),
		"bottom" = list(x = xpad + c(1, dm.ins[1]), y = c(dm.img[2]-dm.ins[2]+1, dm.img[2])),
		"bottomright" = list(x = c(dm.img[1]-dm.ins[1]+1, dm.img[1]), y = c(dm.img[2]-dm.ins[2]+1, dm.img[2])))

# Calculate index into image and replace
  idx <- Map(function(v) seq.int(v[1], v[2]), pp)
	if (colorMode(img) == Color)
		img[idx$x, idx$y, ] <- ins
	else
		img[idx$x, idx$y] <- ins
	plot(img)

# Add possible frame to inset
# Method 1 - replace pixels in situ ## Not quite, how to specify color across matrix
#	inc <- ceiling(lwd)
#	p1 <- lapply(pp, function(v) c(seq(v[1], len = inc), seq(v[2] - inc + 1, len = inc)))
#	p2 <- lapply(pp, function(v) seq(v[1], v[2]))
#	img[p1$x, p2$y, ] <- 1
#	img[p2$x, p1$y, ] <- 1
	
# Method 2 - paint onto image, save and reload
	rect(p$x[1], p$y[1], p$x[2], p$y[2], border = col, lend = lend, lwd = lwd, ...)
	rect(pp$x[1], pp$y[1], pp$x[2], pp$y[2], border = col, lend = lend, lwd = lwd/frac, ...)

# Write and retrieve the new image
	ftemp <- tempfile()
	dev.print(tiff, ftemp, width = dm.img[1], height = dm.img[2], bg = "transparent")
	dev.off()
	img <- readImage(ftemp, type = "tiff")
	file.remove(ftemp)
	plot(img)
	invisible(img)
}
