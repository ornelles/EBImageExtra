##
## putInset - add a scaled inset to the given image and return marked-up
##
putInset <- function(img, inset, position, frac = NULL, mag = NULL,
	col = "white", border = col, lwd = 2, lend = "square", res = 300, 
	...)
{
# argument checks
	if(!is(img, "Image") | !is(inset, "Image"))
		stop ("both 'img' and 'inset' must be Image objects")
	cm1 <- colorMode(img)
	cm2 <- colorMode(inset)
	if (cm1 != cm2)
		warning("grayscale image converted to RGB")
	if (cm1 > cm2)
		inset <- toRGB(inset)
	else if (cm2 > cm1)
		img <- toRGB(inset)

	dmx <- dim(img)
	dmi <- dim(inset)
	cmx <- colorMode(img)

# Determine magnification 'mag' and fractional size 'frac'
	if (is.null(frac) & is.null(mag)) {
		message("default value of 1/3 used for 'frac'")
		fracx <- 1/3
		mag <- fracx * dmx[1]/dmi[1]
	}
	else if (is.null(frac) & !is.null(mag)) { ### WRONG?
		fracx <- mag * dmi[1]/dmx[1]
		fracy <- mag * dmi[2]/dmx[2]
		if (fracx >= 1 | fracy >= 1)
			stop("inset will exceed size of image with mag = ", signif(mag, 2))
	}
	else if (!is.null(frac) & is.null(mag)) {
		if (frac > 1 | frac < 0)
			stop("'frac' must be between 0 and 1")
		fracx <- frac
		mag <- fracx * dmx[1]/dmi[1]
	}
	else if (!is.null(frac) & !is.null(mag)) {
		if (signif(mag, 3) != signif(frac * dmx[1]/dmi[1]))
			stop("incompatible 'mag' and 'frac' values provided, use one or the other")
		else
			fracx <- frac
	}

# Resize inset by mag and recalculate dmi
  inset <- resize(inset, w = mag * dmi[1])
  dmi <- dim(inset)
	
# Determine position for inset
	CHOICES <- c("topleft", "top", "topright", "left", "center", "right",
		"bottomleft", "bottom", "bottomright")
	if (!missing(position))
			position <- match.arg(position, CHOICES)
	else {
		vx <- seq(0, dmx[1] + 1, length = 4)
		vy <- seq(0, dmx[2] + 1, length = 4)
		while (TRUE) {
			p <- locator(1)
			ix <- findInterval(p$x, vx)
			iy <- findInterval(p$y, vy)
			i <- ix + 3*(iy - 1)
			if (!i %in% 1:9)
				next # only accept valid values
			position <- CHOICES[i]
			if (position == "center")
				position <- "top"
			break
		}
	}
		
# Calculate coordinates of inset in image
	xpad <- floor((dmx[1] - dmi[1])/2)
	ypad <- floor((dmx[2] - dmi[2])/2)
	pp <- switch(position,
		"topleft" = list(x = c(1, dmi[1]), y = c(1, dmi[2])),
		"top" = list(x = xpad + c(1, dmi[1]), y = c(1, dmi[2])),
		"topright" = list(x = c(dmx[1]-dmi[1]+1, dmx[1]), y = c(1, dmi[2])),
		"left" = list(x = c(1, dmi[1]), y = ypad + c(1, dmi[2])),
		"right" = list(x = c(dmx[1]-dmi[1]+1, dmx[1]), y = c(dmx[2]-dmi[2]+1, dmx[2]) - ypad),
		"bottomleft" = list(x = c(1, dmi[1]), y = c(dmx[2]-dmi[2]+1, dmx[2])),
		"bottom" = list(x = xpad + c(1, dmi[1]), y = c(dmx[2]-dmi[2]+1, dmx[2])),
		"bottomright" = list(x = c(dmx[1]-dmi[1]+1, dmx[1]), y = c(dmx[2]-dmi[2]+1, dmx[2])))

# Calculate index into image and replace
  idx <- Map(function(v) seq.int(v[1], v[2]), pp)
	if (colorMode(img) == Color)
		img[idx$x, idx$y, ] <- inset
	else
		img[idx$x, idx$y] <- inset

# Add frame to elements
#	rect(p$x[1], p$y[1], p$x[2], p$y[2], border = col, lend = lend, lwd = lwd, ...)
#	rect(pp$x[1], pp$y[1], pp$x[2], pp$y[2], border = col, lend = lend, lwd = lwd/prop, ...)

# Write and retrieve the new image
#	ftemp <- tempfile()
#	dev.print(tiff, ftemp, width = dmx[1], height = dmx[2], bg = "transparent")
#	dev.off()
#	img <- readImage(ftemp, type = "tiff")
#	file.remove(ftemp)
	plot(img)
	invisible(img)
}
