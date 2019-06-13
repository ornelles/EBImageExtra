##
## putInset - add a scaled inset to the given image and return marked-up
##
putInset <- function(img, ins, position, frac = NULL, mag = NULL,
	col = "white", lwd = 2, lend = "square", res = 300, frac.default = 1/3,
	...)
{
# argument checks
	if(!is(img, "Image") | !is(ins, "Image"))
		stop ("both 'img' and 'ins' must be Image objects")
	cmx <- colorMode(img)
	cm2 <- colorMode(ins)
	if (cm1 != cm2)
		warning("grayscale image converted to RGB")
	if (cmx > cm2)
		ins <- toRGB(ins)
	else if (cm2 > cmx)
		img <- toRGB(img)

# Common variables
	choices <- c("topleft", "top", "topright", "left", "center", "right",
		"bottomleft", "bottom", "bottomright")
	choices <- factor(choices, levels = choices)

	dm.img <- dim(img)[1:2]
	dm.ins <- dim(ins)[1:2]
	cmx <- colorMode(img)

# Determine magnification 'mag' from fractional size 'frac'
	if (is.null(frac) & is.null(mag)) { # given neither 'mag' nor 'frac'
		frac <- frac.default
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
			mag <- mag
	}

# Resize inset by mag and recalculate dm.ins
  ins <- resize(ins, w = mag * dm.ins[1])
  dm.ins <- dim(ins)[1:2]

# Determine position for inset from 'position' and 'choices'
	if (!missing(position)) {
			if (is(position, "character"))
				position <- match.arg(position, CHOICES)
			else if (is.numeric(position) && position > 0 && position <= length(choices))
				position <- levels(choices)[position]
			else
				stop("'position' must be a character or integer in [1,9]")
	}
	else {
		nx <- 3 # number of tiles across and down
		vx <- seq(0, dm.img[1] + 1, length = nx + 1)
		vy <- seq(0, dm.img[2] + 1, length = nx + 1)
		i <- 0
		while (!i %in% seq_len(nx*nx)) {
			p <- locator(1)
			ix <- findInterval(p$x, vx)
			iy <- findInterval(p$y, vy)
			i <- ix + nx*(iy - 1)
		}
		position <- levels(choices)[i]
	}

# Calculate translation adjustment for given position
	pad <- dm.img - dm.ins
	offset <- switch(position,
		"topleft" = c(0, 0),
		"top" = c(0.5, 0),
		"topright" = c(1, 0),
		"left" = c(0, 0.5),
		"center" = c(0.5, 0.5),
		"right" = c(1, 0.5),
		"bottomleft" = c(0, 1),
		"bottom" = c(0.5, 1),
		"bottomright" = c(1, 1))
	
	mask <- Image("black", dim = dm.ins, colormode = 2)
	mask <- translate(mask, offset * pad, output.dim = dm.img, bg.col = "white")
	ins <- translate(ins, offset * pad, output.dim = dm.img, bg.col = "black")
	ans <- mask * img + ins

# Add possible frame to inset
# Method 1 - replace pixels in situ ## Not quite, how to specify color across matrix
#	inc <- ceiling(lwd)
#	p1 <- lapply(pp, function(v) c(seq(v[1], len = inc), seq(v[2] - inc + 1, len = inc)))
#	p2 <- lapply(pp, function(v) seq(v[1], v[2]))
#	img[p1$x, p2$y, ] <- 1
#	img[p2$x, p1$y, ] <- 1
	
# Method 2 - paint onto image, save and reload ## NOT CORRECT
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
