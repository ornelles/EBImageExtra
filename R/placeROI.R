##
## placeROI - place a scaled inset (roi) into an image as an inset
##
placeROI <- function(roi, img, position, frac = NULL, mag = NULL,
	frac.default = 1/3, showImage = TRUE)
{
# Check arguments
	if(!is(roi, "Image"))
		stop ("'roi' must be an Image object")
	if (is.character(img)) {
		if (file.exists(img[1]))
			img <- readImage(img)
		else
			stop("'img' is not a valid image filename")
	}
	else if (!is(img, "Image"))
		stop("'img' must be an Image object or valid filename")

# Harmonize colorMode
	cmx <- colorMode(img)
	cm2 <- colorMode(roi)
	if (cmx != cm2) warning("grayscale image promoted to RGB")
	if (cmx > cm2) roi <- toRGB(roi)
	else if (cm2 > cmx) img <- toRGB(img)

# Common variables
	choices <- c("topleft", "top", "topright", "left", "center", "right",
		"bottomleft", "bottom", "bottomright")
	choices <- factor(choices, levels = choices)

	dm.img <- dim(img)[1:2]
	dm.roi <- dim(roi)[1:2]
	cmx <- colorMode(img)

# Determine magnification 'mag' from fractional size 'frac'
	if (is.null(frac) & is.null(mag)) { # given neither 'mag' nor 'frac'
		frac <- frac.default
		mag <- frac * dm.img[1]/dm.roi[1]
	}
	else if (is.null(frac) & !is.null(mag)) { # given 'mag'
		frac <- mag * dm.roi / dm.img
		if (any(frac[1:2] >= 1))
			stop("inset will exceed size of image with mag = ", signif(mag, 2))
		frac <- frac[1]
	}
	else if (!is.null(frac) & is.null(mag)) { # given 'frac'
		if (frac > 1 | frac < 0)
			stop("'frac' must be between 0 and 1")
		frac <- frac
		mag <- frac * dm.img[1]/dm.roi[1]
	}
	else if (!is.null(frac) & !is.null(mag)) { # given both
		if (signif(mag, 3) != signif(frac * dm.img[1]/dm.roi[1]))
			stop("incompatible 'mag' and 'frac' values provided, use one or the other")
		else
			mag <- mag
	}

# Resize inset by mag and recalculate dm.roi
  roi <- resize(roi, w = mag * dm.roi[1])
  dm.roi <- dim(roi)[1:2]

# Determine position for inset from 'position' and 'choices'
	if (!missing(position)) {
			if (is(position, "character")) {
				position <- sub("upper", "top", position)
				position <- sub("lower", "bottom", position)
				position <- match.arg(position, choices)
			}
			else if (is.numeric(position) && position > 0 && position <= length(choices))
				position <- levels(choices)[position]
			else
				stop("'position' must be a character or integer in 1 through 9")
	}
	else {
		nx <- 3 # three "zones" across and down
		vx <- seq(0, dm.img[1] + 1, length = nx + 1)
		vy <- seq(0, dm.img[2] + 1, length = nx + 1)
		i <- 0
		cat("Click near the corner or side to have inset\n")
		flush.console()
		while (!i %in% seq_len(nx*nx)) {
			p <- locator(1)
			ix <- findInterval(p$x, vx)
			iy <- findInterval(p$y, vy)
			i <- ix + nx*(iy - 1)
		}
		position <- levels(choices)[i]
	}

# Calculate translation adjustment for given position
	pad <- dm.img - dm.roi
	offset <- switch(as.character(position),
		"topleft" = c(0, 0), "top" = c(0.5, 0), "topright" = c(1, 0),
		"left" = c(0, 0.5), "center" = c(0.5, 0.5), "right" = c(1, 0.5),
		"bottomleft" = c(0, 1), "bottom" = c(0.5, 1), "bottomright" = c(1, 1))
	
# Assemble image by creating masks and using array math
	mask <- Image("black", dim = dm.roi, colormode = 2)
	mask <- translate(mask, offset * pad, output.dim = dm.img, bg.col = "white")
	roi <- translate(roi, offset * pad, output.dim = dm.img, bg.col = "black")
	ans <- mask * img + roi
	if (showImage == TRUE)
		plot(ans)
	invisible(ans)
}
