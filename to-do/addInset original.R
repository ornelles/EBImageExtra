##
## addInset - add a scaled inset to the given image and return marked-up
## image
##
addInset <- function(img, x = NULL, y = NULL, prop = 0.4, location = NULL,
	res = 300, col = "white", lwd = 2, lend = "square", plot = TRUE, ...)
{
# Extract corners of inset as top left, bottom right
	if (plot == TRUE) {
		dev.new(width = dim(img)[1]/res, height = dim(img)[2]/res)
		par(mar = c(0, 0, 0, 0))
		plot(img)
	}

# Process corners of inset
	if (missing(x) && missing(y)) # interact
		x <- locator(2, type = "p", pch = 3, col = "gray")
	if (!missing(y) &&  is(y, "Image"))
		{img <- y; y <- NULL}
  p <- xy.coords(x, y, recycle = TRUE)[1:2]
  p <- lapply(p, round) # convert to integers
	p <- lapply(p, sort) # forces top left to bottom right order
	idx <- lapply(p, function(v) seq(v[1], v[2])) # index of pixels
	
# Extract inset and enlarge according to prop 
  inset <- img[idx$x, idx$y, ] # extracted inset
	width  <- floor(prop * dim(img)[1])
  inset <- resize(inset, w = width)

# Determine coordinates to place the inset
	dmi <- dim(inset)
	dmx <- dim(img)
	location <- match.arg(location, c("topright","topleft","bottomright","bottomleft"))
	pp <- switch(location, 
			"topright" = list(x = c(dmx[1]-dmi[1]+1, dmx[1]), y = c(1, dmi[2])),
			"topleft" = list(x = c(1, dmi[1]), y = c(1, dmi[2])),
			"bottomright" = list(x = c(dmx[1]-dmi[1]+1, dmx[1]), y = c(dmx[2]-dmi[2]+1, dmx[2])),
			"bottomleft" = list(x = c(1, dmi[1]), y = c(dmx[2]-dmi[2]+1, dmx[2])))

# Calculate index into image and replace
  idx <- Map(function(v) seq.int(v[1], v[2]), pp)
	img[idx$x, idx$y, ] <- inset
	if (plot != TRUE) {
		dev.new(width = dmx[1]/res, height = dmx[2]/res)
		par(mar = c(0, 0, 0 ,0))
	}
	plot(img)

# Add frame to elements
	rect(p$x[1], p$y[1], p$x[2], p$y[2], border = col, lend = lend, lwd = lwd, ...)
	rect(pp$x[1], pp$y[1], pp$x[2], pp$y[2], border = col, lend = lend, lwd = lwd/prop, ...)

# Write and retrieve the new image
	ftemp <- tempfile()
	dev.print(tiff, ftemp, width = dmx[1], height = dmx[2], bg = "transparent")
	dev.off()
	img <- readImage(ftemp, type = "tiff")
	file.remove(ftemp)
	invisible(img)
}