frameROI <- function(x, width = 2, col = "white", sides = 1:4)
{
	if (!is(x, "Image"))
		stop("'x' must be an Image object")
	if (is(col, "numeric"))
		col <- palette()[col]
	dm <- dim(x)[1:2]
	
# Identify borders and create new dimension
	nx <- sum(c(2,4) %in% sides)
	ny <- sum(c(1,3) %in% sides)
	dm2 <- dm - c(nx, ny)*width
	if (any(dm2 < 1))
		stop("'width' is too large to use")

# replace border pixels with chosen color
	ans <- resize(x, w = dm2[1], h = dm2[2], output.dim = dm2)
	xborder <- Image(col, c(dm2[1], width), colormode = colorMode(x))
	yborder <- Image(col, c(width, dm[2]), colormode = colorMode(x))
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
