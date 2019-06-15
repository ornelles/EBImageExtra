frameROI <- function(img, lwd = 2, col = "white", sides = 1:4)
{
	if (!is(img, "Image"))
		stop("'img' must be an Image object")
	if (is(col, "numeric"))
		col <- palette()[col]
	
# Identify borders and be sure size is appropriate
	dm <- dim(img)[1:2]
	nx <- sum(c(2,4) %in% sides)
	ny <- sum(c(1,3) %in% sides)
	dm2 <- dm - c(nx, ny)*lwd
	if (any(dm2 < 1))
		stop("'lwd' is too large to use")

# extract corners of image 
	pp <- list(x = c(1, dm[1]), y = c(1, dm[2]))
  pp <- lapply(pp, sort)

# create two list of border coordinates
	pp1 <- lapply(pp, function(v) list(seq(v[1], len = lwd),
		seq(v[2] - lwd + 1, len = lwd)))
	pp2 <- lapply(pp, function(v) v[1]:v[2])

# create logical masks to define each side of the border
	m <- Image(TRUE, dim(img)[1:2])
	S1 <- col(m) %in% pp1$y[[2]] & row(m) %in% pp2$x
	S3 <- col(m) %in% pp1$y[[1]] & row(m) %in% pp2$x
	S2 <- row(m) %in% pp1$x[[1]] & col(m) %in% pp2$y
	S4 <- row(m) %in% pp1$x[[2]] & col(m) %in% pp2$y
	S <- list(S1, S2, S3, S4)

# paint border areas as FALSE (0) in the logical mask
	m[Reduce(`|`, S[1:4 %in% sides])] <- FALSE

# convert the logical mask to a de-facto binary image  
	if (colorMode(img) == Color)
		M <- abind(m, m, m, along = 3)
	else
		M <- m

# combine with solid colored image, replace appropriate pixesl in image
	mask <- Image(col, dim = dim(img)[1:2], colormode = colorMode(img))
	ans <- img * M + mask * !M
	return(ans)
}
