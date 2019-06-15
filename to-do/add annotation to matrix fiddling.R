img <- readImage(file.choose())

p <- getROI(img, asImage = FALSE)

col <- "lightgreen"
w <- 4
p <- lapply(1:4, function(i) locator(2, type = "p", pch = 3))
ans <- img
for (p in v) {
	p <- lapply(p, round)
	pp <- lapply(p, function(v) list(seq(v[1], len = w), seq(v[2]-w+1, len = w)))
	pp2 <- lapply(p, function(v) v[1]:v[2])

	m <- Image(TRUE, dim(img)[1:2])
	S1 <- col(m) %in% pp$y[[2]] & row(m) %in% pp2$x
	S3 <- col(m) %in% pp$y[[1]] & row(m) %in% pp2$x
	S2 <- row(m) %in% pp$x[[1]] & col(m) %in% pp2$y
	S4 <- row(m) %in% pp$x[[2]] & col(m) %in% pp2$y
	m[S1 | S2 | S3 | S4] <- FALSE

#	m[S3 | S4] <- TRUE # to remove border on side 3 and 4...

	if (colorMode(img) == Color)
		M <- abind(m, m, m, along = 3)
	else
		M <- m

	col <- "white"
	mask <- Image(col, dim = dim(img)[1:2], colormode = 2)
	ans <- ans * M + mask * !M
	plot(ans)
}


