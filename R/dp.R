#' Plot Device Wrapper
#' 
#' This is a wrapper to plot the current device to a file.
#' 
#' @param path file path to save the device window. If missing, 
#'   \code{\link{file.choose}} will be called to select the file#' @param save \code{logical} value to save image. If missing, the global 
#'   environment will be searched for a \code{logical} value named 'SaveImage'#' @param width device width in units specified by \code{units}. If missing
#'   the width of the current device will be used#' @param dev suitable printing device, defaults to \code{\link{png}} but 
#'   can be one of \code{\link{bmp}}, \code{\link{jpeg}}, or \code{\link{tiff}}. 
#'   If not specified, the device will be determined by the path extension#' @param res resolution in pixels per \code{units}, default of 300#' @param units the units in which \code{width} is given. Defaults to "in" 
#'   (inches) rather than the normal default of \code{px} (pixels)#' @param ... additional paramters passed to \code{\link{dev.print}}
#' 
#' @details
#' 
#' @seealso
#' \code{\link{dev.print}} that is called by this function
#' 
#' @return
#' The basename of the saved file will be returned or \code{NULL}
#' if nothing was saved.
#' 
#' @import EBImage
#' 
#' @export
#' 
dp <- function(path, save = SaveImage, width, dev, res = 300, units = "in", ...)
{
	if (missing(save))
		if (exists("SaveImage", mode = "logical")) save <- SaveImage
		else stop ("no value for 'save' was found")
	save <- as.logical(SaveImage)
	if (dev.cur() == 1 || save == FALSE) return() # nothing to save
	if (missing(width)) width <- dev.size()[1]
	if (missing(path)) path <- file.choose(T)
	path <- path[1]
	if (!grepl("\\.", path)) path <- paste(path, "png", sep = ".")
	m <- regexpr("\\.([[:alnum:]]+)$", path)
	ext <- substring(path, m + 1)
	if (grepl("bmp$", ext, TRUE)) dev <- bmp
	else if (grepl("jpeg$|jpg$", ext, TRUE)) dev <- jpeg
	else if (grepl("tif$|tiff$", ext, TRUE)) dev <- tiff
	else if (grepl("png$", ext, TRUE)) dev <- png
	else stop("can't match device with file type: '", ext, "'")
	dev.print(dev, path, width = width, res = res, units = units, ...)
	return(basename(path))
}
