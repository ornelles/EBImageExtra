### Description  
This is a collection of functions to extend the `EBImage` package and provide helper functions for processing micrographs. These functions require the `EBImage` package and are typically used with the `virustiter` package. The `virustiter` package requires the `lattice` and `latticeExtra` packages.

---  
### Functional Groups

#### Extensions to EBImage
* `circularity` - more precisely calculate the circularity of an image object
* `perimeter` - more precisely calculate the perimeter of an image object
* `ni` - a vectorized wrapper to the `normalize()` function with defaults for 12-bit images
* `bresenham` - Bresenham's integer line "plotting" algorithm
* `pnpoly` - determine if points are within a polygon (algorithm of W. Randolph Franklin)

#### Image stacks  
* `stackObjects2` - place detected objects *with* the bounding box in an image stack
* `interleave` - reorganize an image stack by interleaving the images
* `plotStack` - plot an image stack with optional labels
* `locatorStack` - interact with a plotted image stack to select frames
* `labelStack` - label frames of a plotted image stack

#### Filters  
* `sobel` - the [Sobel filter](https://en.wikipedia.org/wiki/Sobel_operator) for edge detection
* `usm` - the [Unsharp mask filter](https://en.wikipedia.org/wiki/Unsharp_masking) to sharpen images by increasing local contrast

#### Region of interest management
`EBImage` uses the `S4` `Image` class to store and process images. A region of interest is an `Image` object with the additional class named `Roi` and a slot named `loc` to store the original `x,y` and `x2,y2` location of the rectangular region of interest. This class of objects allows the image and original location to be passed in one object. 

* `getROI` - get a region of interest
* `putROI` - put a region of interest (with scaling) at one of nine locations in an image
* `drawROI` - draw a frame *around* or *within* an image to highlight the region of interest 
* `as.Roi` - convert `Image` object to `Roi` object or reset the `loc` slot of an `Roi` object to the image dimensions

#### Micrograph utilities  
* `scaleBar` - add horizontal scale bar to image
* `crop` - crop a grayscale or binary image to exclude zero pixels at the edges
* `boxtext` - add text with a shaded background box to a plot
* `dp` - wrapper to call `dev.print` for the current device window where the global logical variable `SaveImage` can be used to determine whether a file will be saved or not
* `inset` - *function to be added...maybe* add a framed inset to an image by sequential use of getROI, drawROI, and putROI to select a region from an image, draw a frame about the selected region and then place the selection as a framed inset

At the moment, `inset` can be emulated by something like the following:
```
  plot(img) # assuming 'img' holds an Image object
  ins <- getROI(img)
  ans <- drawROI(img, ins, lwd = 2)
  ans <- putROI(ans, ins, "topright", lwd = 4)
  plot(ans)
```
---
## License  
GPL-3
