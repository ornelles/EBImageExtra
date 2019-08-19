### Description  
This is a collection of functions to extend the `EBImage` package and provide helper functions for processing micrographs. These functions require the `EBImage` package and are typically used with the `virusTiter` package. The `virusTiter` package requires the `lattice` and `latticeExtra` packages.

---  
### Functional Groups

#### Extensions to EBImage
* `circularity` - more precisely calculate the circularity of an image object
* `perimeter` - more precisely calculate the perimeter of an image object
* `ni` - a vectorized wrapper to the `normalize()` function with defaults for 12-bit images

#### Image stacks  
* `stackObjects2` - place detected objects *with* bounding box in an image stack
* `interleave` - reorganize an image stack by interleaving the images
* `plotStack` - plot an image stack with optional labels
* `locatorStack` - interact with a plotted image stack to select frames
* `labelStack` - label frames of a plotted image stack

#### Filters  
* `sobel` - the [Sobel filter](https://en.wikipedia.org/wiki/Sobel_operator) for edge detection
* usm - the [Unsharp mask filter](https://en.wikipedia.org/wiki/Unsharp_masking) to sharpen images by increasing local contrast

#### Micrograph utilities  
* `scaleBar` - add horizontal scale bar to image
* `crop` - crop a grayscale or binary image to include non-zero pixels
* `boxtext` - add text with background box to a plot
* `inset` - *function to be added...maybe* add a framed inset to an image by repeated use of getROI, putROI and drawROI to select a region from an image, draw a frame about the selected region, place the selection as an inset, and draw a frame about the inset

At the moment, `inset` can be emulated by something like the following:
```
library(magrittr)
plot(img)
ins <- getROI(img)
ans <- putROI(img, ins, "topright", lwd = 4) %>% drawROI(ins, lwd = 2)
plot(ans)
```
#### Region of interest management
`EBImage` uses the `S4` `Image` class to store and process images. A region of interest is an `Image` object with the additional class named `Roi` and a slot named `loc` to store the original `x,y` and `x2,y2` location of the rectangular region of interest. This class of objects allows the image and original location to be passed in one object. 

* `getROI` - get a region of interest
* `putROI` - put a region of interest (with scaling) at one of nine locations in an image
* `drawROI` - draw a frame *around* or *within* an image to highlight the region of interest 
* `as.Roi` - convert `Image` object to `Roi` object or reset the `loc` slot of an `Roi` object to the image dimensions
---
## License  
GPL-3