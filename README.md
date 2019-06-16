### Description  
This is a collection of functions to extend the `EBImage` package and provide helper functions for  processing micrographs. These functions are dependent on the `EBImage` package which requires the `lattice` and `latticeExtra` packages.

### Grouped by Functionality
#### Extensions to EBImage` 
* circularity - more precise calculation of the circularity of an image object
* perimeter - more precise calculation of the perimeter of an image object
* ni - vectorized wrapper to the `normalize()` function with defaults for 12-bit images

#### Image stacks  
* stackObjects2 - place detected objects *with* bounding box in an image stack
* plotStack - plot an image stack with optional labels
* locatorStack - interact with a plotted image stack to select frames
* labelStack - label frames of a plotted image stack
* tStack - "transpose" an image stack by reordering the images

#### Filters  
* sobel - the [Sobel filter](https://en.wikipedia.org/wiki/Sobel_operator) for edge detection
* usm - the [Unsharp mask filter](https://en.wikipedia.org/wiki/Unsharp_masking) to sharpen images by increasing local contrast

#### Micrograph utilities  
* scaleBar - add horizontal scale bar to image
* crop - crop a grayscale or binary image to include non-zero pixels
* boxtext - add text with background box to a plot

#### Region of interest management
`EBImage` uses the `S4` `Image` class to store and process images. A region of interest is an `Image` object with the additional class named `Roi` and a slot named `loc` to store the original `x,y` and `x2,y2` location of the rectangular region of interest. This class of objects allows the image and original location to be passed in one object. 

* getROI - get a region of interest
* putROI - put a region of interest (with scaling) at one of nine locations in an image
* drawROI - draw a frame *within* an image, typically highlighting the region of interest 
* frameROI - draw a frame *around* an image
* insertROI - a convenience function that makes repeated use of getROI, putROI and drawROI to select a region from an image, draw a frame about the selected region, place the selection as an inset and draw a frame about the inset

## License  
GPL-3