### Description  
This is a collection of functions to extend the `EBImage` package and provide helper functions for  processing micrographs. These functions are dependent on the `EBImage` package and sometimes make use of the `lattice` package.

### Functional Groups  
#### Extensions to EBImage` 
* circularity - More precise calculation of the circularity of an image object
* perimeter - More precise calculation of the perimeter of an image object
* ni - Vectorized wrapper to the `normalize()` function with defaults for 12-bit images

#### Image stacks  
* stackObjects2 - Place detected objects *with* bounding box in an image stack
* plotStack - Plot an image stack with optional labels
* locatorStack - Interact with a plotted image stack to select frames
* labelStack - Label frames of a plotted image stack

#### Filters  
* sobel - [Sobel filter](https://en.wikipedia.org/wiki/Sobel_operator) for edge detection
* usm - [Unsharp mask filter](https://en.wikipedia.org/wiki/Unsharp_masking) to sharpen images by increasing local contrast

#### Micrograph utilities  
* scaleBar - Add horizontal scale bar to image
* crop - Crop a grayscale or binary image to include non-zero pixels
* boxtext - Add text with background box to a plot

#### Region of interest management
`EBImage` uses the S4 `Image` class to store and process images. A region of interest is an `Image` object with the additional class of "`roi`" and the additional slot named "`loc`" to store the original `x,y` and `x2,y2` location of the region of interest. The following code uses this convenience to pass both image and location in one object. 

* getROI - Get a region of interest
* putROI - Put a region of interest (with scaling) at one of nine locations
* drawROI - Draw a frame about (the coordinates) for a region of interest 
* insertROI - Combination of getROI, putROI and drawROI to select a region from an image, draw a frame about the selected region, place the selection as an inset with a frame

## License  
GPL-3