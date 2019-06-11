### Description  
This package collects helper functions to extend the `EBImage` package and help with processing micrographs. These functions are dependent on the `EBImage` package and sometimes make use of the `lattice` package.

### Functional Groups  
#### Extensions to EBImage` 
* circularity - Improved calculation of the circularity of an image object
* perimeter - Improved calculation of the perimeter of image object
* ni - Vectorized wrapper to the `normalize()` function with defaults for 12-bit images

#### Image stacks  
* stackObjects2 - Place detected objects *with* bounding box in an image stack
* plotStack - Plot an image stack with optional labels
* locatorStack - Interact with a plotted image stack to select frames
* labelStack - Label frames of a plotted image stack

#### Filters  
* sobel - [Sobel filter](https://en.wikipedia.org/wiki/Sobel_operator) for edge detection
* usm - [Unsharp mask filter](https://en.wikipedia.org/wiki/Unsharp_masking) to sharpen images

#### Micrograph utilities  
* scaleBar - Add horizontal scale bar to image
* crop - Crop a grayscale or binary image to include only image
* boxtext - Add text with background box to a plot

## License  
GPL-3