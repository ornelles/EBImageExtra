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
* getInset - Extract rectangular inset from image
* putInset - Place inset into an image (TO-DO) 

## License  
GPL-3