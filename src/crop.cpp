#include <Rcpp.h>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]


// [[Rcpp::export]]
NumericMatrix crop_cpp(const NumericMatrix& img,
                       int border = 1,
                       double fill = 0.0)
{
    const int nr = img.nrow();
    const int nc = img.ncol();

    if (border < 0)
        stop("'border' must be non-negative");

    const double* p = img.begin();

    // Bounding box of pixels > 0
    int rmin = nr;
    int rmax = -1;
    int cmin = nc;
    int cmax = -1;


    // Find the smallest rectangle containing all pixels > 0.
    //
    // R matrices are column-major, so traverse each column
    // sequentially through memory.
    for (int c = 0; c < nc; ++c) {

        const R_xlen_t base =
            static_cast<R_xlen_t>(c) * nr;

        for (int r = 0; r < nr; ++r) {

            if (p[base + r] > 0.0) {

                if (r < rmin) rmin = r;
                if (r > rmax) rmax = r;

                if (c < cmin) cmin = c;
                if (c > cmax) cmax = c;
            }
        }
    }


    // No non-zero pixels.
    //
    // This reproduces the behavior of the original function:
    // return an image consisting only of the requested border.
    if (rmax < rmin || cmax < cmin) {

        NumericMatrix out(2 * border, 2 * border);

        if (fill != 0.0)
            std::fill(out.begin(), out.end(), fill);

        return out;
    }


    const int crop_nr = rmax - rmin + 1;
    const int crop_nc = cmax - cmin + 1;

    const int out_nr = crop_nr + 2 * border;
    const int out_nc = crop_nc + 2 * border;


    NumericMatrix out(out_nr, out_nc);

    // NumericMatrix is initialized to zero, so filling is
    // necessary only when a non-zero background is requested.
    if (fill != 0.0)
        std::fill(out.begin(), out.end(), fill);


    double* q = out.begin();


    // Copy the cropped image.
    //
    // Each column is contiguous in memory, allowing std::copy()
    // to copy an entire column segment at once.
    for (int c = 0; c < crop_nc; ++c) {

        const int source_col = cmin + c;
        const int dest_col   = border + c;

        const double* source =
            p +
            static_cast<R_xlen_t>(source_col) * nr +
            rmin;

        double* dest =
            q +
            static_cast<R_xlen_t>(dest_col) * out_nr +
            border;

        std::copy(
            source,
            source + crop_nr,
            dest
        );
    }


    return out;
}