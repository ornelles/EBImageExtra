#include <Rcpp.h>
#include <vector>
#include <cmath>
#include <cstdint>
#include <algorithm>

using namespace Rcpp;

// [[Rcpp::plugins(cpp11)]]


struct Metrics {
    std::uint64_t area;
    double perimeter;
};


// Calculate object area and perimeter
Metrics circularity_metrics(const IntegerMatrix& x)
{
    const int nr = x.nrow();
    const int nc = x.ncol();

    if (nr == 0 || nc == 0)
        return Metrics{0, 0.0};

    // Edge status for previous and current columns
    std::vector<unsigned char> prev(nr, 0);
    std::vector<unsigned char> curr(nr, 0);

    // Direct pointer to R's column-major matrix data
    const int* p = x.begin();

    std::uint64_t area = 0;

    // Connections between edge pixels used by the
    // original perimeter estimator
    std::uint64_t orthogonal = 0;
    std::uint64_t diagonal   = 0;

    // Number of adjacent pairs of object pixels.
    // This allows calculation of the exposed-edge perimeter:
    //
    //      P = 4 * area - 2 * adjacent_pairs
    //
    std::uint64_t object_adjacencies = 0;

    // Used to recognize objects too thin for the
    // edge-adjacency perimeter estimator
    std::uint64_t interior_pixels = 0;


    for (int c = 0; c < nc; ++c) {

        const R_xlen_t base =
            static_cast<R_xlen_t>(c) * nr;


        // Identify edge pixels in the current column
        for (int r = 0; r < nr; ++r) {

            const R_xlen_t i = base + r;

            if (p[i] == 0) {
                curr[r] = 0;
                continue;
            }

            ++area;


            // Count each orthogonal object-object
            // adjacency exactly once
            if (r > 0 && p[i - 1] != 0)
                ++object_adjacencies;

            if (c > 0 && p[i - nr] != 0)
                ++object_adjacencies;


            // A pixel is interior if all four
            // orthogonal neighbors are non-zero
            bool interior = false;

            if (r > 0 && r < nr - 1 &&
                c > 0 && c < nc - 1) {

                interior =
                    p[i - 1]  != 0 &&    // up
                    p[i + 1]  != 0 &&    // down
                    p[i - nr] != 0 &&    // left
                    p[i + nr] != 0;      // right
            }


            if (interior) {
                curr[r] = 0;
                ++interior_pixels;
            }
            else {
                curr[r] = 1;
            }
        }


        // Vertical connections between edge pixels
        for (int r = 0; r < nr - 1; ++r) {

            if (curr[r] && curr[r + 1])
                ++orthogonal;
        }


        if (c > 0) {

            // Horizontal connections between edge pixels
            for (int r = 0; r < nr; ++r) {

                if (prev[r] && curr[r])
                    ++orthogonal;
            }


            // Diagonal connections between edge pixels
            for (int r = 0; r < nr - 1; ++r) {

                if (prev[r] && curr[r + 1])
                    ++diagonal;

                if (prev[r + 1] && curr[r])
                    ++diagonal;
            }
        }


        prev.swap(curr);
    }


    double perimeter;


    if (interior_pixels == 0) {

        /*
         * Very small or thin objects have no interior pixels.
         *
         * The normal edge-adjacency algorithm gives an
         * unrealistically short perimeter for these objects.
         *
         * Instead, use the perimeter of the union of the
         * occupied pixels, counting exposed pixel sides.
         */
        perimeter =
            4.0 * static_cast<double>(area) -
            2.0 * static_cast<double>(object_adjacencies);
    }
    else {

        /*
         * Original perimeter estimator.
         *
         * Each connection is counted exactly once here,
         * so division by 2 is unnecessary.
         */
        perimeter =
            static_cast<double>(orthogonal) +
            std::sqrt(2.0) *
            static_cast<double>(diagonal);
    }


    return Metrics{area, perimeter};
}


// [[Rcpp::export]]
double perimeter_cpp(const IntegerMatrix& x)
{
    return circularity_metrics(x).perimeter;
}


// [[Rcpp::export]]
double circularity_cpp(const IntegerMatrix& x)
{
    const Metrics m = circularity_metrics(x);

    // Circularity is undefined for an empty object or
    // an object with zero calculated perimeter.
    if (m.area == 0 || m.perimeter <= 0.0)
        return 0.0;

    const double pi = std::acos(-1.0);

    double circularity =
        4.0 * pi * static_cast<double>(m.area) /
        (m.perimeter * m.perimeter);


    // Numerical/discretization effects can occasionally
    // produce circularity slightly greater than 1.
    if (circularity > 1.0)
        circularity = 1.0;


    return circularity;
}