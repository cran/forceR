## `forceR` v1.0.18 (Release date: 2022-11-07)

### Minor changes:

-   added `collect_garbage = FALSE` to `convert_measurement()` after the [issue](https://github.com/Peter-T-Ruehr/forceR/issues/1) raised by [Sam](https://github.com/sginot) to possibly prevent the errors he describes.
-   changed CITATION file to meet new bibtext format criteria of CRAN.

### Bug fixes:
-   changed `slice(n=1)` to `slice(1)` in `path.plots()` after Error flags in CRAN Package Check Results.

## `forceR` v1.0.17 (Release date: 2022-11-07)

### Major changes:

-   add parameters `degrees`, `threshold`, and `zero_threshold` to `find_best_fit()`.

### Minor changes:

-   `amp_drift_corr()` now accepts any time step size - though it is still expected that time steps are either constant or varying only to a neglectable degree.
-   results of `amp_drift_corr()` are not rounded anymore.

## `forceR` v1.0.16 (Release date: 2022-06-23)

### Minor changes:

-   made `convert_measurement()` more reliable regarding input column types

## `forceR` v1.0.15 (Release date: 2022-06-07 - *currently on CRAN*)

### Major changes

-   Optimized peak finding by adding/subtracting one time step from bite start/end, respectively, and by making sure that numbers are numeric within the 'find_strongest_peaks()' function
-   Added `peak_duration_max_force()` to calculate duration and maximum force per peak and store it in a tibble with one peak per row

### Minor changes:

-   Added checks of output folder existence to `rescale_peaks()`, `red_peaks_100()`, and `avg_peaks()`
-   Removed the necessity of having the species name in df.peaks of `rescale_peaks()`
-   Added some internal data checks with meaningful error messages to `rescale_peaks()` and `correct_peak()`
-   Disentangled in `plot.to.screen` and `path.plots` `find_best_fits()`
-   Updated vignette
-   Updated README.md
-   added DOI to CITATION

### Bug fixes

-   Corrected input tests of `y_to_force()`

## `forceR` v1.0.14 (Release date: 2022-05-10)

### Major changes

-   Added logo (v.1.0.14)
-   Added this NEWS.md file

### Minor changes:

-   Updated the help file of `y_to_force()` to clarify inputs and improved input checks
-   Added recommendation in help file of `crop_measurement()` and the vignette to not over-crop
-   Added recommendation in the help file of `crop_measurement()` and the vignette to copy RAW files when extracting distinct regions within time series data

### Bug fixes

-   Changed file name extension of output files from "\_cropped" to "\_converted" in `convert_measurement()`

## `forceR` v0.0.13 (Release date: 2022-05-02)

-   First version of `forceR` on CRAN.
