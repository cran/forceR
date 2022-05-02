#' Simulated Force Measurements with Taxonomic Info.
#'
#' A series of 24 measurements with six simulated peaks each.
#' Simulated using the `simulate_bites()` function .
#' Fits the `classifier` sheet that is also part of the `forceR` package.
#'
#' @format A data frame with 9,600 rows and 3 columns:
#' \describe{
#'   \item{t}{time, in ms}
#'   \item{y}{measured values, in V}
#'   \item{measurement}{measurement names}
#' }
"df.all"

#' Simulated Time Series - e.g. Bite Force Measurements
#'
#' A series of 24 measurements with six simulated peaks each.
#' Simulated using the `simulate_bites()` function. Reduced to a sampling
#' frequency of 200 Hz with `reduce_frq()`. Fits the `classifier` sheet that
#' is also part of the `forceR` package.
#'
#' @format A data frame with 1,944 rows and 3 columns:
#' \describe{
#'   \item{t}{time, in ms}
#'   \item{y}{measured values, in V}
#'   \item{measurement}{measurement names}
#' }
"df.all.200"

#' Simulated Force Measurements with Taxonomic Info.
#'
#' A series of 24 measurements with six simulated peaks each.
#' Simulated using the `simulate_bites()` function and translated into force
#' values with `y_to_force()`. Fits the `classifier` sheet that
#' is also part of the `forceR` package.
#'
#' @format A data frame with 1,944 rows and 5 columns:
#' \describe{
#'   \item{species}{species names}
#'   \item{specimen}{specimen names}
#'   \item{measurement}{measurement names}
#'   \item{t}{time, in ms}
#'   \item{force}{measured values, in N}
#' }
"df.all.200.tax"

#' Polynomial Models Describing Average Peak Shapes.
#'
#' Extracted from `df.all`.
#'
#' @format A list with 4 named entries, each containing the 4-degree polynomial
#' model that describes the average peak shape of the given species.
"models"

#' Average Peak Shapes per Species
#'
#' Normalized force values describing the average shape of the 5 strongest peaks
#' per species of `df.all`
#'
#' @format A data frame with 400 rows and 3 columns:
#' \describe{
#'   \item{species}{species names}
#'   \item{index}{values from 1:100 in each species}
#'   \item{force.norm.100.avg}{normalized values reduced to 100 observations per
#'   species, in N}
#' }
"peaks.df.100.avg"

#' Normalized Peak Shapes with 100 Observations
#'
#' Normalized force values describing the shapes of all 5 strongest peaks
#' per species of `df.all`, reduced to 100 observations per measurement.
#'
#' Result of `rescale_peaks()`.
#'
#' @format A data frame with 2000 rows and 6 columns:
#' \describe{
#'   \item{species}{species names}
#'   \item{measurement}{measurement names}
#'   \item{specimen}{specimen names}
#'   \item{peak}{peak numbers}
#'   \item{index}{values from 1:100 in each measurement}
#'   \item{force.norm.100}{normalized values reduced to 100 observations per
#'   measurement, in N}
#' }
"peaks.df.norm.100"

#' Normalized Peak Shapes
#'
#' Normalized force values describing the shapes of all 5 strongest peaks
#' per species of `df.all`
#'
#' Result of `rescale_peaks()`.
#'
#' @format A data frame with 223 rows and 6 columns:
#' \describe{
#'   \item{measurement}{measurement names}
#'   \item{peak}{peak numbers}
#'   \item{t.norm}{time values from 0:1 in each measurement, in ms}
#'   \item{force.norm}{force values from 0:1 in each measurement, in N}
#'   \item{species}{species names}
#'   \item{specimen}{specimen names}
#' }
"peaks.df.norm"

#' Starts and Ends of the 5 Strongest Peaks
#'
#' Start and end time values of the 5 strongest peaks per species of `df.all`
#' with the names of the measurements in which they occur.
#'
#' Result of `rescale_peaks()`.
#'
#' @format A data frame with 4 rows and 4 columns:
#' \describe{
#'   \item{species}{species names}
#'   \item{measurements}{measurement names}
#'   \item{starts}{start values, in ms}
#'   \item{ends}{end values, in ms}
#' }
"peaks.df"

#' Classifier
#'
#' Start and end time values of the 5 strongest peaks per species of `df.all`
#' with the names of the measurements in which they occur.
#'
#' Result of `rescale_peaks()`.
#'
#' @format A data frame with 24 rows and 5 columns:
#' \describe{
#'   \item{species}{species names}
#'   \item{specimen}{specimen names}
#'   \item{measurement}{measurement names}
#'   \item{amp}{amplifier values, in V/N}
#'   \item{lever.ratio}{ration of on-lever to out-lever of mesaurement setup}
#' }
"classifier"
