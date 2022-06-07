#' Get Today's Date as String
#'
#' Creates a character string containing today's date
#' in the format "yyyy_mm_dd" which can be used in file names.
#'
#'
#' @return
#' |  |  |
#' | --- | --- |
#' | `date.string` | a character string of today's date in the format "yyyy_mm_dd" |
#'
#' @importFrom magrittr '%>%'
#' @importFrom stringr str_sub str_split
#' @importFrom readr read_delim read_csv write_csv cols
#' @importFrom dplyr filter pull select mutate arrange slice group_by ungroup left_join summarize distinct
#' first lead lag case_when bind_cols tibble as_tibble desc progress_estimated bind_rows all_of rename n
#' mutate_at summarise
#' @importFrom stats spline aggregate quantile setNames lm AIC predict complete.cases sd runif rnorm
#' @importFrom graphics points lines locator title par boxplot axis
#' @importFrom grDevices dev.off dev.print pdf
#' @importFrom purrr map_df
#' @importFrom roll roll_mean
#' @importFrom filesstrings file.move
#' @export
today <- function(){
  date.string <- gsub("-", "_", substring(as.character(as.POSIXct(Sys.time())), 1, 10))
  return(date.string)
}

#' Scale data series to new minimum and maximum
#'
#' Maps a series of numeric values to a new range defined by minimum (from) and maximum (to).
#'
#' @param data numeric vector containing the data to be scaled
#' @param from minimum of new range
#' @param to maximum of new range
#' @return numeric vector with scaled data
#' @examples
#' rescale_to_range(data = 1:10,
#'                   from = 1,
#'                   to = 100)
#' @export
rescale_to_range <- function(data, from, to) {
  if(!is.numeric(data)) stop ("'data' must be numeric.")
  if(length(data) < 2) stop ("'data' is not a vector of numbers.")
  if(!is.numeric(from)) stop ("'from' must be numeric.")
  if(length(from) != 1) stop ("'from' must be a single number.")
  if(!is.numeric(to)) stop ("'to' must be numeric.")
  if(length(to) != 1) stop ("'to' must be a single number.")
  data <- data - min(data)
  data <- data / max(data)
  data <- data * (to - from)
  data + from
  return(data)
}

#' Print progress
#'
#' Prints loop progress in `[%]` to console.
#'
#' @param current Numeric value of current loop iteration.
#' @param end Numeric value number last loop iteration.
#' @return Prints percentage of loop progress to console.
#' @export
print_progress <- function(current, end) {
  cat("\r", paste0(round(current/end*100,2), "%.."))
}

#' Simulate bites
#'
#' Simulates either sinusoidal or plateau-like bites.
#'
#' @param no.of.bites Number of bites in time series. Default: `5`.
#' @param length.of.bite Length of each bite. Default: `1000`.
#' @param length.of.series Length of the whole time series. Default: `10000`.
#' @param max.y Maximum y value. Default: `1`.
#' @param max.y.jit Jitter above and below maximum y value in `[%]` of maximum y value. Default: `NULL`.
#' @param jit Jitter along the whole time series. Default: `NULL`.
#' @param peak.pos Position (in percent) of peak within peak curve.
#' Only applies to `bite.type = "sin` (sinusoidal bites.) Default: same as `50`.
#' @param slope.perc.start Percentage of how much of the whole bite is defined by the ascending slope.
#' Only applies to `bite.type = "plat` (plateau-like bites.) Default: `10`.
#' @param slope.perc.end Percentage of how much of the whole bite is defined by the descending slope.
#' Only applies to `bite.type = "plat` (plateau-like bites.) Default: same as `slope.perc.start`.
#' @param bite.type String: either "sin" or "plat" for sinusoidal or plateau-like bites, respectively. Default: `"sin"`.
#' @param plot Logical. If `TRUE`, the simulated time series will be plotted to the active plot device. Default: `TRUE`.
#' @return Returns a tibble with the columns `t` and `y` containing simulated bites.
#' @examples
#' # simulate a time series with sinusoidal bites
#' # where the peaks are located in the centers of the bites.
#' simulate_bites(no.of.bites = 5,
#'                 length.of.bite = 1000,
#'                 length.of.series = 10000,
#'                 max.y = 5,
#'                 max.y.jit = 15,
#'                 jit = 0.5,
#'                 peak.pos = 0.5,
#'                 bite.type = "sin",
#'                 plot = TRUE)
#'
#' # simulate a time series with sinusoidal bites
#' # where the peaks are located towards the ends of the bites.
#' simulate_bites(no.of.bites = 5,
#'                 length.of.bite = 1000,
#'                 length.of.series = 10000,
#'                 max.y = 5,
#'                 max.y.jit = 15,
#'                 jit = 0.5,
#'                 peak.pos = 0.8,
#'                 bite.type = "sin",
#'                 plot = TRUE)
#'
#' # simulate a time series with plateau-like bites
#' simulate_bites(no.of.bites = 5,
#'                 length.of.bite = 1000,
#'                 length.of.series = 10000,
#'                 max.y = 5,
#'                 max.y.jit = 15,
#'                 jit = 1,
#'                 bite.type = "plat",
#'                 plot = TRUE)
#'
#' # simulate a time series with plateau-like bites
#' # with slowly ascending bite start and abprupt bite end.
#' simulate_bites(no.of.bites = 5,
#'                 length.of.bite = 1000,
#'                 length.of.series = 10000,
#'                 max.y = 5,
#'                 max.y.jit = 15,
#'                 slope.perc.start = 60,
#'                 slope.perc.end = 10,
#'                 jit = 1,
#'                 bite.type = "plat",
#'                 plot = TRUE)
#' @export
simulate_bites <- function(no.of.bites = 5,
                           length.of.bite = 1000,
                           length.of.series = 10000,
                           max.y = 1,
                           max.y.jit = NULL,
                           peak.pos = 50,
                           slope.perc.start = 10,
                           slope.perc.end = slope.perc.start,
                           jit = NULL,
                           bite.type = "sin",
                           plot = TRUE){

  if(!is.numeric(no.of.bites)) stop ("'no.of.bites' must be numeric.")
  if(length(no.of.bites) != 1) stop ("'no.of.bites' must be a single number.")

  if(!is.numeric(length.of.bite)) stop ("'length.of.bite' must be numeric.")
  if(length(length.of.bite) != 1) stop ("'length.of.bite' must be a single number.")

  if(!is.numeric(length.of.series)) stop ("'length.of.series' must be numeric.")
  if(length(length.of.series) != 1) stop ("'length.of.series' must be a single number.")

  if(!is.numeric(max.y)) stop ("'max.y' must be numeric.")
  if(length(max.y) != 1) stop ("'max.y' must be a single number.")

  if(!is.numeric(max.y.jit) & !is.null(max.y.jit)) stop ("'max.y.jit' must be numeric or NULL.")
  if(length(max.y.jit) != 1 & !is.null(max.y.jit)) stop ("'max.y.jit' must be a single number.")

  if(!is.numeric(jit) & !is.null(jit)) stop ("'jit' must be numeric or NULL.")
  if(length(jit) != 1 & !is.null(jit)) stop ("'jit' must be a single number.")

  if(!is.character(bite.type)) stop ("'bite.type' must be a character string.")
  if(length(bite.type) != 1) stop ("'bite.type' must only contain one character string.")
  if(!(bite.type == "sin") & !(bite.type == "plat")) stop ("'bite.type' can only be 'sin' or 'plat'.")

  if(!is.logical(plot)) stop ("'plot' must be logical.")

  if(bite.type == "plat"){
    if((100-slope.perc.start-slope.perc.end) < 0) stop("The percentages of 'slope.perc.start' and 'slope.perc.end' add up to more than 100%.
  Please reduce at least one of these values.")

    if(!is.numeric(slope.perc.start)) stop ("'slope.perc.start' must be numeric.")
    if(length(slope.perc.start) != 1) stop ("'slope.perc.start' must be a single number.")

    if(!is.numeric(slope.perc.end)) stop ("'slope.perc.end' must be numeric.")
    if(length(slope.perc.end) != 1) stop ("'slope.perc.end' must be a single number.")

    if(slope.perc.start < 1) warning ("'slope.perc.start' is smaller than 1%. Please check if this is correct.")
    if(slope.perc.end < 1) warning ("'slope.perc.end' is smaller than 1%. Please check if this is correct.")

    slope.perc.start <- slope.perc.start/100
    slope.perc.end <- slope.perc.end/100
  } else{
    if(bite.type == "sin") if(peak.pos < 1) warning ("'peak.pos' is smaller than 1%. Please check if this is correct.")

    if(!is.numeric(peak.pos)) stop ("'peak.pos' must be numeric.")
    if(length(peak.pos) != 1) stop ("'peak.pos' must be a single number.")

    peak.pos <- peak.pos/100
  }


  dist.between.bites <- floor((length.of.series - no.of.bites * length.of.bite)/no.of.bites)

  if(dist.between.bites < 0) stop('Distance between bites < 0.
  Reduce number of bites, or reduce bite length, or increase length of measurement')
  if(dist.between.bites == 0) warning('Distance between bites = 0.')

  y <- NULL
  for(i in 1:no.of.bites){
    if(bite.type == "sin"){
      curr.bite.shape <- c(sin(seq(-1.5, 1.5, length.out = peak.pos*length.of.bite)),
                           sin(seq(1.5, 4.5, length.out = (1-peak.pos)*length.of.bite)))
      # plot(curr.bite.shape, type = "l")
    } else if(bite.type == "plat"){
      curr.bite.shape <- c(sin(seq(-1.5, 1.5, length.out = slope.perc.start*length.of.bite)),
                           rep(1, (1 - slope.perc.start - slope.perc.end)*length.of.bite),
                           sin(seq(1.5, 4.5, length.out = slope.perc.end*length.of.bite)))
      # plot(curr.bite.shape, type = "l")
    }


    if(!is.null(max.y.jit)){
      # rescale to 0 - max.y including max.y jitter
      curr.bite.shape <- rescale_to_range(curr.bite.shape, 0, max.y + ((runif(1, -max.y.jit/100, max.y.jit/100))*max.y))
    } else {
      # rescale to 0 - max.y without max.y jitter
      curr.bite.shape <- rescale_to_range(curr.bite.shape, 0, max.y)
    }


    if(length(curr.bite.shape) > length.of.bite){
      curr.bite.shape <- curr.bite.shape[-1]
    } else if (length(curr.bite.shape) < length.of.bite){
      curr.bite.shape <- c(curr.bite.shape, 0)
    }

    # add jitter
    if(!is.null(jit)){
      curr.bite.shape <- curr.bite.shape + rnorm(length.of.bite, -jit/100*max.y, jit/100*max.y)
    }

    y <- c(y, curr.bite.shape, rep(0, dist.between.bites))
  }
  y <- c(rep(0, dist.between.bites/2), y[-c(((length(y)+1)-dist.between.bites/2):length(y))])

  if(length(y) < length.of.series){
    y <- c(y, rep(0, length.of.series - length(y)))
  }

  df <- tibble(t = 1:length.of.series,
               y = y)
  if(plot == TRUE){
    plot(df$t, df$y, type = "l")
    lines(c(0, length.of.series), c(max.y, max.y), lty = 2, col = "gray70")
  }

  return(df)
}

#' Get path to forceR example
#'
#' forceR comes with example files of short bite force measurements.
#' The files are stored in `forceR's` `inst/extdata`
#' folder, and this function returns the path to that folder or one of the
#' files  so they can be used in examples.
#' @param type A character string (either `"folder"`, `"raw"`, or
#' `"ampdriftcorr"`) defining if the path returned be the function
#' should point to one of the the example files or the folder containing them.
#' Default: `"folder"`.
#' @return
#' If `type = "folder"`: returns the file path to the folder containing
#' `BF_raw.csv` and
#' `BF_ampdriftcorr.csv`.
#'
#' If `type = "LJStream"`: returns the file path
#' to `BF_raw.csv`, which contains a short bite force raw
#' measurement.
#'
#' If `type = "raw"`: returns the file path
#' to `BF_raw.csv`, which contains a short bite force raw
#' measurement.
#'
#' If `type = "ampdriftcorr"`: returns the file path
#' to `BF_ampdriftcorr.csv`, which contains a short bite
#' force raw measurement where the amplifier drift as been corrected for by
#' the amp_drift_corr() funcion.

#' @export
forceR_example <- function(type = "folder"){
  if(type == "raw"){
  path <- system.file("extdata", "raw/0001_C_big.csv",
                      package = "forceR",
              mustWork = TRUE)
  } else if(type == "LJStream"){
    path <- system.file("extdata", "LJStream/0001_C_big_LJStream_0.dat",
                        package = "forceR",
                        mustWork = TRUE)
  } else if(type == "ampdriftcorr"){
    path <- system.file("extdata", "ampdriftcorr/0001_C_big_ampdriftcorr.csv",
                        package = "forceR",
                        mustWork = TRUE)
  } else if(type == "folder"){
    path <- system.file("extdata", "raw",
                        package = "forceR",
                        mustWork = TRUE)
  } else {
    stop (
      '"path" must be either "folder", "raw", or "ampdriftcorr".'
      )
  }
  return(path)
}
