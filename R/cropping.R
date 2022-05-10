#' Convert LJStream *.dat file to standard time series
#'
#' Converts LJStream *.dat file to standard time series.
#'
#' @param file File path to raw measurement (*.dat file).
#'
#' @param path.data A string character defining where to save the results. If `NULL`,
#' data is not stored in a file. Default: `NULL`.
#'
#' @return Returns and, if `path.data is not NULL`, saves data in
#' csv-format in `path.data`. \cr
#' \cr
#' The output tibble has the following format:
#'
#' | **`t`** |   | **`y`** |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.1` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#' @examples
#' # get file path of forceR example file
#' filename <- forceR_example(type = "LJStream")
#' file.converted <- convert_measurement (file = filename,
#'                        path.data = NULL)
#' file.converted
#'
#' @export
convert_measurement <- function (file,
                                 path.data = NULL){

  # # testing
  # convert_measurement (file = forceR_example(type = "LJStream"),
  #                                  path.data = NULL)
  # convert_measurement (file = forceR_example(type = "LJStream"),
  #                                  path.data = "./test_folder")

  if(!is.character(file)) stop ("'file' must be a character string.")

  # dplyr NULLs
  Time <- y0 <- NULL

  if(!file.exists(file)) stop(paste0("File ", file, " does not exist!"))

  if(!is.null(path.data)){
    if(!dir.exists(path.data)) stop ("Folder to store plots does not exist: ", path.data, ".")
  }

  res.reduction <- 10
  file.name <- basename(file)

  data <- read_delim(file, delim = "\t", skip = 6,
                     show_col_types = FALSE)

  # Check what the decimal is. Returns "" when it is a full stop, and the
  #   decimal, e.g. "," if it is not.
  decimal <- gsub('[[:digit:]]+', '', data[1,1])

  if(decimal != ""){
    # print(paste0("Converting \'", decimal, "\' to \'.\'..."))
    data$Time <- as.numeric(gsub(decimal, ".", gsub("\\.", "", data$Time))) * 1000
    data$y0 <- as.numeric(gsub(decimal, ".", gsub("\\.", "", data$y0)))
  }

  data <- data %>% as_tibble() %>%
    select(Time, y0) %>%
    rename(t = Time, y = y0)

  # if(write.files == TRUE){
  if(!is.null(path.data)){
    write_csv(data, file.path(path.data,
                              paste0(gsub("\\.dat$", "_converted\\.csv", basename(file)))),
              quote = "none")
  }
  return(data)
  # print("Done!")
}


#' Crop Time Series
#'
#' Interactive function to crop a time series.
#' @param file File path to measurement.
#'
#' @param path.data A string character defining where to save the results. If `NULL`,
#' data is not stored in a file. Default: `NULL`.
#'
#' @details Select points at start and end of desired part of measurements.
#' Only the last two points will be
#'   taken into account to allow the user to correct erroneous clicks.
#'
#' If a measurement contains two distinct regions of bites with a lot of
#' unnecessary data and/or measurement artefacts in-between (such as user-made
#' peaks), I recommend to manually copy the RAW data files, give the copy a
#' new measurement number (as if it was actually a separate measurement), and
#' crop the distinct parts containing actual bites separately from the two
#' copies of the file. For more distinct regions, create more copies.
#'
#' I recommend to not crop the files too much in case baseline corrections are
#' needed later, because then the `baseline_corr()` function will not be able
#' to figure out where the actual baseline might be. Leaving several seconds
#' before and after the first and last bite of a series will prevent such
#' problems.
#'
#' @return Returns and, if `path.data is not NULL`, saves data in
#' csv-format in `path.data`. \cr
#' \cr
#' The tibble has the following format:
#'
#' | **`t`** |   | **`y`** |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.1` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#'
#' @examples
#' # get file path of forceR example file
#' filename <- forceR_example(type = "raw")
#'
#'# # crop file without storing result as file - out-commented to pass package tests
#'# file.cropped <- crop_measurement(file = filename,
#'#                    path.data = NULL)
#'
#'# file.cropped
#'
#' @export
crop_measurement <- function (file,
                              path.data = NULL){

  if(!is.character(file)) stop ("'file' must be a character string.")

  if(!file.exists(file)) stop(paste0("File ", file, " does not exist!"))

  # dyplr NULLs
  y <- NULL

  if(!is.null(path.data)){
    if(!dir.exists(path.data)) stop ("Folder to store plots does not exist: ", path.data, ".")
  }

  res.reduction <- 10
  file.name <- basename(file)

  data = read_csv(file, show_col_types = FALSE)

  colnames(data) <- c("t", "y")

  # Check what the decimal is. Returns "" when it is a full stop, and the
  #   decimal, e.g. "," if it is not.
  decimal <- gsub('[[:digit:]]+', '', data[1,1])

  if(decimal != ""){
    warning(paste0("Converting decimal separator \'", decimal, "\' to \'.\'..."))
    data$t <- as.numeric(gsub(decimal, ".", gsub("\\.", "", data$t))) * 1000
    data$y <- as.numeric(gsub(decimal, ".", gsub("\\.", "", data$y)))
  }

  sampling_rate_Hz <- 1/round(data$t[2] - data$t[1], digits = 4) * 1000

  plot(data[,1:2], type = 'l',
       main = paste0(file.name, " (", sampling_rate_Hz, " Hz)"), xlab = "time [m.sec]", ylab = "y")

  message("Select beginning and end of measurement and click \"Finish\". Only the last two points will be used.")
  cutoffs <- locator(type = "n")

  # only take last two cutoffs
  cutoffs$x <- cutoffs$x[(length(cutoffs$x)-1):(length(cutoffs$x))]
  cutoffs$y <- cutoffs$y[(length(cutoffs$y)-1):(length(cutoffs$y))]

  # convert cutoffs outside the graph area to time value(s) at beginning and/or end of graph area
  if(cutoffs$x[length(cutoffs$x)-1] < min(data$t)){cutoffs$x[1] <- min(data$t)}
  if(cutoffs$x[length(cutoffs$x)] > max(data$t)){cutoffs$x[2] <- max(data$t)}

  ceiling_dec <- function(x, level=1) round(x + 5*10^(-level-1), level)
  floor_dec <- function(x, level=1) round(x - 5*10^(-level-1), level)
  start_time_msec <- which(data$t == ceiling_dec(cutoffs$x[length(cutoffs$x)-1], level = -1))
  end_time_msec <-  which(data$t == floor_dec(cutoffs$x[length(cutoffs$x)], level = -1))

  data_cut <- data[start_time_msec:end_time_msec,]
  data_cut$t <- data_cut$t - data_cut$t[1]

  # define x ticks
  x_ticks <- seq(0, data_cut$t[nrow(data_cut)], 1000)

  # plot with amplifier_scaling - correction
  plot(data_cut$t[seq(1,nrow(data_cut),res.reduction)], data_cut$y[seq(1,nrow(data_cut),res.reduction)], type = 'l',
       main = paste0(file.name, " (", sampling_rate_Hz, " Hz)"), xlab = "t", ylab = "y", xaxt = "n")

  axis(side = 1,
       at = x_ticks,
       labels = x_ticks)

  data_cut <- data_cut %>%
    select(t, y)

  # if(write.files == TRUE){
  if(!is.null(path.data)){
    write_csv(data_cut, paste0(path.data, sub("\\.[[:alnum:]]+$", "", file.name), "_cropped.csv"), quote = "none")
  }
  return(data_cut)
  # print("Done!")
}

