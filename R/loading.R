#' Load single measurement
#'
#' Loads a single measurement.
#' @details
#' #' The input files need to be in the following format (even though column names do not matter):
#'
#' | **`t`** |   | **`y`** |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.1` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#'
#' All columns except the first two are removed.
#'
#' @param filename Character string containing the path to measurement file.
#' @param columns A vector of column numbers. The first entry will be used as the x-axis values, the second entry as y-axis values.
#' All other columns will be ignored. Default: `c(1,2)`.
#' @return A tibble with two columns named "t" and "y".
#' @examples
#'# Store filename
#'filename <- forceR_example(type="raw")
#'
#'df.1 <- load_single(filename,
#'                     columns = c(1:2))
#'
#' @export
load_single <- function (filename,
                         columns = c(1:2)){
  if(!is.character(filename)) stop ("'filename' must be a character string.")
  if(!is.numeric(columns)) stop ("'columns' must be a numeric.")
  if(length(columns) != 2) stop ("'columns' must be of length 2.")

  # read data
  data_read <- read_csv(filename, progress = FALSE, col_types = cols())

  # check if data as at least as many columns as max. number in columns variable
  if(ncol(data_read) < max(columns)) stop('Data has fewer columns than defined by user.')

  # print(paste0("Removing all columns but ", colnames(data_read)[columns[1]], " and ", colnames(data_read)[columns[2]],
  #              ", renaming them to t and y and adding filename column based on the file name."))

  data_read <- data_read[,c(columns[1],columns[2])]

  # only keep first two columns (Time and final corrected Voltage) and measurement
  colnames(data_read) <- c("t", "y")
  data_read$filename <- sub("\\.[[:alnum:]]+$", "", basename(filename))

  return(data_read)
}




#' Load Multiple Measurements
#'
#' Loads multiple measurements.
#' @details
#' The input files need to be in the following format (even though column names do not matter):
#'
#' | **`t`** | **`y`** |
#' | :----: |:----: |
#' | `t.1` | `y.1` |
#' | `...` | `...` |
#' | `t.n` | `y.n` |
#'
#' All columns except the first two are removed.
#'
#' @param folder Character string containing the path to the measurements.
#' @param columns A vector of column numbers. The first entry will be used as the x-axis values, the second entry as y-axis values.
#' All other columns will be ignored. Default: `c(1,2)`.
#' @param show.progress A logical value indicating if progress should be
#' printed to the console. Default: `FALSE`.
#' @return Returns a tibble in the format
#'
#' | **`t`** | **`y`** | **`filename`**|
#' | :----: | :----: | :----: |
#' | `t.1` |  `y.1` |  `...`  |
#' | `...` | `...` | `...`  |
#' | `t.n` |  `y.n` |  `...`  |
#' @examples
#'# store name of folder that contains files
#'input.folder <- forceR_example(type = "folder")
#'
#'# load a mutiple files
#' df.all <- load_mult(folder = input.folder,
#'                     columns = c(1:2),
#'                     show.progress = FALSE)
#' @export
load_mult <- function (folder,
                       columns = c(1:2),
                       show.progress = FALSE){

  if(!is.character(folder)) stop ("'folder' must be a character string.")
  if(!is.numeric(columns)) stop ("'columns' must be a numeric.")
  if(length(columns) != 2) stop ("'columns' must be of length 2.")

  # dplyr NULLS
  pb <- NULL

  file.list <- dir(folder,
                   pattern = "\\.csv",
                   recursive = FALSE,
                   full.names = TRUE)
  if(length(file.list) == 0) stop ("No files found in ", folder, ".")
  # print(paste0("Found ", length(file.list), " files."))

  # do a test read
  # print("Checking if first file passes requirements...")
  data_read <- read_csv(file.list[1],
                        progress = FALSE, col_types = cols())
  # check if more than 2 columns were defined for plotting
  if(length(columns)>2) stop('More than two columns were defined by user.')
  # check if data as at least as many columns as max. number in columns variable
  if(ncol(data_read) < max(columns)) stop(paste0('Data in ', file.list[1], ' has fewer columns than defined by user.'))
  # print("Requirements met.")

  # reloading file list withouth full names
  file.list <- dir(folder, pattern = "\\.csv", recursive = FALSE)
  # print(paste0("Removing all columns but ", colnames(data_read)[columns[1]], " and ", colnames(data_read)[columns[2]],
  #                                        ", renaming them to t and y and adding filename column based on the file name."))

  if(show.progress == TRUE){
    suppressWarnings(pb <- progress_estimated(length(file.list)))

    load_single_w_progress <- function (file){
      # file <- "C:/Users/pruehr/Documents/forceR/vignettes/example_data/corrected/0980_cropped_ampdriftcorr.csv"
      suppressWarnings(pb$tick()$print())
      data_read <- read_csv(file, progress = FALSE, col_types = cols())
      # only keep first two columns
      data_read <- data_read[,c(columns[1],columns[2])]
      colnames(data_read) <- c("t", "y")
      data_read$filename <- sub("\\.[[:alnum:]]+$", "", basename(file))
      return(data_read)
    }
    suppressWarnings(measurements <- file.path(folder, file.list) %>%
                       map_df(~load_single_w_progress(.)))
  } else {
    load_single_wo_progress <- function (file){
      # file <- "C:/Users/pruehr/Documents/forceR/vignettes/example_data/corrected/0980_cropped_ampdriftcorr.csv"
      # suppressWarnings(pb$tick()$print())
      data_read <- read_csv(file, progress = FALSE, col_types = cols())
      # only keep first two columns
      data_read <- data_read[,c(columns[1],columns[2])]
      colnames(data_read) <- c("t", "y")
      data_read$filename <- sub("\\.[[:alnum:]]+$", "", basename(file))
      return(data_read)
    }
    suppressWarnings(measurements <- file.path(folder, file.list) %>%
                       map_df(~load_single_wo_progress(.)))
  }

  # measurements$Time.sec <- measurements$Time.msec/1000
  return(measurements)
}
