#' Plot raw measurement
#'
#' Plots a time series.
#' @details
#' #' The input files need to be in the following format (even though column names do not matter):
#'
#' | **`t`** |   | **`y`** |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.1` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#'
#' @param file File path to measurement.
#' @param columns A vector of column numbers. The first entry will be used as the x-axis values, the second entry as y-axis values.
#' All other columns will be ignored. Default: `c(1,2)`.
#' @return Creates a plot in the current plot device.
#' @examples
#' filename = forceR_example(type = "raw")
#' plot_measurement(filename)
#'
#' @export
plot_measurement <- function (file,
                              columns = c(1:2)){

  if(!is.character(file)) stop ("'file' must be a character string.")
  if(!is.numeric(columns)) stop ("'columns' must be a numeric.")
  if(length(columns) != 2) stop ("'columns' must be of length 2.")

  # tested for v1.19
  if(file.exists(file)){
    file.name <- basename(file)
    data = read_csv(file, show_col_types = FALSE)

    # check if data as at least as many columns as max. number in columns variable
    if(ncol(data) < max(columns)) stop('Data has fewer columns than defined by user.')


    sampling_rate_Hz <- 1/round(data[2,1] - data[1,1], digits = 4)*1000

    plot(data[,columns], type = 'l',
         main = paste0(file.name, " (", sampling_rate_Hz, " Hz)"), xlab = "time [m.sec]", ylab = "y")

  } else {
    stop("File does not exist!")
  }
}



#' Plot Peaks
#'
#' Plots the peaks identified by the function `find_peaks()`.
#'
#' @param df.peaks df.peaks The resulting tibble of the function `find_peaks()`. See `?find_peaks` for more details.
#'
#' @param df.data A data frame or tibble in the below format. The columns `t` (time), `force` and `measurement`
#'   (measurement ID) must be present. This will usually be the same table that was used before in `find_peaks()`.
#'
#' @param additional.msecs A numeric value indicating how many m.secs before and after the actual peak curve should be plotted. Default: `2000`
#'
#' @param plot.to.screen A logical value indicating if results should be
#' plotted in the current R plot device. Default: `TRUE`.
#'
#' @param path.plots A string character defining where to save the plots. If `NULL`,
#' plots will not be saved to PDF files. Default: `NULL`
#'
#' @param show.progress A logical value indicating if progress should be
#' printed to the console. Default: `FALSE`.
#'
#' @details
#' `df.peaks` at least needs to contain the following columns:
#'
#' **`measurements`** |  **`starts`** |  **`ends`** |
#' | :----: | :----: |:----: |:----: |
#' | `measurements.1` | `starts.1` | `ends.1` |
#' | `...` |  `...` |  `...` |
#' | `measurements.n` | `starts.m` | `ends.m` |
#'
#' Check `forceR::peaks.df` to see an example tibble.
#'
#' `df.data` at least needs to contain the following columns:
#'
#' | **`t`** | **`force`** |  **`measurement`** |
#' | :----: | :----: |:----: |
#' | `t.1` |  `force.1` | `measurement.1` |
#' | `...` |  `...` |  `...` |
#' | `t.n` |  `force.n` | `measurement.m` |
#'
#' Check `forceR::df.all.200.tax` to see an example tibble.
#'
#'
#' @return
#'
#' Plots one graph per peak curve and, if `plot.to.pdf == TURE`, saves all peak curves as one PDF at `path.plots`.
#'
#' @examples
#' # Using the forceR::peaks.df and forceR::df.all.200.tax datasets:
#'
#' # plot peaks
#' plot_peaks(df.peaks = forceR::peaks.df,
#'            df.data = forceR::df.all.200.tax,
#'            additional.msecs = 20) # instead of the default (2000) because of
#'                                   # the highly downsampled example dataset.
#'
#' @export
plot_peaks <- function(df.peaks,
                       df.data,
                       additional.msecs = 2000,
                       plot.to.screen = TRUE,
                       path.plots = NULL,
                       show.progress = FALSE){

  # # testing:
  # plot_peaks (df.peaks = forceR::peaks.df,
  #             df.data = forceR::df.all.200.tax,
  #             additional.msecs = 20,
  #             plot.to.screen = TRUE,
  #             path.plots = "./test_folder",
  #             show.progress = TRUE)

  if(sum(colnames(df.peaks) %in% c("starts", "ends", "measurements")) != 3){
    stop ("column names of 'df.peaks' must contain 'starts', 'ends', 'measurements'.")
  }
  if(sum(colnames(df.data) %in% c("t", "force", "measurement")) != 3){
    stop ("column names of 'df.data' must contain 't', 'force', 'measurement'.")
  }
  if(!is.numeric(additional.msecs)) stop ("'additional.msecs' must be a numeric.")

  if(!is.null(path.plots)){
    if(!dir.exists(path.plots)) stop ("Folder to store plots does not exist: ", path.plots, ".")
  }

  # if(!is.logical(plot.to.pdf)) stop ("'plot.to.pdf' must be logical.")

  measurement <- NULL

  # if(plot.to.pdf == TRUE){
  if(!is.null(path.plots)){
    # print(paste0("plotting to ", path.plots, today(),"_all_peak_curves.pdf..."))
    # on.exit(invisible(dev.off()), add = TRUE)
    pdf(file.path(path.plots, paste0("all_peak_curves_", today(), ".pdf")),
        onefile = TRUE, paper = "a4", height = 14)
  }

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar), add = TRUE)
  par(mfrow=c(3,2))
  for(b in 1:nrow(df.peaks)){ # nrow(df.peaks)

    curr.peak.starts <- str_split(df.peaks$starts[b], pattern = "; ")[[1]]
    curr.peak.ends <- str_split(df.peaks$ends[b], pattern = "; ")[[1]]
    measurements <- str_split(df.peaks$measurements[b], pattern = "; ")[[1]]

    # plot each peak
    for(c in 1:length(curr.peak.starts)){
      curr.measurement <- measurements[c]
      curr.peak.start <- as.numeric(curr.peak.starts[c])
      curr.peak.end <- as.numeric(curr.peak.ends[c])

      # add msces before and after
      curr.peak.window.with.frame <- df.data %>%
        filter(measurement == curr.measurement) %>%
        filter(t >= (curr.peak.start-additional.msecs) & t <= (curr.peak.end+additional.msecs)) %>%
        select(t, force)

      # plot(curr.peak.window.with.frame, type="l")

      curr.peak.window <- curr.peak.window.with.frame %>%
        filter(t >= (curr.peak.start) & t <= (curr.peak.end))

      # plot(curr.peak.window, type="l")

      if(nrow(curr.peak.window) >= 1){
        plot(curr.peak.window.with.frame, type="l", col="grey80", lwd=.5, ylim = c(0,max(curr.peak.window$force)))
        lines(curr.peak.window, type="l", main = paste0(curr.measurement), col="black", lwd=1)
        title(main = paste0(curr.measurement, ", peak: ", c), cex.main = 0.95)
      }
    }
    if(show.progress == TRUE){
      print_progress(b, nrow(df.peaks))
    }
  }
  # if(plot.to.pdf == TRUE){
  if(!is.null(path.plots)){
    invisible(dev.off())
  }
  # par(mfrow=c(1,1))
  # print("Done!")
}
