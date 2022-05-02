#' Charge Amplifier Drift Correction
#'
#' Removes the systemic, asymptotical drift of charge amplifiers with
#' resistor-capacitor (RC) circuits.
#'
#' @param filename Path to file on which amplifier drift correction should be
#' performed.
#'
#' @param tau Numeric time constant of charge amplifier in the same time unit
#' as the measurement data.
#' Default: `9400`
#'
#' @param res.reduction A numeric value to reduce the number of time steps by
#' during plotting. Speeds up
#' the plotting process and reduces PDF size. Has no effect on the results,
#' only on the plots. Default: `10`.
#'
#' @param plot.to.screen A logical value indicating if results should be
#' plotted in the current R plot device. Default: `FALSE`.
#'
#' @param write.data A logical value indicating if drift-corrected file should
#' be saved. If yes, it will be saved in `output.folder`.
#' Default: `FALSE`.
#'
#' @param write.PDFs A logical value indicating whether results should be saved
#' as PDFs. Does not slow
#' down the process as much as printing to the R plot device and is considered
#' necessary to quality check the results. If yes, it will be saved in
#' `output.folder/PDFs`. Default: `FALSE`.
#'
#' @param write.logs A logical value indicating whether a log file with
#' information on the method and values used to correct the amplifier drift should be
#' saved. Is considered necessary for reproducibility. If yes, it will be saved
#' in `output.folder/logs`. Default: `FALSE`.
#'
#' @param output.folder Path to folder where data, PDF and log files should
#' be stored.
#'
#' @param show.progress A logical value indicating if progress should be
#' printed to the console. Slows down the process. Default: `FALSE`.
#'
#' @details
#' forceR generally expects file names to start with a leading number specifying the
#' measurement number (E.g. "0001_G_maculatus.csv"). The number ("0001") is used to
#' keep data files, log files, and PDF files of the same measurement associated
#' with each other.
#'
#' The input file should be in the following format:
#'
#' | `t` |   | `y` |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.2` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#'
#' @return
#' Returns a tibble containing the amplifier drift-corrected data in the
#' following format
#'
#' | `t` |   | `y` |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.2` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#'
#' @examples
#'# define file for amplifier drift correction
#'filename <- forceR_example(type = "raw")
#'
#'# Run amplifier drift correction without saving files or printing to screen:
#'file.ampdriftcorr <- amp_drift_corr(filename = filename,
#'                                      tau = 9400,
#'                                      res.reduction = 10,
#'                                      plot.to.screen = FALSE,
#'                                      write.data = FALSE,
#'                                      write.PDFs = FALSE,
#'                                      write.logs = FALSE,
#'                                      output.folder,
#'                                      show.progress = FALSE)
#'# file.ampdriftcorr
#'
#'# Run amplifier drift correction with saving files and printing to screen:
#'#    - commented out to pass package tests
#'# file.ampdriftcorr <- amp_drift_corr(filename = filename,
#'#                                       tau = 9400,
#'#                                       res.reduction = 10,
#'#                                       plot.to.screen = TRUE,
#'#                                       write.data = TRUE,
#'#                                       write.PDFs = TRUE,
#'#                                       write.logs = TRUE,
#'#                                       output.folder = "./ampdriftcorr",
#'#                                       show.progress = TRUE)
#'#
#'# file.ampdriftcorr
#'
#' @export
#'
amp_drift_corr <- function(filename,
                           tau = 9400,
                           res.reduction = 10,
                           plot.to.screen = FALSE,
                           write.data = FALSE,
                           write.PDFs = FALSE,
                           write.logs = FALSE,
                           output.folder = NULL,
                           show.progress = FALSE){

  if(!is.character(filename)) stop ("'filename' must be a character string.")
  if(!file.exists(filename)) stop (paste0("filename ", filename, " cannot be found."))

  if(!is.numeric(tau)) stop ("'tau' must be numeric.")

  if(!is.logical(plot.to.screen)) stop ("'plot.to.screen' must be logical.")

  if(!is.logical(write.PDFs)) stop ("'write.PDFs' must be logical.")
  if(!is.logical(write.data)) stop ("'write.data' must be logical.")
  if(!is.logical(write.logs)) stop ("'write.logs' must be logical.")

  if(!is.numeric(res.reduction)) stop ("'res.reduction' must be numeric.")

  if(!is.logical(show.progress)) stop ("'show.progress' must be logical.")

  # dplyr NULLS
  x <- NULL


  if(write.PDFs == TRUE | write.data == TRUE | write.logs == TRUE){
    if(is.null(output.folder)) stop ("'output.folder' must be defined when one of the following arguments are TRUE:
                                     write.data, write.PDFs, write.logs.")
    if(!is.character(output.folder)) stop ("'output.folder' must be a character string.")
    if(!dir.exists(output.folder)){
      dir.create(output.folder, showWarnings = FALSE)
    } else {
      # print(paste0(output.folder, " already exists."))
    }


    output.folder.logs <- file.path(output.folder, "logs_ampdriftcorr")
    if(!dir.exists(output.folder.logs)){
      dir.create(output.folder.logs, showWarnings = FALSE)
    } else {
      # print(paste0(output.folder.logs, " already exists."))
    }

    output.folder.pdfs <- file.path(output.folder, "pdfs_ampdriftcorr")
    if(!dir.exists(output.folder.pdfs)){
      dir.create(output.folder.pdfs, showWarnings = FALSE)
    } else {
      # print(paste0(output.folder.pdfs, " already exists."))
    }
  }

  curr.measurement <- sub("^([^_]+)_.*", "\\1", basename(filename))
  # curr.measurement <- sub('\\..*$', '', basename(filename))

  data <- read_delim(filename,
                     delim = ",",
                     show_col_types = FALSE)

  colnames(data) <- c("t", "y")
  time.start <- data$t[1]
  data$t <- data$t-time.start
  data$y.raw <- data$y
  # plot(data$t, data$y, type = "l",
  #      main = paste0(curr.measurement))


  # get time step size
  t.step <- data$t[2] - data$t[1]
  # if(t.step == 0.5){
  #   data$t <- seq(0, nrow(data)*t.step-t.step, t.step)
  # } else {
  #   data$t <- seq(0, nrow(data)*t.step-1, t.step)
  # }

  # # plot data with rownumber in x, not data$t
  # plot(data$y, type = "l", ylim=c(min(data$y), max(data$y)),
  #      main = curr.measurement)

  # logarithmic drift correction
  # correction constant
  # print(t.step)

  # reference y
  V0 <- 1

  # calculate change for t.step
  corr.const <- V0*exp(-(t.step/tau))

  # get first y value to make it V.0
  V.0 <- data$y[1]

  # check if there are na-values in y (so far only the case in bf_0051)
  na.row <- which(is.na(data$y))
  if(length(na.row) > 0){
    warning(paste0("found ", length(na.row), " NA values in time series!"))
    # replace na value with next y value
    data$y[na.row] <- data$y[na.row+1]
  }

  # subtract V.0 from y
  data$y.0cor <- data$y-V.0
  # plot(data$y.0cor, type="l")

  # create empty column for corrected y values
  data$y <- NA
  data$y[1] <- 0

  # predict y under no load
  # indexing vectors instead of data.frames is much faster
  y <- data$y
  y.0cor <- data$y.0cor

  if(show.progress == TRUE){
    for(i in 2:nrow(data)){ # nrow(data)
      y[i] <- y[i-1] + (y.0cor[i] - (y.0cor[i-1] * corr.const))
      print_progress(i, nrow(data))
    }
  } else {
    for(i in 2:nrow(data)){ # nrow(data)
      y[i] <- y[i-1] + (y.0cor[i] - (y.0cor[i-1] * corr.const))
    }
  }

  data$y <- y

  # delete columns with correction data
  data$y.0cor <- NULL


  #
  # # plot data
  # plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.raw[seq(1,nrow(data),res.reduction/t.step)], type = "l",
  #      ylim=c(min(data$y.raw, data$y, na.rm = TRUE), max(data$y.raw, data$y, na.rm = TRUE)),
  #      main = paste0(curr.measurement, "; t.step = ", t.step), lwd = 3, col = "grey70",
  #      xlab = "t", ylab = "y")
  # lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", lwd = 2, col = "green")
  # lines(c(min(data$t), max(data$t)), c(0,0), type="l", col = "red")

  # get linear correction line
  lin.cor.line.x <- c(0,max(data$t))
  lin.cor.line.y <- c(0,data$y[nrow(data)]) # nrow(data)
  # lines(lin.cor.line.x, lin.cor.line.y, type="l", col = "orange")

  baseline.sp <- spline(x=lin.cor.line.x, y=lin.cor.line.y, n = nrow(data)*2.1)
  baseline.sp <- bind_cols(x = baseline.sp$x, y = baseline.sp$y)
  # lines(baseline.sp$x, baseline.sp$y, type="l", col = "orange", lwd=1)

  # set round factor: -1 = 10; 0 = same
  if(t.step == 1 | t.step == 2){
    round.factor <- 0
  } else if(t.step == 10){
    round.factor <- -1
  } else if(t.step == 0.5){
    round.factor <- "defined later"
  }

  # filter points that lie within measured area
  if(t.step == 1 | t.step == 2 | t.step == 10){
    baseline.sp.filtered <- baseline.sp %>%
      mutate(x = round(x,round.factor)) %>%
      group_by(x) %>%
      summarize(mean.V = mean(y)) %>%
      filter(x>=0 & x<= max(data$t))
    if(t.step == 2){
      baseline.sp.filtered <- baseline.sp.filtered %>%
        filter(x %% t.step == 0)
    }
  } else if(t.step == 0.5){
    baseline.sp.filtered <- baseline.sp %>%
      mutate(x = ceiling(x*2) / 2) %>%
      group_by(x) %>%
      summarize(mean.V = mean(y)) %>%
      filter(x>=0 & x<= max(data$t))
  }

  # subtract new baseline from rawdata
  data$y <- round(data$y-baseline.sp.filtered$mean.V,6)
  # lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "darkgreen")

  if(write.PDFs == TRUE){
    pdf(file.path(output.folder.pdfs, paste0(curr.measurement, "_ampdriftcorr", ".pdf")),
        width = 20, height = 10)

    plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.raw[seq(1,nrow(data),res.reduction/t.step)], type = "l",
         ylim=c(min(data$y.raw, data$y, na.rm = TRUE), max(data$y.raw, data$y, na.rm = TRUE)),
         main = paste0(curr.measurement, "; t.step = ", t.step), lwd = 3, col = "grey70",
         xlab = "t", ylab = "y")
    lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", lwd = 2, col = "green")
    lines(c(min(data$t), max(data$t)), c(0,0), type="l", col = "red")
    lines(baseline.sp$x, baseline.sp$y, type="l", col = "orange", lwd=1)
    lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "darkgreen")

    invisible(dev.off())
  }

  # plot data
  if(plot.to.screen == TRUE){
    plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.raw[seq(1,nrow(data),res.reduction/t.step)], type = "l",
         ylim=c(min(data$y.raw, data$y, na.rm = TRUE), max(data$y.raw, data$y, na.rm = TRUE)),
         main = paste0(curr.measurement, "; t.step = ", t.step), lwd = 3, col = "grey70",
         xlab = "t", ylab = "y")

    lines(data$t[seq(1,nrow(data),res.reduction/t.step)],
          data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", lwd = 2, col = "green")

    lines(c(min(data$t), max(data$t)), c(0,0), type="l", col = "red")

    lines(baseline.sp$x, baseline.sp$y, type="l", col = "orange", lwd=1)

    lines(data$t[seq(1,nrow(data),res.reduction/t.step)],
          data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "darkgreen")
  }

  data <- data %>%
    select(t, y)

  if(write.data == TRUE){
    write_csv(data, file.path(output.folder, paste0(curr.measurement, "_ampdriftcorr.csv")))
  }

  # save little log file with cut window infos
  if(write.logs == TRUE){
    write_csv(data.frame(correction.factor = "exp(-(t/tau))",
                         tau = tau,
                         t = t.step,
                         script.version = "0.0.4"),
              file.path(output.folder.logs, paste0(curr.measurement, "_ampdriftcorr_log.csv")))
  }
  return(data)
}




#' Automatic or Manual Baseline Correction of Time Series
#'
#' If baseline (zero-line) of measurement is unstable (e.g. due to temperature fluctuations, wind, ...),
#' the baseline needs to be continually adjusted throughout the measurement. This script allows an
#' automatic adjustment of the baseline.
#' The automatic approach invokes a sliding window, during which the 'minimum' within each sliding window
#' is stored. A 'minimum' is defined by the `quantile.size`: if set to `0.05`, the value below which only 5% of
#' the measurement data within the sliding window lies, is treated as the current window's minimum. This
#' prevents the treatment of potential artifacts as minima. In a second iteration, another sliding window
#' calculates the average of these 'minima'. The resulting values are subtracted from
#' the original time series. This approach works well for time series with relatively short peaks.
#' If the automatic approach does not yield acceptable results, an interactive manual approach to correct
#' the baseline can be performed instead.
#'
#' @param filename A character string containing the full path to the measurement file that needs correction.
#' See Details for info on what the file should look like.
#'
#' @param corr.type Character string defining the desired mode of baseline correction. One of `"auto"`
#'   or `"manual"`. Default: `"auto"`
#'
#' @param window.size.mins A numeric value for the size of the search window to find minima in. Should be
#'   in the same time unit as the measurement. Longer peaks require higher values, shorter peaks require
#'   smaller values. Default: `1000`.
#'
#' @param window.size.means  A numeric value for the size of the window to average the minima in. Should be
#'   in the same time unit as the measurement. By default (`NULL`), the same value as specified for
#'   `window.size.mins` is used.
#'
#' @param quantile.size A numerical value between `0` and `1` to define the quantile which is treated as
#'   the 'minimum' of a sliding window. Default: `0.05`.
#'
#' @param y.scale A numeric value to reduce the y-axis range during plotting. This simplifies the manual
#'   placement of the points during the manual correction procedure.
#'
#' @param res.reduction A numeric value to reduce the number of time steps by during plotting. Speeds up
#'   the plotting process and reduces PDF size. Has no effect on the results, only on the plots. Default: `10`.
#'
#' @param Hz A numeric value to reduce sampling frequency for temporary analyses. This works as a smoothing
#'   filter during temporary analyses and does not reduce the actual sampling frequency of the data.
#'   Default: `100`.
#'
#' @param plot.to.screen A logical value indicating if results should be
#' plotted in the current R plot device. Default: `FALSE`.
#'
#' @param write.data A logical value indicating if drift-corrected file should
#' be saved. If yes, it will be saved in `output.folder`.
#' Default: `FALSE`.
#'
#' @param write.PDFs A logical value indicating whether results should be saved
#' as PDFs. Does not slow
#' down the process as much as printing to the R plot device and is considered
#' necessary to quality check the results. If yes, it will be saved in
#' `output.folder/PDFs`. Default: `FALSE`.
#'
#' @param write.logs A logical value indicating whether a log file with
#' information on the method and values used to correct the baseline drift should be
#' saved. Is considered necessary for reproducibility. If yes, it will be saved
#' in `output.folder/logs`. Default: `FALSE`.
#'
#' @param output.folder Path to folder where data, PDF and log files should
#' be stored. Default: `NULL.`
#'
#' @param show.progress A logical value indicating if progress should be
#' printed to the console. Default: `FALSE`.
#'
#' @details
#' `forceR` generally expects file names to start with a leading number specifying the
#' measurement number (E.g. "0001_G_maculatus.csv"). The number ("0001") is used to
#' keep data files, log files, and PDF files of the same measurement associated
#' with each other.
#'
#' The input files should to be in the following format:
#'
#' | `t` |   | `y` |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.2` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#'
#' In case there are more than two columns, only the first two columns will be used. If the first two columns
#' are not named 't' and 'y', they will be renamed.
#'
#' @return
#' Returns a tibble containing the amplifier baseline drift-corrected data in the
#' following format
#'
#' | `t` |   | `y` |
#' | :----: | :----: |:----: |
#' | `t.1` |   | `y.2` |
#' | `...` |   | `...` |
#' | `t.n` |   | `y.n` |
#'
#' @examples
#'#'########### AUTOMATIC MODE
#'# define file to apply the baseline drift correction to
#'filename <- forceR_example(type = "ampdriftcorr")
#'
#'# run automatic baseline drift corrections without saving files or
#'#   printing to screen:
#'file.baseline_corr <- baseline_corr(filename = filename,
#'                                      corr.type = "auto",
#'                                      window.size.mins = 1000,
#'                                      window.size.means = NULL,
#'                                      quantile.size = 0.05,
#'                                      y.scale = 0.5,
#'                                      res.reduction = 10,
#'                                      Hz = 100,
#'                                      plot.to.screen = FALSE,
#'                                      write.data = FALSE,
#'                                      write.PDFs = FALSE,
#'                                      write.logs = FALSE,
#'                                      show.progress = FALSE)
#'
#'file.baseline_corr
#'
#'#'########### MANUAL MODE
#'
#'# define file to apply the baseline drift correction to
#'filename <- forceR_example(type = "ampdriftcorr")
#'
#'# run interactive baseline drift corrections with saving files and
#'#   printing to screen:  - out-commented to pass package tests
#'
#'# file.baseline_corr <- baseline_corr(filename = filename,
#'#                                       corr.type = "manual",
#'#                                       window.size.mins = 1000,
#'#                                       window.size.means = NULL,
#'#                                       quantile.size = 0.05,
#'#                                       y.scale = 0.5,
#'#                                       res.reduction = 10,
#'#                                       Hz = 100,
#'#                                       plot.to.screen = TRUE,
#'#                                       output.folder = "./baselinecorr",
#'#                                       show.progress = TRUE)
#'
#'# file.baseline_corr
#' @importFrom roll roll_mean
#' @export
#'
baseline_corr <- function(filename,
                          corr.type = "auto",
                          window.size.mins = 1000,
                          window.size.means = NULL,
                          quantile.size = 0.05,
                          y.scale = 0.5,
                          res.reduction = 10,
                          Hz = 100,
                          plot.to.screen = FALSE,
                          write.data = FALSE,
                          write.PDFs = FALSE,
                          write.logs = FALSE,
                          output.folder = NULL,
                          show.progress = FALSE){

  if(!is.character(filename)) stop ("'filename' must contain character string only.")
  if(length(filename) > 1) stop ("'filename' must only contain one character string.")
  if(!file.exists(filename)) stop (paste0("filename ", filename, " cannot be found."))

  if(!is.character(corr.type)) stop ("'corr.type' must be a character string.")
  if(length(corr.type) != 1) stop ("'corr.type' must only contain one character string.")
  if(!(corr.type == "auto") & !(corr.type == "manual")) stop ("'corr.type' can only be one of the following strings: 'auto' or 'manual'.")

  if(!is.logical(plot.to.screen)) stop ("'plot.to.screen' must be logical.")

  if(!is.logical(write.PDFs)) stop ("'write.PDFs' must be logical.")

  if(!is.numeric(window.size.mins)) stop ("'window.size.mins' must be numeric.")
  if(length(window.size.mins) > 1) stop ("'window.size.mins' must be a single number.")

  if(!is.numeric(window.size.means) & !is.null(window.size.means)) stop ("'window.size.means' must be numeric or NULL.")
  if(length(window.size.means) != 1 & !is.null(window.size.means)) stop ("'window.size.means' must be a single number.")

  if(!is.numeric(quantile.size)) stop ("'quantile.size' must be numeric.")
  if(length(quantile.size) > 1) stop ("'quantile.size' must be a single number.")

  if(!is.numeric(y.scale)) stop ("'y.scale' must be numeric.")
  if(length(y.scale) > 1) stop ("'y.scale' must be a single number.")

  if(!is.numeric(res.reduction)) stop ("'res.reduction' must be numeric.")
  if(length(res.reduction) > 1) stop ("'res.reduction' must be a single number.")

  if(!is.numeric(Hz)) stop ("'Hz' must be numeric.")
  if(length(Hz) > 1) stop ("'Hz' must be a single number.")

  # dplyr NULLs
  x <- y <- y.zerocor <- t.10 <- V.10 <- NULL

  if(is.null(window.size.means)) window.size.means <- window.size.mins

  if(write.PDFs == TRUE | write.data == TRUE | write.logs == TRUE){
    if(is.null(output.folder)) stop ("'output.folder' must be defined when one of the following arguments are TRUE:
                                     write.data, write.PDFs, write.logs.")
    if(!is.character(output.folder)) stop ("'output.folder' must be a character string.")
    if(!is.character(output.folder)) stop ("'output.folder' must be a character string.")
    if(!dir.exists(output.folder)){
      dir.create(output.folder, showWarnings = FALSE)
    } else {
      # print(paste0(output.folder, " already exists."))
    }

    output.folder.logs <- file.path(output.folder, "logs_baselinecorr")
    if(!dir.exists(output.folder.logs)){
      dir.create(output.folder.logs, showWarnings = FALSE)
    } else {
      # print(paste0(output.folder.logs, " already exists."))
    }

    output.folder.pdfs <- file.path(output.folder, "pdfs_baselinecorr")
    if(!dir.exists(output.folder.pdfs)){
      dir.create(output.folder.pdfs, showWarnings = FALSE)
    } else {
      # print(paste0(output.folder.pdfs, " already exists."))
    }
  }

  data <- read_delim(filename,
                     delim = ",",
                     show_col_types = FALSE)

  curr.measurement <- sub("^([^_]+)_.*", "\\1", basename(filename))
  # curr.measurement <- sub('\\..*$', '', basename(filename))

  if(ncol(data) <= 1) stop("Data contains less than two columns. Process terminated")

  if(ncol(data) > 2){
    warning("Data contains more than two columns. Only first two columns will be used.")
    data <- data[,1:2]
  }

  if(sum(colnames(data) == c("t", "y")) != 2){
    warning("Column names of data were not 't' and 'y'. Renaming colums.")
    colnames(data) <- c("t", "y")
  }

  t.step <- data$t[2] - data$t[1]

  # # plot measurement to screen
  # plot(data$t[seq(1,nrow(data),res.reduction/t.step)],
  #      data$y[seq(1,nrow(data),res.reduction/t.step)],
  #      type = "l", lwd = 2, col = "darkgreen",
  #      main = paste0(curr.measurement))
  # lines(c(data$t[1], data$t[nrow(data)]), rep(0, 2), type="l", col = "red", lwd = 1)


  # manual mode -------------------------------------------------------------
  if(corr.type == "manual"){
    message("Manual drift correction selected.")
    message("Measurement name: ", curr.measurement, "\n")

    t.step <- data$t[2] - data$t[1]

    # plot measurement to screen. Due to interactive mode, this will always be done
    plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type = "l", lwd = 2, col = "grey90",
         main = paste0(curr.measurement), ylim = c(1.1*min(data$y), y.scale*max(data$y)))
    lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type = "l", col = "darkgreen",
          main = paste0(curr.measurement))
    lines(c(data$t[1], data$t[nrow(data)]), rep(0, 2), type="l", col = "red", lwd = 1)

    message("Please select baseline points and click \"Finish\".")
    baseline <- locator(type = "n")
    message(paste0(length(baseline$x), " line points taken."))

    lines(baseline, type="l", col = "orange")
    means.spline <- spline(x=baseline$x, y=baseline$y, n = nrow(data)*2.1)
    means.spline <- bind_cols(x = means.spline$x, y = means.spline$y)
    lines(means.spline$x, means.spline$y, type="l", col = "green", lwd=2)

    # set round factor: -1 = 10; 0 = same
    if(t.step == 1){
      round.factor <- 0
    } else if(t.step == 10){
      round.factor <- -1
    } else if(t.step == 0.5){
      round.factor <- "defined later"
    } else if(t.step == 2){
      round.factor <- "defined later"
    }

    # filter points that lie within measured area
    if(t.step == 1 | t.step == 10){
      means.spline.filtered <- means.spline %>%
        mutate(x = round(x,round.factor)) %>%
        group_by(x) %>%
        summarize(mean.V = mean(y)) %>%
        filter(x>=0 & x<= max(data$t))
    } else if(t.step == 0.5){
      means.spline.filtered <- means.spline %>%
        mutate(x = ceiling(x*2) / 2) %>%
        group_by(x) %>%
        summarize(mean.V = mean(y)) %>%
        filter(x>=0 & x<= max(data$t))
    } else if(t.step == 2){
      means.spline.filtered <- means.spline %>%
        mutate(x = 2 * ceiling(x / 2)) %>%
        group_by(x) %>%
        summarize(mean.V = mean(y)) %>%
        filter(x>=0 & x<= max(data$t))
    }

    # add values at start and end if line is shorter than data time frame
    line.t.start <- means.spline.filtered$x[1]
    line.t.end <- means.spline.filtered$x[nrow(means.spline.filtered)]
    points(c(line.t.start, line.t.end), c(0,0), col = "red", pch = 16)
    if(line.t.start > 0){
      line.start.tibble <- as_tibble(setNames(data.frame(matrix(nrow = line.t.start/t.step, ncol = length(c("x", "mean.V")))), c("x", "mean.V")))
      line.start.tibble$x <- seq(0, line.t.start-t.step, t.step)
      line.start.tibble$mean.V <- means.spline.filtered$mean.V[1]
      means.spline.filtered <- rbind(line.start.tibble,means.spline.filtered)
    }
    if(line.t.end < data$t[nrow(data)]){
      line.end.tibble <- as_tibble(setNames(data.frame(matrix(nrow = (data$t[nrow(data)]-line.t.end)/t.step, ncol = length(c("x", "mean.V")))), c("x", "mean.V")))
      line.end.tibble$x <- seq(line.t.end+t.step, data$t[nrow(data)], t.step)
      line.end.tibble$mean.V <- means.spline.filtered$mean.V[nrow(data)]
      means.spline.filtered <- rbind(means.spline.filtered, line.end.tibble)
    }
    lines(means.spline.filtered$x, means.spline.filtered$mean.V, type="l", col = "darkgreen", lwd=2)

    # subtract new baseline from cor.data
    data$y.zerocor <- data$y-means.spline.filtered$mean.V

    # round y values
    data$y <- round(data$y, 6)
    data$y.zerocor <- round(data$y.zerocor, 6)

    if(write.PDFs == TRUE){
      # start PDF device
      pdf(file.path(output.folder.pdfs, paste0(curr.measurement, "_baselinecorr", ".pdf")),
          width = 20, height = 10)

      # plot data
      plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type = "l",
           main = paste0(curr.measurement), lwd = 4, col = "grey80",
           xlim = c(0, max(data$t)),
           ylim = c(min(data$y, data$y, data$y.zerocor, na.rm = TRUE),
                    max(data$y, data$y, data$y.zerocor, na.rm = TRUE)))
      lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "grey60", lwd = 4)
      lines(means.spline.filtered$x, means.spline.filtered$mean.V, type="l", col = "green", lwd = 1)
      lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.zerocor[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "darkgreen", lwd = 1)
      lines(c(data$t[1], data$t[nrow(data)]), rep(0, 2), type="l", col = "red", lwd = 1)

      invisible(dev.off())
    }

    if(plot.to.screen == TRUE){
      # plot data with correction lines
      plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type = "l",
           main = paste0(curr.measurement), lwd = 4, col = "grey80",
           xlim = c(0, max(data$t)),
           ylim = c(min(data$y, data$y, data$y.zerocor, na.rm = TRUE),
                    max(data$y, data$y, data$y.zerocor, na.rm = TRUE)))
      lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "grey60", lwd = 4)
      lines(means.spline.filtered$x, means.spline.filtered$mean.V, type="l", col = "green", lwd = 1)
      lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.zerocor[seq(1,nrow(data),res.reduction/t.step)], type="l", col = "darkgreen", lwd = 1)
      lines(c(data$t[1], data$t[nrow(data)]), rep(0, 2), type="l", col = "red", lwd = 1)
    }

    data <- data %>%
      select(t, y.zerocor) %>%
      rename(y = y.zerocor)

    if(write.data == TRUE){
      write_csv(data, file.path(output.folder, paste0(curr.measurement, "_baselinecorr.csv")))
    }

    if(write.logs == TRUE){
      # save little log file with manual base spline line
      # write_csv(data.frame(baseline), paste0(output.folder, "/logs/", "prepared_bf_", curr.measurement, "_man_log.csv"))
      write_csv(data.frame(baseline), file.path(output.folder.logs, paste0(curr.measurement, "_baselinecorr_log.csv")))
    }
    # print("Done!")
  }

  # automatic mode ----------------------------------------------------------
  if(corr.type == "auto") {
    # graphics.off()
    # message("You selected automatic drift correction:")
    # message(paste0("sliding minima window size: ", window.size.mins))
    # message(paste0("sliding means of minima window size: ", window.size.means))
    # message(paste0("calculating with ", Hz, " Hz."))

    t.step <- data$t[2] - data$t[1]

    max.t.msec <- max(data$t, na.rm=T)

    # add lines in front and at end of data time window for better moving averages
    lines.to.add <- window.size.mins/t.step*15/t.step

    line.start.tibble <- as_tibble(setNames(data.frame(matrix(nrow = lines.to.add, ncol = length(colnames(data)))), colnames(data)))

    line.start.tibble$y <- data$y[1]
    line.start.tibble$y <- data$y[1]

    line.end.tibble <- as_tibble(setNames(data.frame(matrix(nrow = lines.to.add, ncol = length(colnames(data)))), colnames(data)))
    line.end.tibble$y <- data$y[nrow(data)]
    line.end.tibble$y <- data$y[nrow(data)]

    data.concat <- rbind(line.start.tibble, data, line.end.tibble)

    data.concat$t <- seq(0, nrow(data.concat)*t.step-t.step, t.step)

    # create data frame with data reduced to x Hz to even out spikes and unsteady sensor signal
    t.step.10 = 1000/Hz

    data.concat.10 <- data.concat %>%
      group_by(t.10 = t.step.10 * round(t/t.step.10)) %>%
      summarize(V.10 = mean(y)) %>%
      rename(t = t.10, y = V.10)

    # message(paste0("Finding ", quantile.size*100, "th percentile sliding minima..."))

    times.msec <- c()
    minima <- c()
    for(k in (window.size.mins/2/t.step.10):(nrow(data.concat.10)-window.size.mins/t.step.10/2)){
      times.msec[length(times.msec)+1] <- k*t.step.10-window.size.mins*t.step.10/2/t.step.10
      # get window
      curr.window.data <- data.concat.10$y[(k-window.size.mins/2/t.step.10):
                                             (k+window.size.mins/2/t.step.10)]
      # calculate slope within window
      curr.slope <- curr.window.data[length(curr.window.data)] - curr.window.data[1]
      # if positive or constant slope:
      if(curr.slope >= 0){
        # get y value below only (1-quantile.size)*100 % of the data lie.
        # This prevents getting singular value or very short minima
        minima[length(minima)+1] <- quantile(curr.window.data, quantile.size)
        # if negative slope:
      } else {
        # check if k becomes smaller than 1 if halve window.size is subtracted and set k to 1 if that's the case
        if(k-window.size.mins/2 < 1){k=1}
        # get window new window with different position relative to k
        curr.window.data <- data.concat.10$y[(k-window.size.mins/t.step.10):
                                               (k-window.size.mins/t.step.10/2)]
        # get y value below only (1-quantile.size)*100 % of the data lie.
        # This prevents getting singular value or very short minima
        minima[length(minima)+1] <- quantile(curr.window.data, quantile.size)
      }
      if(show.progress == TRUE){
        print_progress(k, nrow(data.concat.10)-window.size.mins/t.step.10/2)
      }
    }


    # replace NA values with previous minumum
    for(m in 1:length(minima)){
      if(is.na(minima[m])){
        minima[m] <- minima[m-1]
      }
    }

    # replace minima at beginning outside the data time window with first minimum found
    minima[1:(window.size.mins/2/t.step.10+1)] <- 0

    # plot(data.concat.10$t, data.concat.10$y, type = "l", col = "darkgreen",
    #       main = paste0(curr.measurement))
    # lines(c(data.concat.10$t[1], data.concat.10$t[nrow(data.concat.10)]), rep(0, 2), type="l", col = "red", lwd = 1)
    # lines(times.msec, minima, col = "cyan")

    # message("Finding sliding means...")

    means <- roll_mean(
      x = minima,
      width = window.size.means/t.step.10,    complete_obs = FALSE,
      na_restore = FALSE,
      online = FALSE
    )
    means <- means[(window.size.mins/2/t.step):length(means)]

    times.msec.means <- roll_mean(
      x = times.msec,
      width = window.size.means/t.step.10,
      complete_obs = FALSE,
      na_restore = FALSE,
      online = FALSE
    )
    times.msec.means <- times.msec.means[(window.size.mins/2/t.step):length(times.msec.means)]

    length.diff <- t.step.10 * (length(minima) - length(means))
    means <- c(rep(means[1], ceiling(length.diff/2/t.step.10)),
               means,
               rep(means[length(means)], floor(length.diff/2/t.step.10)))

    times.msec.means <- c(seq(0, floor(length.diff/2-t.step.10), t.step.10),
                          times.msec.means,
                          seq(max(times.msec.means, na.rm = TRUE)+t.step, floor(max(times.msec.means+t.step, na.rm = TRUE)+floor(length.diff/2)), t.step.10))


    # plot(data.concat.10$t, data.concat.10$y, type = "l", col = "darkgreen",
    #       main = paste0(curr.measurement))
    # lines(times.msec, minima, type = "l", col = "blue")
    # lines(times.msec.means, means, type = "l", col = "green")

    t.start <- lines.to.add*t.step

    # get means into correct time frame
    means.tibble <- as_tibble(cbind(times.msec.means, means)) %>%
      rename(t = times.msec.means) %>%
      filter(t >= t.start & t <= max.t.msec+t.start) %>%
      mutate(t = t-t.start)

    means.spline <- spline(x=means.tibble$t, y=means.tibble$means, n = nrow(data))
    means.spline <- bind_cols(t = means.spline$x, means = means.spline$y)

    data$y.zerocor <- data$y-means.spline$means

    # plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)],
    #      type = "l", col = "grey80", lwd = 3,
    #      main = paste0(curr.measurement))
    # lines(means.tibble$t, means.tibble$means, col = "blue")
    # lines(means.spline$t, means.spline$means, type="l", col = "red", lwd=2)
    # lines(data$t, data$y.zerocor, col = "green")

    # round y values
    data$y <- round(data$y, 6)
    data$y <- round(data$y, 6)
    data$y.zerocor <- round(data$y.zerocor, 6)

    if(write.PDFs == TRUE){
      # start PDF device
      # print("Writing to PDF...")
      pdf(file.path(output.folder.pdfs, paste0(curr.measurement, "_baselinecorr", ".pdf")),
          width = 20, height = 10)

      plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)],
           main = paste0(curr.measurement), lwd = 4, type="l", col = "grey60",
           xlim = c(0, max(data$t)),
           ylim = c(min(data$y, data$y, data$y.zerocor),
                    max(data$y, data$y, data$y.zerocor))) # y_raw
      lines(x=c(data$t[1], data$t[nrow(data)]), y=c(0,0), lty = 3, col = "red") # zero liner
      lines(times.msec-nrow(line.start.tibble)*t.step, minima, type="l", col = "blue", lwd = 1) # minima
      lines(means.spline$t , means.spline$means, type="l", col = "green", lwd = 2) # means
      lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.zerocor[seq(1,nrow(data),res.reduction/t.step)], type = "l", lwd = 1,
            main = paste0(curr.measurement), col = "darkgreen") # y_cor

      invisible(dev.off())
    }

    if(plot.to.screen == TRUE){
      # print("Printing to screen...")
      # plot data without correction lines
      plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.zerocor[seq(1,nrow(data),res.reduction/t.step)], type = "l",
           main = paste0(curr.measurement), lwd = 1, col = "black",
           xlim = c(0, max(data$t)),
           ylim = c(min(data$y, data$y, data$y.zerocor),
                    max(data$y, data$y, data$y.zerocor))) # y_raw
      lines(x=c(data$t[1], data$t[nrow(data)]), y=c(0,0), lty = 3, col = "red") # zero line


      # plot data with correction lines
      plot(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y[seq(1,nrow(data),res.reduction/t.step)],
           main = paste0(curr.measurement), lwd = 4, type="l", col = "grey60",
           xlim = c(0, max(data$t)),
           ylim = c(min(data$y, data$y, data$y.zerocor),
                    max(data$y, data$y, data$y.zerocor))) # y_raw
      lines(x=c(data$t[1], data$t[nrow(data)]), y=c(0,0), lty = 3, col = "red") # zero liner
      lines(times.msec-nrow(line.start.tibble)*t.step, minima, type="l", col = "blue", lwd = 1) # minima
      lines(means.spline$t , means.spline$means, type="l", col = "green", lwd = 2) # means
      lines(data$t[seq(1,nrow(data),res.reduction/t.step)], data$y.zerocor[seq(1,nrow(data),res.reduction/t.step)], type = "l", lwd = 1,
            main = paste0(curr.measurement), col = "darkgreen") # y_cor
    }


    data <- data %>%
      select(t, y.zerocor) %>%
      rename(y = y.zerocor)

    if(write.data == TRUE){
      write_csv(data, file.path(output.folder, paste0(curr.measurement, "_baselinecorr.csv")))
    }

    if(write.logs == TRUE){
      # save little log file with window size infos
      write_csv(data.frame(window.size.minima.msec = window.size.mins,
                           window.size.means.msec = window.size.means,
                           script.version = "0.0.4"),
                file.path(output.folder.logs, paste0(curr.measurement, "_baselinecorr_log.csv")))
    }
    # print("Done!")
  }

  return(data)
}
