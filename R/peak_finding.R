#' Find Peaks
#'
#' Identifies peaks in a first iteration and optimizes the starts and ends of the strongest peaks per species in a second iteration.
#'
#'
#' @param df A data frame or tibble in the below format. The columns `t` (= time), `force` and `measurement`
#'   (= measurement ID) must be present.
#'
#' @param no.of.peaks A numeric value defining how many peaks per `species` (not per `measurement`") should be identified. The function will always return
#'  the strongest peaks. Default: `5`
#'
#' @param initial.threshold A numeric value defining the threshold (in % of the maximum force of the measurement)
#'   that is used during the first iteration. Default: `0.05`
#'
#' @param slope.length.start A numeric value defining the window size (in time steps) of
#'   slope calculation for peak starts during the second iteration. Default: `5`
#'
#' @param slope.length.end A numeric value defining the window size (in time steps) of
#'   slope calculation for peak ends during the second iteration. Default: `5`
#'
#' @param slope.thresh.start A numeric value defining the threshold at which to stop the sliding window and save the
#'   current time point as the actual start of the current peak. Default: `0.04`
#'
#' @param slope.thresh.end A numeric value defining the threshold at which to stop the sliding window and save the
#'   current time point as the actual end of the current peak. Default: `0.04`
#'
#' @param path.data A string character defining where to save the results. If `NULL` (default),
#' data is not stored in a file. Default: `NULL`.
#'
#' @param path.plots A string character defining where to save the plots. Default: `NULL`.
#'
#' @param show.progress A logical value indicating if progress should be
#' printed to the console. Default: `FALSE`.
#'
#' @details
#' The input data frame `df` needs to contain the following columns:
#'
#' | **`t`** | **`force`** |  **`measurement`** |
#' | :----: | :----: |:----: |
#' | `t.1` |  `force.1` | `measurement.1` |
#' | `...` |  `...` |  `...` |
#' | `t.n` |  `force.n` | `measurement.m` |
#'
#' @return Creates a tibble in the following format and saves it as a CSV-file:
#' The column **`species`** contains one species per row\cr (`species.1 ... species.n`).
#'
#' The column **`measurements`** contains as many measurements as `no.of.peaks`, separated by '`;`':\cr (`measurement.1; ...; measurements.no.of.peaks`).
#'
#' The column **`starts`** contains as many peak starts as `no.of.peaks`, separated by '`;`':\cr (`start.1; ...; start.no.of.peaks`).
#'
#' The column **`ends`** contains as many peak ends as `no.of.peaks`, separated by '`;`':\cr (`end.1; ...; end.no.of.peaks`).
#'
#' @examples
#' require(dplyr)
#' # Using the forceR::df.all.200.tax dataset:
#'
#' # reduce dataset (only rows 40 to 95 of species A (containing data of
#'   # measurement 1 and 2 (m_01 and m_02)))
#' df.all.200.tax_filtered <- forceR::df.all.200.tax[40:95, ] %>%
#'   filter(species == "species_A")
#'
#' # find the 4 strongest peaks
#' peaks.df <- find_strongest_peaks(df = df.all.200.tax_filtered,
#'                                  no.of.peaks = 4)
#'
#' # plot results (three peaks in measurement 1, 1 peak in measurement 2):
#' # plot_peaks(df.peaks = peaks.df,
#' #            df.data = df.all.200.tax_filtered,
#' #            additional.msecs = 20)
#'
#' @export
find_strongest_peaks <- function(df,
                                 no.of.peaks = 5,
                                 initial.threshold = 0.05,
                                 slope.length.start = 5,
                                 slope.length.end = 5,
                                 slope.thresh.start = 0.02,
                                 slope.thresh.end = 0.02,
                                 path.data = NULL,
                                 path.plots = NULL,
                                 show.progress = FALSE){

  # # testing
  # find_strongest_peaks (df = df.all.200.tax_filtered,
  #                       no.of.peaks = 5,
  #                       initial.threshold = 0.05,
  #                       slope.length.start = 5,
  #                       slope.length.end = 5,
  #                       slope.thresh.start = 0.02,
  #                       slope.thresh.end = 0.02,
  #                       path.data = "./test_folder",
  #                       path.plots = "./test_folder",
  #                       show.progress = TRUE)

  if(sum(colnames(df) %in% c("t", "force", "measurement")) != 3){
    stop ("column names of 'df.peaks' must contain 't', 'force', 'measurement'")
  }
  if(!is.null(path.plots) & !is.character(path.plots)) stop ("'path.plots' must be a character string.")

  if(!is.null(path.data)){
    if(!dir.exists(path.data)) stop ("Folder to store data does not exist: ", path.data, ".")
    if(!is.character(path.plots)) stop ("'path.plots' must be a character string")
  }

  specimen <- species <- slope <- measurement <- NULL # here! classifier is needed below for species info adding - should be parsed in function?

  # print("Initial rough threshold search for peaks...")
  measurements.all <- sort(unique(df$measurement))
  BF.starts.ends.tibble <- as_tibble(setNames(data.frame(matrix(nrow = length(measurements.all), ncol = length(c("measurement", "specimen", "starts", "ends")))), c("measurement", "specimen", "starts", "ends")))

  if(!is.null(path.plots)){
    # print(paste0("Saving plots at ", path.plots, "/", today(),"_rescaled_peaks_100.pdf..."))
    # on.exit(invisible(dev.off()), add = TRUE)
    pdf(file.path(path.plots, paste0("initial_starts_and_ends_", today(), ".pdf")),
        onefile = TRUE, paper = "a4", height = 14)
  }
  for(i in 1:length(measurements.all)){ # 1:length(measurements.all)   (plot.start.i+120)
    # get current measurement
    curr.measurement <- measurements.all[i]

    curr.plot.window <- df %>%
      # select("t","V","measurement") %>%
      filter(measurement == curr.measurement) %>%
      select(t, force, measurement, specimen) %>%
      ungroup()

    # plot(curr.plot.window$t, curr.plot.window$force, type="l")

    curr.specimen <- curr.plot.window %>%
      slice(1) %>%
      pull(specimen)

    threshold <- initial.threshold * max(curr.plot.window$force)

    # lines(c(0,max(curr.plot.window$t)), c(threshold,threshold), col = "green", type="l")

    curr.plot.window <- curr.plot.window %>%
      mutate(above.thrsh = case_when(force>=threshold ~ 1,
                                     force<=threshold ~ 0))

    switching.vector <- curr.plot.window$above.thrsh[2:nrow(curr.plot.window)] - curr.plot.window$above.thrsh[1:(nrow(curr.plot.window)-1)]
    starts <- which(switching.vector==1)
    ends <- which(switching.vector==-1)+1 # take next one to not underestimate downgoing slope (this mirrors behaviour at start of bite)

    starts.msecs <- curr.plot.window$t[starts]#-1000/Hz
    ends.msecs <- curr.plot.window$t[ends]#+1000/Hz

    BF.starts.ends.tibble$measurement[i] <- curr.measurement
    BF.starts.ends.tibble$specimen[i] <- curr.specimen
    BF.starts.ends.tibble$starts[i] <- paste(starts.msecs, collapse = "; ")
    BF.starts.ends.tibble$ends[i] <- paste(ends.msecs, collapse = "; ")

    number.of.starts <- length(starts.msecs)
    number.of.ends <- length(ends.msecs)

    # if(plot.to.screen == TRUE){
    plot(curr.plot.window$t, curr.plot.window$force, type="l")
    lines(c(0,max(curr.plot.window$t)), c(threshold,threshold), col = "green", type="l")
    points(starts.msecs, rep(threshold, number.of.starts), pch = 16, cex = 0.5, col = "blue")
    points(ends.msecs, rep(threshold, number.of.ends), pch = 16, cex = 0.5, col = "orange")

    curr.plot.title <- paste0("sp.: ", curr.specimen, "; meas.: ", curr.measurement, ". Found ", length(starts), " starts & ", length(starts), " ends.")
    title(curr.plot.title, line = -2, adj = .98)
    # }

    if(show.progress == TRUE){
      print_progress(i, length(measurements.all))
    }

    # if(plot.to.pdf == TRUE){


    # print(paste0("specimen: ", curr.specimen, "; measurement: ", curr.measurement, ". Found ", length(starts), " peak starts and ", length(starts), " peak ends."))
  }
  invisible(dev.off())

  # find max BF per peak
  # print(paste0("Finding max. peak force per peak... (this may take a while)"))
  # this needs multithreading
  BF.starts.ends.tibble$max.bf.peaks <- as.numeric(NA)

  for(b in 1:nrow(BF.starts.ends.tibble)){
    # print(b)
    curr.measurement <- BF.starts.ends.tibble$measurement[b]
    curr.specimen <- BF.starts.ends.tibble$specimen[b]

    curr.peak.starts <- str_split(BF.starts.ends.tibble$starts[b], pattern = "; ")[[1]]
    curr.peak.ends <- str_split(BF.starts.ends.tibble$ends[b], pattern = "; ")[[1]]

    max.bf.peaks <- c()
    for(c in 1:length(curr.peak.starts)){
      curr.start.msec <- as.numeric(curr.peak.starts[c])
      curr.end.msec <- as.numeric(curr.peak.ends[c])

      # get window between peak threshold start and stop
      curr.max.BF.peak <- df %>%
        select(measurement, t, force) %>%
        filter(measurement == curr.measurement) %>%
        filter(t >= curr.start.msec & t <= curr.end.msec) %>%
        arrange(desc(force)) %>%
        ungroup() %>%
        slice(1) %>%
        pull(force)

      max.bf.peaks <- c(max.bf.peaks, round(curr.max.BF.peak,6))
      # plot(curr.peak.window$t, curr.peak.window$force, type = "l")
    }

    BF.starts.ends.tibble$max.bf.peaks[b] <- paste(max.bf.peaks, collapse = "; ")

    if(show.progress == TRUE){
      print_progress(b, nrow(BF.starts.ends.tibble))
    }
  }

  # convert peak table to long format
  # print(paste0("Converting table..."))
  specimens <- unique(BF.starts.ends.tibble$specimen)
  all.peaks.specimen.measurement <- as_tibble(
    setNames(data.frame(
      matrix(nrow = 0,
             ncol = length(c("peaks.starts.specimen.BF", "max.bf.peaks.specimen.BF",
                             "peaks.ends.specimen.BF", "BF")))),
      c("peaks.starts.specimen.BF", "max.bf.peaks.specimen.BF",
        "peaks.ends.specimen.BF", "BF")))

  for(b in 1:length(specimens)){
    curr.specimen <- specimens[b]
    if(!is.na(curr.specimen)){
      # curr.specimen <- BF.starts.ends.tibble %>%
      #   filter(specimen == curr.specimen) %>%
      #   slice(1) %>%
      #   pull(specimen)

      curr.specimen.peaks <- BF.starts.ends.tibble %>% filter(specimen==curr.specimen)
      specimen.measurements <- curr.specimen.peaks %>% pull(measurement)


      for(l in 1:length(specimen.measurements)){
        peaks.starts.specimen.measurement <- str_split(curr.specimen.peaks %>%
                                                         filter(measurement == specimen.measurements[l]) %>%
                                                         pull(starts),
                                                       pattern = "; ")[[1]]
        max.bf.peaks.specimen.measurement <- str_split(curr.specimen.peaks %>%
                                                         filter(measurement == specimen.measurements[l]) %>%
                                                         pull(max.bf.peaks),
                                                       pattern = "; ")[[1]]
        peaks.ends.specimen.measurement <- str_split(curr.specimen.peaks %>%
                                                       filter(measurement == specimen.measurements[l]) %>%
                                                       pull(ends),
                                                     pattern = "; ")[[1]]

        peaks.per.specimen.measurement <- length(max.bf.peaks.specimen.measurement)

        # print(l)
        all.peaks.specimen.measurement <- as_tibble(rbind(all.peaks.specimen.measurement, cbind(peaks.starts.specimen.measurement, max.bf.peaks.specimen.measurement, peaks.ends.specimen.measurement,
                                                                                                specimen = curr.specimen, measurement = specimen.measurements[l])))
      }
    }
    if(show.progress == TRUE){
      print_progress(b, length(specimens))
    }
  }

  # mutate numeric columsn to numeric
  all.peaks.specimen.measurement <- all.peaks.specimen.measurement %>%
    mutate_at(c('peaks.starts.specimen.measurement',
                'max.bf.peaks.specimen.measurement',
                'peaks.ends.specimen.measurement'),
              as.numeric)

  all.peaks.specimen.measurement %>%
    arrange(desc(max.bf.peaks.specimen.measurement))

  # add species to all.peaks.specimen.measurement
  all.peaks.specimen.measurement <- left_join(all.peaks.specimen.measurement,
                                              df %>%
                                                ungroup() %>%
                                                distinct(measurement, species),
                                              by = "measurement")


  # print(paste0("Finding the ", no.of.peaks, " strongest peaks per species..."))
  # find x strongest peaks per species
  strongest.peaks.taxa <- all.peaks.specimen.measurement %>%
    group_by(species) %>%
    arrange(desc(max.bf.peaks.specimen.measurement)) %>%
    slice(1:no.of.peaks)


  # print(paste0("Optimizing peak starts and ends for ", no.of.peaks, " strongest peaks per species..."))
  taxa <- unique(strongest.peaks.taxa$species)
  df.peaks <- as_tibble(setNames(data.frame(matrix(nrow = length(taxa), ncol = length(c("species", "measurements", "starts", "ends")))), c("species", "measurements", "starts", "ends")))

  for(b in 1:length(taxa)){ # length(taxa)
    curr.species <- taxa[b]
    # get data for species
    strongest.peaks.species <- strongest.peaks.taxa %>%
      filter(species == curr.species)

    if(nrow(strongest.peaks.species) < 1){
      warning(paste0("No data found for ", curr.species, "!!!"))
    } else {
      # get peak starts and ends per species
      starts.strongest.species <- strongest.peaks.species %>% pull(peaks.starts.specimen.measurement)
      ends.strongest.species <- strongest.peaks.species %>% pull(peaks.ends.specimen.measurement)
      measurements.strongest.BF <- strongest.peaks.species %>% pull(measurement)

      actual.starts <- c()
      actual.ends <- c()
      measurements <- c()

      for(c in 1:length(starts.strongest.species)){
        curr.start.msec <- as.numeric(starts.strongest.species[c])
        curr.end.msec <- as.numeric(ends.strongest.species[c])
        curr.measurement <- measurements.strongest.BF[c]

        # get window between peak threshold start and stop
        curr.peak.window <- df %>%
          # select("t","V","measurement") %>%
          filter(measurement == curr.measurement) %>%
          filter(t >= curr.start.msec & t <= curr.end.msec) %>%
          select(t, force)

        # plot(curr.peak.window$t, curr.peak.window$force, type = "l")

        # get window before peak threshold start and stop
        msecs.before.peak <- 500

        # create tibble to filter and store actual peak start data (name is same as in next loop to make coding easier)
        curr.peak.window.before.after <- df %>%
          filter(measurement == curr.measurement) %>%
          filter(t >= curr.start.msec-msecs.before.peak & t <= curr.start.msec) %>%
          select(t, force)

        # plot(curr.peak.window.before.after$t, curr.peak.window.before.after$force, type = "l")

        curr.peak.window.before.after <- curr.peak.window.before.after %>%
          mutate(slope = force - lag(force, n = slope.length.start, default = first(force)))

        # plot(curr.peak.window.before.after$t, curr.peak.window.before.after$slope, type = "l")

        # find latest value below slope.thresh
        curr.peak.actual.start <- curr.peak.window.before.after %>%
          filter(slope <= slope.thresh.start) %>%
          arrange(desc(t)) %>%
          ungroup() %>%
          slice(1) %>%
          pull(t)

        # subtract one time step
        t_step <- curr.peak.window.before.after$t[2] - curr.peak.window.before.after$t[1]
        curr.peak.actual.start <- curr.peak.actual.start - t_step

        # get window after peak threshold start and stop
        msecs.after.peak <- 500

        curr.peak.window.before.after <- df %>%
          filter(measurement == curr.measurement) %>%
          filter(t >= curr.end.msec & t <= curr.end.msec+msecs.after.peak) %>%
          select(t, force)

        # plot(curr.peak.window.before.after$t, curr.peak.window.before.after$force, type = "l")

        curr.peak.window.before.after <- curr.peak.window.before.after %>%
          mutate(slope = force - lead(force, n = slope.length.start, default = first(force)))

        # plot(curr.peak.window.before.after$t, curr.peak.window.before.after$slope, type = "l")

        # find earliest value below slope.thresh.end
        curr.peak.actual.end <- curr.peak.window.before.after %>%
          filter(slope <= slope.thresh.end) %>%
          arrange((t)) %>%
          ungroup() %>%
          slice(1) %>%
          pull(t) # + slope.length.msecs

        curr.peak.actual.end <- curr.peak.actual.end + t_step


        # get actual peak window between peak start and stop
        curr.peak.window.actual <- df %>%
          # select("t","V","measurement") %>%
          filter(measurement == curr.measurement) %>%
          filter(t >= (curr.peak.actual.start) & t <= curr.peak.actual.end) %>%
          select(t, force)

        # plot(curr.peak.window.actual$t, curr.peak.window.actual$force, type = "l")

        actual.starts <- c(actual.starts, curr.peak.actual.start)
        actual.ends <- c(actual.ends, curr.peak.actual.end)
        measurements <- c(measurements, curr.measurement)

        # print_progress(c, length(starts.strongest.species))
      }
      # print(b)
      # print(curr.measurement)
      # print(measurements)
      df.peaks$starts[b] <- paste(actual.starts, collapse = "; ")
      df.peaks$ends[b] <- paste(actual.ends, collapse = "; ")
      df.peaks$measurements[b] <- paste(measurements, collapse = "; ")
    }

    df.peaks$species[b] <- curr.species
    if(show.progress == TRUE){
      print_progress(b, length(taxa))
    }
  }
  # df.peaks

  if(!is.null(path.data)){
    # save peak starts and ends
    # print(paste0("Saving df.peaks as ", path.data, today(), "_taxa_starts_ends.csv"))
    write_csv(df.peaks, file.path(path.data, paste0("taxa_starts_ends_", today(), ".csv")))
  }

  return(df.peaks)
  # print("Done!")
}




#' Manually Correct Single Peak
#'
#' Interactive correction of a single peak.
#'
#' When running the function, you will be prompted to select the actual start and end of the current `peak` of the current `measurement`. If more or less
#'   than exactly 2 points are defined, the process is terminated.
#'
#' @param df.peaks The resulting tibble of the function `find_peaks()`. See `?find_peaks` for more details.
#' @param df.data A data frame or tibble in the below format. The columns `t` (time), `force`, `measurement`, and `specimen`.
#'   (measurement ID) must be present. This will usually be the same table that was used before in `find_peaks()`.
#' @param measurement A character string defining the measurement ID (e.g. measurement number) of the peak to be corrected. Must be congruent with the
#'   respective measurement ID within `df.peaks` and `df`.
#' @param peak A numerical value defining the peak to be corrected.
#' @param additional.msecs A numerical value defining the time before and after the originally defined window of the peak to be corrected. Higher
#'   values allow defining start and end points further away from the original start and end points. Default: `500`.
#'   @param path.data A string character defining where to save the result and log file. If `NULL` (default),
#' data is not stored in files. Log files cannot be retrieved in this case.
#' @param path.data A string character defining where to save the results. If `NULL` (default),
#' data is not stored in a file.
#' @details
#' # `df.data` needs to contain the following columns:
#'
#' | **`t`** | **`force`** |  **`measurement`** |
#' | :----: | :----: |:----: |
#' | `t.1` |  `force.1` | `measurement.1` |
#' | `...` |  `...` |  `...` |
#' | `t.n` |  `force.n` | `measurement.m` |
#'
#' @return Changes values within `df.peaks` and returns the changed tibble.
#' @examples
#' # Using the forceR::df.all.200.tax dataset:
#' \donttest{
#' # This function needs user input.
#'peaks.df <- correct_peak(df.peaks = forceR::peaks.df,
#'                         df.data = forceR::df.all.200.tax,
#'                         measurement = "m_01",
#'                         peak = 1,
#'                         additional.msecs = 5)
#' }
#' @export
correct_peak <- function(df.peaks,
                         df.data,
                         measurement,
                         peak,
                         additional.msecs = 500,
                         path.data = NULL){

  oldpar <- par(no.readonly = TRUE)    # code line i
  on.exit(par(oldpar))

  if(sum(colnames(df.peaks) %in% c("starts", "ends", "measurements")) != 3){
    stop ("column names of 'df.peaks' must contain 'starts', 'ends', 'measurements'")
  }

  par(mar=c(5,4,4,2)+0.1, mfrow=c(1,1))

  measurements <- NULL

  if(!is.null(path.data)){
    # create log folder if existing not already
    path.data.manual.peak.start.end.logs <- paste0(path.data, "/manual.peak.start.end.logs/")
    ifelse(!dir.exists(path.data.manual.peak.start.end.logs), dir.create(path.data.manual.peak.start.end.logs), "./manual.peak.start.end.logs already exists")
  }

  curr.measurement <- measurement
  row.number <- which(grepl(measurement,
                            df.peaks %>%
                              pull(measurements)))
  curr.peak.starts <- str_split(df.peaks$starts[row.number], pattern = "; ")[[1]]
  curr.peak.ends <- str_split(df.peaks$ends[row.number], pattern = "; ")[[1]]

  if(length(curr.peak.starts) < peak) stop("There are only ", length(curr.peak.starts), " peaks available for this species - you chose peak no. ", peak, ".")

  curr.peak.start <- as.numeric(curr.peak.starts[peak])
  curr.peak.end <- as.numeric(curr.peak.ends[peak])
  curr.peak.start.old <- curr.peak.start
  curr.peak.end.old <- curr.peak.end

  # add msces before and after
  curr.peak.window.with.frame <- df.data %>%
    filter(measurement == curr.measurement) %>%
    filter(t >= (curr.peak.start-additional.msecs) & t <= (curr.peak.end+additional.msecs)) %>%
    select(t, force)

  curr.peak.window <- curr.peak.window.with.frame %>%
    filter(t >= (curr.peak.start) & t <= (curr.peak.end))

  curr.max.peakF <- max(curr.peak.window.with.frame$force)

  plot(curr.peak.window.with.frame, type="l", col="grey80", lwd=.5, ylim = c(min(curr.peak.window.with.frame$force),max(curr.peak.window.with.frame$force)))
  lines(curr.peak.window, type="l", col="black", lwd=1)
  title(main = paste0(measurement, ", peak: ", peak, ", max = ", round(curr.max.peakF,2), " N"), cex.main = 0.95)

  # manually select new start and end
  print("Select new peak start and end. If more than two points are selected, the operation is automatically terminated.")
  peak.start.end <- NULL
  peak.start.end <- locator(type = "n") # , n=2

  if(!is.null(peak.start.end) & length(peak.start.end$x) == 2){
    round.factor <- 0
    peak.start <- round(peak.start.end$x, round.factor)[1]
    peak.end <- round(peak.start.end$x, round.factor)[2]

    # save new start and end in curr.peak.starts & curr.peak.ends
    curr.peak.starts[peak] <- as.character(peak.start)
    curr.peak.ends[peak] <- as.character(peak.end)

    df.peaks$starts[row.number] <- paste(curr.peak.starts, collapse = "; ")
    df.peaks$ends[row.number] <- paste(curr.peak.ends, collapse = "; ")

    curr.peak.start <- as.numeric(curr.peak.starts[peak])
    curr.peak.end <- as.numeric(curr.peak.ends[peak])

    if(!is.null(path.data)){
      # save little log file with peak and new plot window info
      write_csv(data.frame(measurement = measurement,
                           start.old = curr.peak.start.old, start.new = curr.peak.start,
                           end.old = curr.peak.end.old, end.new = curr.peak.end),
                paste0(path.data.manual.peak.start.end.logs, measurement, "_peak_", peak, "_log.csv"))
    }

    # plot for check
    curr.peak.window.with.frame <- df.data %>%
      filter(measurement == curr.measurement) %>%
      filter(t >= (curr.peak.start-additional.msecs) & t <= (curr.peak.end+additional.msecs)) %>%
      select(t, force)

    curr.peak.window <- curr.peak.window.with.frame %>%
      filter(t >= (curr.peak.start) & t <= (curr.peak.end))

    curr.max.peakF <- max(curr.peak.window$force)

    plot(curr.peak.window.with.frame, type="l", col="grey80", lwd=.5, ylim = c(0,max(curr.peak.window$force)))
    lines(curr.peak.window, type="l", main = paste0(measurement), col="black", lwd=1)
    title(main = paste0(measurement, ", peak: ", peak, ", max = ", round(curr.max.peakF,2), " N"), cex.main = 0.95)
  } else {
    message("Terminated by user.")
  }

  return(df.peaks)
}









#' Peak Duration and Maximum Force
#'
#' Calculate duration and maximum force for each individual peak.
#'
#' @param df.peaks The resulting tibble of the function `find_peaks()`. See `?find_peaks` for more details.
#' @param df.data A data frame or tibble in the below format. The columns `t` (time), `force`, `measurement`, and `specimen`.
#'   (measurement ID) must be present. This will usually be the same table that was used before in `find_peaks()`.
#' @param path.data A string character defining where to save the results. If `NULL` (default),
#' data is not stored in a file.
#' @param show.progress A logical value indicating if progress should be
#' printed to the console. Default: `FALSE`.
#' @details
#' # `df.data` needs to contain the following columns:
#'
#' | **`t`** | **`force`** |  **`measurement`** |
#' | :----: | :----: |:----: |
#' | `t.1` |  `force.1` | `measurement.1` |
#' | `...` |  `...` |  `...` |
#' | `t.n` |  `force.n` | `measurement.m` |
#'
#' @return Changes values within `df.peaks` and returns the changed tibble.
#' @examples
#' # Using the forceR::df.all.200.tax dataset:
#' \donttest{
#' # This function needs user input.
#'peaks.df <- correct_peak(df.peaks = forceR::peaks.df,
#'                         df.data = forceR::df.all.200.tax,
#'                         measurement = "m_01",
#'                         peak = 1,
#'                         additional.msecs = 5)
#' }
#' @export
peak_duration_max_force <- function(df.peaks,
                                    df.data,
                                    path.data = NULL,
                                    show.progress = FALSE){

  if(sum(colnames(df.peaks) %in% c("starts", "ends", "measurements")) != 3){
    stop ("column names of 'df.peaks' must contain 'starts', 'ends', 'measurements'")
  }
  if(sum(colnames(df.data) %in% c("t", "force", "measurement")) != 3){
    stop ("column names of 'df.peaks' must contain 't', 'force', 'measurement'")
  }

  if(!is.null(path.data)){
    # create log folder if existing not already
    path.data.manual.peak.start.end.logs <- paste0(path.data, "/manual.peak.start.end.logs/")
    ifelse(!dir.exists(path.data.manual.peak.start.end.logs), dir.create(path.data.manual.peak.start.end.logs), "./manual.peak.start.end.logs already exists")
  }

  # dplyr NULLs
  start <- end <- measurement <- peak <- NULL

  peaks_1_per_row <- as_tibble(setNames(data.frame(matrix(nrow = 1, ncol = length(c("measurement", "peak", "start", "end", "max_bf")))),
                                        c("measurement", "peak", "start", "end", "max_bf")))
  # change col types and delete first row
  peaks_1_per_row <- peaks_1_per_row %>%
    mutate(measurement = as.character(measurement),
           peak = as.integer(peak)) %>%
    slice(0)

  for(b in 1:nrow(df.peaks)){ # nrow(df.peaks)
    curr.measurements <- str_split(df.peaks$measurements[b], pattern = "; ")[[1]]
    curr.peak.starts <- str_split(df.peaks$starts[b], pattern = "; ")[[1]]
    curr.peak.ends <- str_split(df.peaks$ends[b], pattern = "; ")[[1]]

    for(c in 1:length(curr.peak.starts)){
      curr.measurement <- curr.measurements[c]
      curr.peak.start <- as.numeric(curr.peak.starts[c])
      curr.peak.end <- as.numeric(curr.peak.ends[c])

      # get max_bf of that peak
      curr_df.data <- df.data %>%
        filter(measurement == curr.measurement)
      curr_max_bf <- curr_df.data %>%
        filter(t > curr.peak.start,
               t < curr.peak.end) %>%
        summarise(max_bf = max(force)) %>%
        pull()

      peaks_1_per_row <- bind_rows(peaks_1_per_row,
                                   tibble(measurement=curr.measurement,
                                          peak=c,
                                          start=curr.peak.start,
                                          end=curr.peak.end,
                                          max_bf = curr_max_bf))
    }
    if(show.progress == TRUE){
      forceR::print_progress(b, nrow(df.peaks))
    }
  }

  peaks_1_per_row <- peaks_1_per_row %>%
    mutate(duration = end-start)

  if(!is.null(path.data)){
    # save data
    write_csv(peaks_1_per_row,
              file.path(path.data, paste0("peak_duration_max_force_", today(), ".csv")))
  }
  return(peaks_1_per_row)
}
