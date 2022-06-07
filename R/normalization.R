#' Rescale Peaks
#'
#' Rescales time series in x and y to values ranging from 0 to 1.
#'
#' @param df.peaks The resulting tibble of the function `find_strongest_peaks()`. See `?find_strongest_peaks` for more details.
#'
#' @param df.data A data frame or tibble in the below format. The columns `t` (time), `force` and `measurement`
#'   (measurement ID) must be present. This will usually be the same table that was used before in `find_strongest_peaks()`.
#'
#' @param plot.to.screen A logical value indicating if results should be
#' plotted in the current R plot device. Default: `FALSE`.
#'
#' @param path.data A string character defining where to save the results.
#' If `NULL`, data will not be saved to disk. Default: `NULL`.
#'
#' @param show.progress A logical value indicating if progress should be
#' printed to the console. Default: `FALSE`.
#'
#' @details
#' `df.peaks` at least needs to contain the following columns:
#'
#' | **`species`** | **`measurements`** |  **`starts`** |  **`ends`** |
#' | :----: | :----: |:----: |:----: |
#' | `species.1` |  `measurements.1` | `starts.1` | `ends.1` |
#' | `...` |  `...` |  `...` |  `...` |
#' | `species.n` |  `measurements.n` | `starts.m` | `ends.m` |
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
#' @return This function returns a tibble in the same format as `df`, but with
#' the additional columns `t.norm` and `force.norm` which will contain the
#'  rescaled time and force data both ranging from 0 to 1.
#'
#' @examples
#' # Using the forceR::df.all.200.tax and forceR::df.all.200.tax datasets:
#'
#' # rescale bites
#' peaks.df.norm <- rescale_peaks(df.peaks = forceR::peaks.df,
#'                                df.data = forceR::df.all.200.tax,
#'                                plot.to.screen = FALSE,
#'                                path.data = NULL,
#'                                show.progress = FALSE)
#'
#'
#' # maximum values of time and force both range from 0 - 1:
#' range(peaks.df.norm$t.norm)
#' range(peaks.df.norm$force.norm)
#'
#' @export
rescale_peaks <- function(df.peaks,
                          df.data,
                          plot.to.screen = FALSE,
                          path.data = NULL,
                          show.progress = FALSE){

  if(sum(colnames(df.peaks) %in% c("measurements","starts","ends")) != 3){
    stop ("column names of 'df.peaks' must contain 'measurements','starts','ends'.")
  }

  if(sum(colnames(df.data) %in% c("t","force","measurement")) != 3){
    stop ("column names of 'df.data' must contain 't','force','measurement'.")
  }

  if(!is.null(path.data)){
    if(!is.character(path.data)) stop ("'path.data' must be a character string.")
    if(!file.exists(path.data)) stop ("Make sure that the folder ", path.data, " (defined by 'path.data') exists.")
  }

  # dplyr nulls
  species <- measurement <- peak <- specimen <- start <- end <- t.norm <- force.norm <- NULL

  #### linear mapping: normalization of x and y ####
  # create tibble with 1 peak per row with start and end
  # print("Converting table to one peak per row...")
  df.peaks.1.per.row <- as_tibble(setNames(data.frame(matrix(nrow = 1, ncol = length(c("measurement", "peak", "start", "end")))),
                                           c("measurement", "peak", "start", "end")))
  # change col types and delete first row
  df.peaks.1.per.row <- df.peaks.1.per.row %>%
    mutate(measurement = as.character(measurement),
           peak = as.integer(peak)) %>%
    slice(0)

  for(b in 1:nrow(df.peaks)){ # nrow(df.peaks)
    curr.peak.starts <- str_split(df.peaks$starts[b], pattern = "; ")[[1]]
    curr.peak.ends <- str_split(df.peaks$ends[b], pattern = "; ")[[1]]
    curr.measurements <- str_split(df.peaks$measurements[b], pattern = "; ")[[1]]

    for(c in 1:length(curr.peak.starts)){
      curr.measurement <- curr.measurements[c]
      curr.peak.start <- as.numeric(curr.peak.starts[c])
      curr.peak.end <- as.numeric(curr.peak.ends[c])
      df.peaks.1.per.row <- bind_rows(df.peaks.1.per.row,
                                      tibble(measurement=curr.measurement,
                                             peak=c,
                                             start=curr.peak.start,
                                             end=curr.peak.end))
    }
    if(show.progress == TRUE){
      print_progress(b, nrow(df.peaks))
    }
  }

  # add specimen to df.peaks.1.per.row
  df.peaks.1.per.row <- left_join(df.peaks.1.per.row,
                                  df.data %>%
                                    ungroup() %>%
                                    # select(-t) %>%
                                    distinct(measurement, specimen),
                                  by="measurement") %>%
    select(specimen, measurement, peak, start, end)

  if(!is.null(path.data)){
    # if(write.data == TRUE){
    # print(paste0("Saving df.peaks.1.per.row at ", path.data, today(), "_peak_starts_ends_1_per_row.csv..."))
    write_csv(df.peaks.1.per.row, file.path(path.data, paste0("peak_starts_ends_1_per_row_", today(), ".csv")))
  }

  # print(paste0("Calculating rescaled curves for all ", length(unique(paste0(df.peaks.1.per.row$measurement, df.peaks.1.per.row$peak))), " peaks (this may take a while)..."))

  # save all peaks with their peak forces and rescaled peak forces in one tibble (multi-core)
  df.data.red.for.looping <- df.data %>%
    select(measurement, t, force)

  curves_df_norm <- NULL
  for(i in 1:nrow(df.peaks.1.per.row)){
    curr.peak <- df.peaks.1.per.row$peak[i]
    curr.measurement <- df.peaks.1.per.row$measurement[i]
    curr.peak.start <- df.peaks.1.per.row$start[i]
    curr.peak.end <- df.peaks.1.per.row$end[i]

    curr.peak.window <- df.data.red.for.looping %>%
      ungroup() %>%
      filter(measurement == curr.measurement,
             t >= curr.peak.start & t <= curr.peak.end)

    # check if there is data for this combination
    if(nrow(curr.peak.window) == 0) stop("No data found for peak ", curr.peak,
                                         " of measurement ", curr.measurement, ".")

    curr.peak.window <- curr.peak.window  %>%
      mutate(t.peak = t - first(t)) %>%
      mutate(t.norm =  round(seq(0, 1, length.out = n()),6),
             force.norm = round(rescale_to_range(force, from = 0, to = 1),6),
             peak = curr.peak) %>%
      select(measurement, peak, t.norm, force.norm) # t, force,

    curves_df_norm <-  rbind(curves_df_norm, curr.peak.window)

    if(show.progress == TRUE){
      print_progress(i, nrow(df.peaks.1.per.row))
    }
  }

  # print("Rescaling finished.")

  rm(df.data.red.for.looping)

  # re-add all info stored in df.data
  curves_df_norm <- left_join(curves_df_norm,
                              df.data %>%
                                group_by(measurement) %>%
                                slice(1),
                              by = "measurement") %>%
    select(-c(t, force))

  if(!is.null(path.data)){
    # if(write.data == TRUE){
    # print(paste0("Saving curves_df_norm at ", path.data, "/", today(), "_curves_df_norm.csv..."))
    write_csv(curves_df_norm, file.path(path.data, paste0("curves_df_norm_", today(), ".csv")))
  }

  return(curves_df_norm)
  # print("Done!")
}




#' Reduce Peaks
#'
#' Reduces curves to 100 observations per peak.
#'
#' @param df The resulting tibble of the function `rescale_peaks()`. See `?rescale_peaks` for more details.
#'   The columns `measurement` and `force.norm` must be present.
#'
#' @param plot.to.screen A logical value indicating if results should be
#' plotted in the current R plot device. Default: `FALSE`.
#'
#' @param path.data A string character defining where to save the results.
#' If `NULL`, data will not be saved to disk. Default: `NULL`.
#'
#' @param path.plots A string character defining where to save result plots.
#' If `NULL`, plots will not be saved to disk. Default: `NULL`.
#'
#' @param show.progress A logical value indicating if progress should be
#' printed to the console. Default: `FALSE`.
#'
#' @details
#' # `df` needs to contain the following columns:
#'
#' | **`force`** |  **`measurement`** | **`peak`** |
#' | :----: |:----: |:----: |
#' |  `force.norm.norm.1` | `measurement.1` | `peak.1` |
#' |  `...` |  `...` |  `...`
#' |  `force.norm.n` | `measurement.m` | `peak.n` |
#'
#' @return This function returns a tibble with a similar format as `df`, but the columns `t`, `force`, `t.norm` and `force.norm` are replaced
#'   by the columns `index`, ranging from 1 to 100, and `force.norm.100`, containing the rescaled force data ranging from 0 to 1. Since the time series
#'   has been reduced to 100 observations, this tibble will always contain 100 rows per peak.
#'
#' @examples
#' # Using the forceR::df.all.200.tax dataset:
#' peaks.df.norm.100 <- red_peaks_100(df = forceR::peaks.df.norm,
#'                                    path.data = NULL,
#'                                    path.plots = NULL,
#'                                    show.progress = FALSE)
#'
#'peaks.df.norm.100
#'
#' @export
red_peaks_100 <- function(df,
                          plot.to.screen = FALSE,
                          path.data = NULL,
                          path.plots = NULL,
                          show.progress = FALSE){

  oldpar <- par(no.readonly = TRUE)    # code line i
  on.exit(par(oldpar))

  if(sum(colnames(df) %in% c("measurement","force.norm")) != 2){
    stop ("column names of 'df' must contain 'measurement','force.norm'.")
  }

  if(!is.null(path.data)){
    if(!is.character(path.data)) stop ("'path.data' must be a character string.")
    if(!file.exists(path.data)) stop ("Make sure that the folder ", path.data, " (defined by 'path.data') exists.")
  }

  if(!is.null(path.plots)){
    if(!is.character(path.plots)) stop ("'path.plots' must be a character string.")
    if(!file.exists(path.plots)) stop ("Make sure that the folder ", path.plots, " (defined by 'path.plots') exists.")
  }

  peak <- force.norm <- measurement <- specimen <- NULL

  # reduce all peaks to 100 observations and plot
  # if(write.pdfs == TRUE){
  if(!is.null(path.plots)){
    if(!is.character(path.plots)) stop ("'path.plots' must be a character string")
    # print(paste0("Saving plots at ", path.plots, "/", today(),"_rescaled_peaks_100.pdf..."))
    # on.exit(invisible(dev.off()), add = TRUE)
    pdf(file.path(path.plots, paste0("rescaled_peaks_100_", today(), ".pdf")),
        onefile = TRUE, paper = "a4", height = 14)
  }
  par(mfrow=c(3,2))
  species <- unique(df$species)
  df.peaks.100 <- NULL
  for(b in 1:length(species)){ # length(species)
    curr.species <- species[b]

    curr.species.norm.df <- df %>%
      filter(species == curr.species)

    peaks <- unique(curr.species.norm.df %>%
                      pull(peak))

    for(c in peaks){
      curr.peak.F <- curr.species.norm.df %>%
        filter(peak == c) %>%
        pull(force.norm)

      curr.measurement <- curr.species.norm.df %>%
        slice(1) %>% pull(measurement)

      curr.specimen <- curr.species.norm.df %>%
        slice(1) %>% pull(specimen)

      curr.peak.F.100 <- tibble(species = curr.species,
                                measurement = curr.measurement,
                                specimen = curr.specimen,
                                peak = c,
                                index = seq(1, 100, length.out = 100),
                                force.norm.100 = spline(x=seq(0, 1, length.out = length(curr.peak.F)), y=curr.peak.F, n = 100)$y)

      if(plot.to.screen == TRUE){
        # plot each peak
        # plot((curr.species.norm.df %>%
        #        filter(peak == c) %>%
        #        pull(t.norm)),
        #      curr.species.norm.df %>%
        #        filter(peak == c) %>%
        #        pull(force.norm),
        #      type="l", lwd=5, col="grey80")

        plot(curr.peak.F.100$index/100, curr.peak.F.100$force.norm.100, type="l", lwd=.5)
        title(main = paste0(curr.species, " (meas. ", curr.measurement, ", spec. ", curr.specimen, "); peak ", c), cex.main = 0.95)
      }
      df.peaks.100 <- rbind(df.peaks.100, curr.peak.F.100)
    }

    if(show.progress == TRUE){
      print_progress(b, length(species))
    }
  }

  # par(mfrow=c(1,1))
  if(!is.null(path.plots)){
    invisible(dev.off())
  }

  if(!is.null(path.data)){
    # if(write.data == TRUE){
    write_csv(df.peaks.100, file.path(path.data, paste0("peaks_100_", today(), ".csv")))
  }
  # print("Done!")
  return(df.peaks.100)
}



#' Average Curves per Group
#'
#' Calculates mean curve shape per group (here: species) and rescales result on the y axis to range from 0 to 1.
#'
#' @param df The resulting tibble of the function `red_peaks_100()`. See `?red_peaks_100` for more details.
#'
#' @param path.data A string character defining where to save the results.
#' If `NULL`, data will not be saved to disk. Default: `NULL`.
#'
#' @return This function returns a tibble made of three columns: `species` containing the species names, `index` ranging from 1 to 100 for each species,
#'   and `force.norm.100` containing the averaged and rescaled curve of each species.
#' @export
#' @examples
#' # Using the forceR::df.all.200.tax dataset:
#'
#' # calculate mean curves per species
#' peaks.df.100.avg <- avg_peaks(df = forceR::peaks.df.norm.100,
#'                                path.data = NULL)
#' \donttest{
#' # plot averaged normalized curves per species
#' require(ggplot2)
#' ggplot(peaks.df.100.avg,
#'           aes(x = index ,
#'                y = force.norm.100.avg,
#'                colour=species)) +
#'   geom_line()
#' }

avg_peaks <- function(df,
                      path.data = NULL){

  if(sum(colnames(df) %in% c("species", "index", "force.norm.100")) != 3){
    stop ("column names of 'df' must contain 'species', 'index', 'force.norm.100.")
  }

  if(!is.null(path.data)){
    if(!is.character(path.data)) stop ("'path.data' must be a character string.")
    if(!file.exists(path.data)) stop ("Make sure that the folder ", path.data, " (defined by 'path.data') exists.")
  }


  species <- index <- force.norm.100 <- NULL

  df.peaks.100.avg <- tibble(aggregate(force.norm.100 ~ species + index, df, mean) %>%
                               arrange(species, index)) %>%
    # rescale y to range from 0-1 again (Not the case anymore due to ID-wise averaging)
    group_by(species) %>%
    mutate(force.norm.100.avg = rescale_to_range(force.norm.100, from = 0, to = 1)) %>%
    select(-force.norm.100)

  if(!is.null(path.data)){
    # if(write.data == TRUE){
    write_csv(df.peaks.100.avg, file.path(path.data, paste0("peaks_100_avg_", today(), ".csv")))
  }

  # print("Done!")
  return(df.peaks.100.avg)
}
