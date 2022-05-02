#' Summarize Table
#'
#' Finds minimum, maximum and standard deviation of force per measurement and taxon and
#'  creates summary tibble.
#'
#' @param df Data frame or tibble containing at least three columns. The column names must contain the
#' grouping variables defined in `var1` and `var2` and the column `force` (time series of force measurements).
#' @param var1 A character string defining the column to calculate minimal and maximal force values per measurement.
#'   This must be the column that contains the unique measurement ID, e.g. measurement number.
#' @param var2 A character string defining the column for which the summary should be calculated.
#' @return A tibble summarizing the input data frame `df`. The resulting tibble will contain
#' the columns `t`, `force`, `measurement`, `species`, `specimen`, `amp`, `lever.ratio`,
#' `max.F.measurement`, `mean.F.specimen`, `max.F.specimen`, `sdv.max.F.specimen`, `n.measurements.in.specimen`.
#' @examples
#' # Using the forceR::df.all.200.tax dataset:
#'
#' # sumarize by measurement and specimen
#' df.summary.specimen <- summarize_measurements(df = df.all.200.tax,
#'                                               var1 = "measurement",
#'                                               var2 = "specimen")
#'
#'
#'#  plot results
#' \dontrun{
#' require(ggplot2)
#' ggplot(data = df.summary.specimen, mapping = aes(x=specimen,y=max.F.measurement)) +
#'   geom_jitter(aes(color='blue'),alpha=0.7) +
#'   geom_boxplot(fill="bisque",color="black",alpha=0.3) +
#'   # scale_y_log10() +
#'   labs(y="max(F)/specimen") +
#'   guides(color="none") +
#'   theme_minimal()
#' }
#' @export
summarize_measurements <- function(df, var1, var2){

  if(sum(colnames(df) %in% c("species", "specimen", "measurement", "t", "force")) != 5){
    stop ("column names of 'df' must contain 'species', 'specimen', 'measurement', 't', 'force'.")
  }
  if(!is.character(var1)) stop ("'var1' must be a character string.")
  if(!is.character(var2)) stop ("'var2' must be a character string.")

  if(sum(colnames(df) %in% var1) != 1){
    stop (paste0("column names of 'df' must contain '", var1, "' as defined in 'var1'."))
  }
  if(sum(colnames(df) %in% var2) != 1){
    stop (paste0("column names of 'df' must contain '", var2, "' as defined in 'var2'."))
  }

  max.F.var1 <- t <- force <- NULL

  # df <- df.all.200.tax
  # var1 <- "measurement"
  # var2 <- "species"
  # print(paste0("Summary will be created for '", var1, "' and '", var2, "'."))

  var1.col.no <- which(colnames(df) == var1)
  colnames(df)[var1.col.no] <- "var1"
  var2.col.no <- which(colnames(df) == var2)
  colnames(df)[var2.col.no] <- "var2"

  data.sumarized <- df %>%
    # find max Fs of measurements
    group_by(var1) %>%
    # calculate max force values for each var1
    mutate(max.F.var1 = max(force)) %>%
    # keep only one row per specimen
    slice(1) %>%
    # find mean F values for taxa (= species)
    group_by(var2) %>%
    # calculate species mean of max. Fs of all specimens of one ID (=species)
    mutate(mean.F.var2 = round(mean(max.F.var1),6),
           max.F.var2 = max(max.F.var1),
           sdv.max.F.var2 = sd(max.F.var1),
           # count observations per var2
           n.var1s.in.var2 = n()) %>%
    # keep only one row per var2 (= species)
    # slice(1) %>%
    ungroup()

  colnames(data.sumarized) <- gsub("var1", var1, colnames(data.sumarized))
  colnames(data.sumarized) <- gsub("var2", var2, colnames(data.sumarized))

  if("t" %in% colnames(data.sumarized)){
    data.sumarized <- data.sumarized %>%
    select(-t)
  }
  if("force" %in% colnames(data.sumarized)){
    data.sumarized <- data.sumarized %>%
      select(-force)
  }

  return(data.sumarized)
}
