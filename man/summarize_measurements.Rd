% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarizing.R
\name{summarize_measurements}
\alias{summarize_measurements}
\title{Summarize Table}
\usage{
summarize_measurements(df, var1, var2)
}
\arguments{
\item{df}{Data frame or tibble containing at least three columns. The column names must contain the
grouping variables defined in \code{var1} and \code{var2} and the column \code{force} (time series of force measurements).}

\item{var1}{A character string defining the column to calculate minimal and maximal force values per measurement.
This must be the column that contains the unique measurement ID, e.g. measurement number.}

\item{var2}{A character string defining the column for which the summary should be calculated.}
}
\value{
A tibble summarizing the input data frame \code{df}. The resulting tibble will contain
the columns \code{t}, \code{force}, \code{measurement}, \code{species}, \code{specimen}, \code{amp}, \code{lever.ratio},
\code{max.F.measurement}, \code{mean.F.specimen}, \code{max.F.specimen}, \code{sdv.max.F.specimen}, \code{n.measurements.in.specimen}.
}
\description{
Finds minimum, maximum and standard deviation of force per measurement and taxon and
creates summary tibble.
}
\examples{
# Using the forceR::df.all.200.tax dataset:

# sumarize by measurement and specimen
df.summary.specimen <- summarize_measurements(df = df.all.200.tax,
                                              var1 = "measurement",
                                              var2 = "specimen")


#  plot results
\dontrun{
require(ggplot2)
ggplot(data = df.summary.specimen, mapping = aes(x=specimen,y=max.F.measurement)) +
  geom_jitter(aes(color='blue'),alpha=0.7) +
  geom_boxplot(fill="bisque",color="black",alpha=0.3) +
  # scale_y_log10() +
  labs(y="max(F)/specimen") +
  guides(color="none") +
  theme_minimal()
}
}
