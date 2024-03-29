% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{peaks.df}
\alias{peaks.df}
\title{Starts and Ends of the 5 Strongest Peaks}
\format{
A data frame with 4 rows and 4 columns:
\describe{
\item{species}{species names}
\item{measurements}{measurement names}
\item{starts}{start values, in ms}
\item{ends}{end values, in ms}
}
}
\usage{
peaks.df
}
\description{
Start and end time values of the 5 strongest peaks per species of \code{df.all}
with the names of the measurements in which they occur.
}
\details{
Result of \code{rescale_peaks()}.
}
\keyword{datasets}
