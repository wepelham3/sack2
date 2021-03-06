% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/count_NA_patterns.R
\name{count_NA_patterns}
\alias{count_NA_patterns}
\title{Return a count of each missing data pattern in dataframe}
\usage{
count_NA_patterns(data)
}
\arguments{
\item{data}{Dataframe.}
}
\description{
Function to return a dataframe with a count of rows with
each unique missing data pattern.
}
\examples{
count_NA_patterns(mice::boys)
}
