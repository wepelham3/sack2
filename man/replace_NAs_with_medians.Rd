% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/replace_NAs_with_medians.R
\name{replace_NAs_with_medians}
\alias{replace_NAs_with_medians}
\title{Replace missing values in a dataframe with medians}
\usage{
replace_NAs_with_medians(data)
}
\arguments{
\item{data}{Dataframe.}
}
\description{
Function to replace missing values in each variable with the median value on that variable.
Factor and character columns will be ignored.
}
\examples{
mice::boys
replace_NAs_with_medians(mice::boys)
}
