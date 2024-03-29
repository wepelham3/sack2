% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/charvecs_to_named_charvec_tc.R
\name{charvecs_to_named_charvec_tc}
\alias{charvecs_to_named_charvec_tc}
\title{Combined two character vectors into code declaring a named character vector on the clipboard}
\usage{
charvecs_to_named_charvec_tc(names, values)
}
\arguments{
\item{names}{character vector with the names for the named character vector written to clipboard}

\item{values}{character vector with the values for the named character vector written to clipboard}
}
\description{
Takes two character vectors as inputs, writes code declaring a named character vector onto the clipboard. Useful for creating 1:1 maps when recoding variable names.
}
\examples{

old.varnames <- c("v1", "v2", "v3")
new.varnames <- c("age", "height", "weight")

charvecs_to_named_charvec_tc(new.varnames, old.varnames)

# Now paste to see that this text is on your clipboard:
# c('age' = 'v1',
#   'height' = 'v2',
#   'weight' = 'v3')
}
