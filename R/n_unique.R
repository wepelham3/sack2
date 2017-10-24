#' Count unique values of vector
#'
#' Function to count number of unique values in a vector. Includes \code{NA} as a value.
#'
#' @param data Either a vector, or a dataframe in which to find \code{var}.
#' @param var When \code{data} is a dataframe, bare or quoted name of column to use.
#' @export
#' @examples
#' n_unique(c(0, 1))
#' n_unique(c(0, 1, NA))
#' n_unique(rnorm(10))
#' n_unique(rbinom(10, 1, .5))
#'
#' # illustrate two formats of dataframe inputs
#'
#' df <- data.frame(x = c(0, 1, 2, NA),
#'                  y = c(0, 0, 0, NA),
#'                  group = c("yes", "no", "no", "yes"))
#'
#' n_unique(df, "x")
#' n_unique(df, x)

#**********************************************************
n_unique = function(data, var = NULL) {

  if (is.data.frame(data)){

    # this will do nothing if var is a string
    var <- as.character(substitute(var))

    vec <- data[[var]]

  } else if (is.null(var)){

    vec <- data

  } else {
    stop("Cannot parse inputs.")
  }

  length(unique(vec))
}
#**********************************************************
