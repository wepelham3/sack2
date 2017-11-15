#' Plot ECDF
#'
#' Less typing to get \code{ggplot2::stat_ecdf}.
#' Can accept dataframe with bare or quoted name,
#' or vector.
#'
#' @param data Either a vector, or a dataframe in which to find \code{var}.
#' @param var When \code{data} is a dataframe, bare or quoted name of column to use.
#' @export
#' @examples
#' plot_ecdf(mtcars$mpg)
#' plot_ecdf(mtcars, mpg)
#' plot_ecdf(mtcars, "mpg")

#**********************************************************
plot_ecdf = function(data, var = NULL){

  if (is.data.frame(data)){

    # this will do nothing if var is a string
    var <- as.character(substitute(var))

    vector <- data[[var]]

  } else if (is.null(var)){

    vector <- data

  } else {
    stop("Cannot parse inputs.")
  }

  ggplot2::ggplot(data = data.frame(x = vector), aes(x = x)) +
    ggplot2::stat_ecdf() +
    ggplot2::scale_y_continuous(breaks = seq(0, 1, by = 0.2))
}
#**********************************************************
