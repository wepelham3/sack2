#' Call \code{dplyr::count()} on all columns in a dataframe
#'
#' Useful for situations in which several columns are selected with regex, like
#' so: \code{df \%>\% select(matches("school")) \%>\% count_all()}.
#'
#' @param data Dataframe.
#' @export
#' @examples
#' data <- mtcars[, c("cyl", "gear", "carb")]
#' count_all(data)

#**********************************************************
count_all <- function(data){

  if ( ! is.data.frame(data)) stop("data is not a dataframe.")

  names <- names(data)

  dplyr::count_(data, names)
}
#**********************************************************
