#' Recode a target column in dataframe to indicator of missingness
#'
#' @description
#' Returns the input dataframe with the target column recoded such that
#' 0 = missing and 1 = present.
#'
#' To facilitate a common task that would otherwise require a pattern like this:
#'
#' \code{
#' df \%>\%
#'   mutate(x = ifelse( ! is.na(x), 1, 0)) \%>\%
#'   count(x)
#' }
#'
#' @details Note that despite the name "missingness indicator", a value of 1 = present.
#'
#' @param data Dataframe
#' @param x String indicating name of target column
#'
#' @export
#' @examples
#' to_missind(mice::boys, "bmi")

#**********************************************************
to_missind <- function(data, x){

  data[[x]] <- ifelse(is.na(data[[x]]), 0, 1)

  data
}
#**********************************************************
