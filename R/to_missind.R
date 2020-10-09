#' Recode a target column in dataframe to indicator of missingness
#'
#' Returns the input dataframe with the target column recoded such that
#' 0 = missing and 1 = present.
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
