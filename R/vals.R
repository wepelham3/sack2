#' Create tidy dataframe that summarizes frequency of values in a vector
#'
#' Akin to 1-dimensional \code{table} with a few differences:
#' it returns as a dataframe,
#' it always includes \code{NA}s,
#' it includes the proportion of elements,
#' it can take a \code{vals(df, var)} input format for easy chaining.
#'
#' Note that the \code{cum.prop} column in resulting dataframe was calculated after
#' sorting based on value.  It will not remain correct if resorted.
#'
#' @param data Either a vector, or a dataframe in which to find \code{var}.
#' @param var When \code{data} is a dataframe, bare name of column to use.
#' @export
#' @examples
#'
#' # first input format uses a (df, name) specification like dplyr
#' vals(mtcars, gear)
#'
#' # second input format just takes a vector
#' vals(mtcars$gear)
#'
#' # with NAs
#' vals(mice::boys, gen)
#' vals(mice::boys$gen)

#**********************************************************
vals = function(data, var = NULL) {

  if (is.data.frame(data)){

    # otherwise may append the grouping variable(s) onto front of df
    if (dplyr::is_grouped_df(data)) data <- dplyr::ungroup(data)

    vector <- dplyr::select(data, !!rlang::enquo(var)) %>% .[[1]]

  } else if (is.null(var)){

    vector <- data

  } else {
    stop("Cannot understand inputs.")
  }

  df = data.frame(value = unique(vector)) %>%
    dplyr::mutate(count = ifelse(is.na(value),
                                 purrr::map_int(value, ~ sum(is.na(vector))),
                                 purrr::map_int(value, ~ sum(vector == .x, na.rm = TRUE))),
                  prop = count / length(vector)) %>%
    dplyr::arrange(value) %>%
    dplyr::mutate(cum.prop = cumsum(prop)) %>%
    # round, now that cum.prop has been computed
    dplyr::mutate_at(.vars = c("prop", "cum.prop"),
                     .funs = "round", 2)

  return(df)
}
#**********************************************************
