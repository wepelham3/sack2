#' Create tidy dataframe that summarizes frequency of values in a vector
#'
#' Akin to 1-dimensional \code{table} with a few differences:
#' it returns as a dataframe,
#' it always includes \code{NA}s,
#' it includes the proportion of elements,
#' it can take a \code{vals(df, var)} input format for easy chaining,
#' it will include value labels when \code{sjlabelled::is_labelled}.
#'
#' Note that the \code{cum.prop} column in resulting dataframe was calculated after
#' sorting based on value.  It will not remain correct if resorted.
#'
#' @param data Either a vector, or a dataframe in which to find \code{var}.
#' @param var When \code{data} is a dataframe, bare name of column to use.
#' @param digits Optional argument to round proportions to specific number of digits (default is 2).
#' @export
#' @examples
#' # first input format uses a (df, name) specification like dplyr
#' vals(mtcars, gear)
#'
#' # second input format just takes a vector
#' vals(mtcars$gear)
#'
#' # with NAs
#' vals(mice::boys, gen)
#'
#' # with more decimals in proportions
#' vals(mice::boys$gen, digits = 3)

#**********************************************************
vals = function(data, var = NULL, digits = 2) {

  if (is.data.frame(data)){

    # otherwise may append the grouping variable(s) onto front of df
    if (dplyr::is_grouped_df(data)) data <- dplyr::ungroup(data)

    vector <- dplyr::select(data, !!rlang::enquo(var)) %>% .[[1]]

  } else if (is.null(var)){

    vector <- data

  } else {
    stop("Cannot parse inputs.")
  }

  result <- data.frame(value = unique(vector)) %>%
    dplyr::mutate(count = ifelse(is.na(value),
                                 purrr::map_int(value, ~ sum(is.na(vector))),
                                 purrr::map_int(value, ~ sum(vector == .x, na.rm = TRUE))),
                  prop = count / length(vector)) %>%
    dplyr::arrange(value) %>%
    dplyr::mutate(cumprop = cumsum(prop)) %>%
    # round, now that cumprop has been computed
    dplyr::mutate_at(.vars = c("prop", "cumprop"),
                     .funs = "round", digits = digits) %>%
    dplyr::select(value, count, prop, cumprop)

  if (sum(is.na(vector)) > 0) {
    result <- result %>%
      dplyr::mutate(prop.nonmissing = ifelse(is.na(value),
                                             NA,
                                             count / length(na.omit(vector))),
                    cumprop.nonmissing = cumsum(prop.nonmissing)) %>%
      # round, now that cumprop.nonmissing has been computed
      dplyr::mutate_at(.vars = c("prop.nonmissing", "cumprop.nonmissing"),
                       .funs = "round", digits = digits)
  }

  if (sjlabelled::is_labelled(vector) == TRUE) {

    labels <- sjlabelled::get_labels(vector, include.values = TRUE) %>%
      broom::tidy() %>%
      rename(value = names,
             label = x) %>%
      mutate(value = as.numeric(value)) # this is a potential source of bugs

    result <- result %>%
      left_join(labels) %>%
      select(label, everything())
  }

  result
}
#**********************************************************
