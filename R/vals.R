#' Create tidy dataframe that summarizes frequency of values in a vector
#'
#' Akin to 1-dimensional \code{table} with a few differences:
#' (1) it returns as a dataframe, and (2) it includes the proportion of elements.
#'
#' Note that the \code{cum.prop} column in resulting dataframe was calculated after
#' sorting based on value.  It will not remain correct if resorted.
#'
#' @export
#' @examples
#' vals(mtcars$gear)
#'
#' # with NAs
#' vals(mice::boys$gen)

#**********************************************************
vals = function(vector) {

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
