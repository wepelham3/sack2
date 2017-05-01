#' Create tidy dataframe that summarizes frequency of values in a vector
#'
#' Akin to \code{table} with a few differences:
#' (1) it returns as a dataframe, and (2) it includes the proportion of elements.
#'
#' Calls print internally and returns dataframe invisibly.
#'
#' @export
#' @examples
#' vals(mtcars$gear)
#' vals(mtcars$qsec)

#**********************************************************
vals = function(vector) {

  df = tibble::data_frame(value = unique(vector)) %>%
    dplyr::mutate(count = purrr::map_int(value, ~ sum(vector == .x, na.rm = TRUE)),
                  prop = round(count / length(vector), 2)) %>%
    dplyr::arrange(values)

  print(df, n = 200)

  invisible(df)

}
#**********************************************************
