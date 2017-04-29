#' Return tidy summary of dataframe
#'
#' Function to return a dataframe with descriptives for a target dataframe.
#' Each row in the returned dataframe will correspond to a column in the original dataframe.
#'
#' @param data Dataframe.
#' @export
#' @examples
#' describe_df(mtcars)
#' describe_df(mice::boys)

#**********************************************************
describe_df = function(data) {

  # calculate columns that apply to all variables
  results.all = tibble::data_frame(variable = names(data),
                                   n = colSums(!is.na(data)),
                                   distinct = purrr::map_dbl(data, sack2::n_unique))


  data.numeric = data %>%
    purrr::keep(is.numeric)

  # calculate columns that ONLY apply to numeric variables
  results.numeric = tibble::data_frame(variable = names(data.numeric),
                                       mean = purrr::map_dbl(data.numeric, mean, na.rm = TRUE),
                                       sd = purrr::map_dbl(data.numeric, sd, na.rm = TRUE),
                                       min = purrr::map_dbl(data.numeric, min, na.rm = TRUE),
                                       p10 = purrr::map_dbl(data.numeric, quantile, probs = 0.10, na.rm = TRUE),
                                       p25 = purrr::map_dbl(data.numeric, quantile, probs = 0.25,  na.rm = TRUE),
                                       median = purrr::map_dbl(data.numeric, median, na.rm = TRUE),
                                       p75 = purrr::map_dbl(data.numeric, quantile, probs = 0.75,  na.rm = TRUE),
                                       p90 = purrr::map_dbl(data.numeric, quantile, probs = 0.90,  na.rm = TRUE),
                                       max = purrr::map_dbl(data.numeric, max, na.rm = TRUE))

  results = dplyr::left_join(results.all, results.numeric)

  return(results)
}
#**********************************************************
