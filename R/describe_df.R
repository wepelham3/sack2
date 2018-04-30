#' Return tidy summary of dataframe
#'
#' Function to return a dataframe with descriptives for a target dataframe.
#' Each row in the returned dataframe will correspond to a column in the original dataframe.
#' Respects grouped dataframes a la \code{dplyr}.
#'
#' Note that the count in the \code{distinct} column excludes values of \code{NA}.
#'
#' @param data Dataframe.
#' @param digits Optional argument to round output to a specified number of digits.
#' @export
#' @examples
#' describe_df(mtcars)
#' describe_df(mtcars, 2)
#' describe_df(mice::boys)
#'
#' # NAs are excluded from the count of 'distinct'
#' describe_df(data.frame(var1 = c(0, 1, 1, 0, NA)))

#**********************************************************
describe_df = function(data, digits = NULL) {

  if (!is.data.frame(data)) stop("First argument is not a dataframe.")

  .groups <- NULL

  if (dplyr::is_grouped_df(data) == TRUE){

    .groups <- dplyr::groups(data)

    warning("describe_df respected the following grouping vars: ",
            paste0(.groups, collapse = ", "))

  }

  # recode any binary factors to 0/1 numerics

  cols.to.binarize <- keep(data,
                           ~ is.factor(.x) & nlevels(.x) == 2) %>%
    names() %>%
    setdiff(.groups)

  data <- data %>%
    dplyr::mutate_at(cols.to.binarize,
                     ~ as.numeric(.x) - 1)

  warning("describe_df binarized the following vars: ",
          paste0(cols.to.binarize, collapse = ", "))

  # define internal function that summarizes a given dataframe

  get_description = function(data){

    # calculate columns that apply to all variables

    results.all <-
      tibble::data_frame(variable = names(data),
                         n = colSums(!is.na(data)),
                         nmis = colSums(is.na(data)),
                         distinct = purrr::map(data, unique) %>%
                           purrr::map(~ purrr::discard(.x, is.na)) %>%
                           purrr::map_dbl(length))


    data.numeric <- data %>%
      purrr::keep(is.numeric)

    # calculate columns that ONLY apply to numeric variables

    results.numeric <-
      tibble::data_frame(variable = names(data.numeric),
                         mean = purrr::map_dbl(data.numeric, mean, na.rm = TRUE),
                         sd = purrr::map_dbl(data.numeric, sd, na.rm = TRUE),
                         min = purrr::map_dbl(data.numeric, min, na.rm = TRUE),
                         p10 = purrr::map_dbl(data.numeric, quantile, probs = 0.10, na.rm = TRUE),
                         p25 = purrr::map_dbl(data.numeric, quantile, probs = 0.25, na.rm = TRUE),
                         median = purrr::map_dbl(data.numeric, median, na.rm = TRUE),
                         p75 = purrr::map_dbl(data.numeric, quantile, probs = 0.75, na.rm = TRUE),
                         p90 = purrr::map_dbl(data.numeric, quantile, probs = 0.90, na.rm = TRUE),
                         max = purrr::map_dbl(data.numeric, max, na.rm = TRUE))

    dplyr::left_join(results.all, results.numeric)

  }

  # call internal function within do() if a grouped dataframe

  if (dplyr::is_grouped_df(data) == TRUE){

    results <- dplyr::do(data,
                         get_description(.)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(! variable %in% .groups)

  } else {

    results <- get_description(data)

  }

  if (! is.null(digits)){
    results <- sack2::round0(results, digits)
  }

  results
}
#**********************************************************
