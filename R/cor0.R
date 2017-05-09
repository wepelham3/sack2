#' Create tidy summary of pairwise correlations of vars in a dataframe
#'
#' Wraps typical workflow of \code{df \%>\% as.matrix() \%>\% Hmisc::rcorr() \%>\% broom::tidy()}.
#'
#' Drops all factors with more than 2 levels.  Converts factors with 2 levels to numerics
#' via \code{as.numeric(x) - 1}.
#'
#' @param data Dataframe.
#' @param pattern Optional vector of patterns to match (i.e., filter for) in the resulting
#' two columns with variable names. Useful if only interested in correlations with a specific variable (e.g., the outcome).
#' @export
#' @examples
#' cor0(mtcars)
#'
#' # with factors and missing data
#' cor0(mice::boys)
#'
#' # with a filter applied
#' cor0(mtcars, "mpg")
#' cor0(mtcars, c("mpg", "cyl"))

#**********************************************************
cor0 = function(data, pattern = NULL) {

  ncols.valid <- sum(purrr::map_dbl(data, ~ is.numeric(.x) | (is.factor(.x) & nlevels(.x == 2))))

  ncols.dropped <- ncol(data) - ncols.valid

  if(ncols.dropped > 0) {
    warning(paste0("Dropped [", ncols.dropped, "] non-numeric columns with more than two values before computing correlations."))
  }

  data <- data %>%
    dplyr::mutate_if(.predicate = function(.x){is.factor(.x) & nlevels(.x == 2)},
                     .funs = function(.x){as.numeric(.x) - 1}) %>%
    purrr::keep(is.numeric)

  summary <- data %>%
    as.matrix() %>%
    Hmisc::rcorr() %>%
    broom::tidy()

  if (!is.null(pattern)) {
    summary = summary %>%
      dplyr::filter(stringr::str_detect(column1, paste(pattern, collapse = "|")) |
                    stringr::str_detect(column2, paste(pattern, collapse = "|")))
  }

  return(summary)
}
#**********************************************************
