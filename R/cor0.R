#' Create tidy summary of pairwise correlations of vars in a dataframe
#'
#' Wraps typical workflow of \code{df \%>\% as.matrix() \%>\% Hmisc::rcorr() \%>\% broom::tidy()}.
#'
#' Drops all factors with more than 2 levels.  Converts factors with 2 levels to numerics
#' via \code{as.numeric(x) - 1}.
#'
#' @param data Dataframe.
#' @export
#' @examples
#' cor0(mtcars)
#' cor0(mice::boys)

#**********************************************************
cor0 = function(data) {

  ncols.valid <- sum(purrr::map_dbl(data, ~ is.numeric(.x) | (is.factor(.x) & nlevels(.x == 2))))

  ncols.dropped = ncol(data) - ncols.valid

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

  return(summary)
}
#**********************************************************
