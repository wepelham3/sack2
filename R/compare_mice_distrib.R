#' Compare imputed vs. raw data for \code{mice} model
#'
#' Returns a dataframe with descriptive statistics for raw vs. imputed data.
#'
#' @param mice \code{mids} object.
#' @export
#' @examples
#' boys2 <- cbind(boys, dummy = c(NA, NA, NA, rbinom(nrow(boys) - 3, 1, .5)))
#' mice <- mice(boys2, method = c("pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "polr", "pmm", "pmm", "logreg"))
# 'compare_mice_distrib(mice)

#**********************************************************
compare_mice_distrib = function(mice){

  if (is.mids(mice) == FALSE) stop("First argument is not a mids object.")

  imp <- mice %>%
    .$imp %>%
    purrr::discard(is.null)

  vars <- names(imp)

  summary.imp <- imp %>%
    purrr::map(~ purrr::reduce(.x, c)) %>%
    purrr::map(sack2::describe_vec) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(n.values = n) %>%
    dplyr::mutate(n.cases = n.values / mice$m,
                  variable = vars,
                  source = "imputed")

  summary.obs <- mice %>%
    .$data %>%
    dplyr::select_at(vars) %>%
    sack2::describe_df() %>%
    dplyr::rename(n.values = n) %>%
    dplyr::mutate(n.cases = n.values,
                  source = "observed") %>%
    dplyr::select(source, variable, n.cases, n.values, dplyr::everything())

  dplyr::bind_rows(summary.obs,
                   summary.imp) %>%
    dplyr::select(-nmis) %>%
    dplyr::mutate(source = factor(source,
                                  levels = c("observed", "imputed"))) %>%
    dplyr::arrange(variable, source)

}
#**********************************************************
