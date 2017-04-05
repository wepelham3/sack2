#' fit_mice_for_diag
#'
#' Function that creates a \code{mice} object for diagnostics, with
#' \code{m = 5}. Iteratively increases the number of iterations by 5 until
#' Rhat < 1.05 for mean and variance of all variables in imputation model.
#' Thus accessing \code{$iteration} in the resulting \code{mice} object
#' will give the value of \code{maxit} to be used in the final \code{m > 5}
#' imputation model.
#'
#'See \code{mice::mice} for more documentation of arguments.
#'
#' @param data Dataframe to be imputed.
#' @param pred Square matrix indicating which variables should be used as predictors of which other variables.
#' @param method Character vector of methods to use for each variable in imputation model.
#' @export
#' @examples
#' fit_mice_for_diag(data = df.to.imp, pred = pred.matrix, method = mice.vars$method)

#**********************************************************
fit_mice_for_diag = function(data, pred, method) {

    # start with 10 iterations
    current.maxit = 10
    # initialize max.rhat above threshold
    current.max.rhat = 2

    while (current.max.rhat > 1.05) {
        data.imp = mice::mice(data = data, seed = 07191992, m = 5, maxit = current.maxit,
                              pred = pred, method = method, printFlag = FALSE, diagnostics = TRUE)

        # statistical checks with Gelman/Rubin
        current.max.rhat = miceadds::Rhat.mice(data.imp) %>%
            dplyr::select(Rhat.M.imp, Rhat.Var.imp) %>%
            purrr::map_dbl(max, na.rm = TRUE) %>%
            max()

        current.maxit = current.maxit + 5
    }

    cat("----------------------------------")
    cat("\n")
    cat(paste0("Final number of maxiter was... ", current.maxit - 5))
    cat("\n")
    cat(paste0("This yielded a max(Rhat) of... ", current.max.rhat))
    cat("\n")
    cat("----------------------------------")

    return(data.imp)
}
#**********************************************************
