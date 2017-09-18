#' Create a \code{mice} object for diagnostics
#'
#' Function that creates a \code{mice} object for diagnostics, with
#' \code{m = 5}. Iteratively increases the number of iterations by 5 until
#' Rhat < 1.05 for mean and variance of all variables in imputation model.
#' Thus, accessing \code{$iteration} in the resulting \code{mice} object
#' will give the value of \code{maxit} to be used in the final \code{m > 5}
#' imputation model.
#'
#'See \code{mice::mice} for more documentation of arguments.
#'
#' @param ... Arguments to be passed to \code{mice} call (typically, dataframe, method vector, and prediction matrix).
#' @param iter10 Logical switch indicating whether function should use the iterative procedure
#' to find a sufficient number of iterations. When \code{TRUE}, will stop after fitting a \code{m = 5, maxit = 10} model. Included for convenience when working with code
#' that includes the function.
#' @export
#' @examples
#' library(mice)
#' pred.matrix <- quickpred(boys, mincor = .10, minpuc = 0.50)
#' df.imp.m5 <- fit_mice_for_diag(data = boys,
#'                                predictorMatrix = pred.matrix,
#'                                method = c("pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "polr", "pmm", "pmm"))
#' df.imp.m5$m
#' df.imp.m5$iteration

#**********************************************************
fit_mice_for_diag = function(..., iter10 = FALSE) {

    # start with 10 iterations
    current.maxit <- 10
    # initialize max.rhat above threshold of 1.05
    current.max.rhat <- 2

    while (current.max.rhat > 1.05) {
        data.imp <- mice::mice(seed = 07191992, m = 5, maxit = current.maxit,
                               printFlag = FALSE, diagnostics = TRUE, ...)

        # stop running if iter10 switch is on
        if (iter10 == TRUE) return(data.imp)

        # if not, continue with statistical checks (Gelman/Rubin)
        current.max.rhat <- miceadds::Rhat.mice(data.imp) %>%
            dplyr::select(Rhat.M.imp, Rhat.Var.imp) %>%
            purrr::map_dbl(max, na.rm = TRUE) %>%
            max()

        current.maxit <- current.maxit + 5
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
