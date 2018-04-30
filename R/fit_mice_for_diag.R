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
fit_mice_for_diag = function (..., iter10 = FALSE){

  current.maxit <- 10

  current.max.rhat <- 999

  while (current.max.rhat > 1.05) {

    .mice <- mice::mice(seed = 7191992, m = 5, maxit = current.maxit,
                        printFlag = FALSE, diagnostics = TRUE, ...)

    if (iter10 == TRUE) return(.mice)

    current.max.rhat <- miceadds::Rhat.mice(.mice) %>%
      dplyr::select(Rhat.M.imp, Rhat.Var.imp) %>%
      purrr::map_dbl(max, na.rm = TRUE) %>%
      max()

    current.maxit <- current.maxit + 5

    cat("----------------------------------")
    cat("\n")
    cat(paste0("Finished running maxiter of ...", current.maxit))
    cat("\n")
    cat(paste0("This yielded a max(Rhat) of... ", round(current.max.rhat, 2)))
    cat("\n")
    cat("----------------------------------")
    cat("\n")
  }

  .mice
}
#**********************************************************
