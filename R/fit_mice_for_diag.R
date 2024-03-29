#' Create a \code{mice} object for diagnostics
#'
#' Function that creates a \code{mice} object for diagnostics, with
#' \code{m = 5}. Iteratively increases the number of iterations by 5 until
#' Rhat < \code{Rhat.threshold} for mean and variance of all variables in imputation model.
#' Thus, accessing \code{$iteration} in the resulting \code{mice} object
#' will give the value of \code{maxit} to be used in the final \code{m > 5}
#' imputation model.
#'
#' Always uses same random seed. Includes a dataframe with all Rhat values at
#' each step, which can be accessessed via \code{mice$rhat}. See \code{mice::mice}
#' for more documentation of arguments.
#'
#' @param ... Arguments to be passed to \code{mice} call (typically, dataframe,
#' method vector, and prediction matrix).
#' @param iter10 Logical switch indicating whether function should use the
#' iterative procedure to find a sufficient number of iterations. When
#' \code{TRUE}, will stop after fitting a \code{m = 5, maxit = 10} model.
#' Included for convenience when working with code that includes the function.
#' @param Rhat.threshold Maximum acceptable R-hat statistic
#' @param final.maxit Number of iterations after which search should stop, regardless
#' of whether Rhat threshold has been met.
#' @export
#' @examples
#' library(mice)
#' pred.matrix <- quickpred(boys, mincor = .10, minpuc = 0.50)
#' mice.m5 <- fit_mice_for_diag(data = boys,
#'                              predictorMatrix = pred.matrix,
#'                              method = c("pmm"))
#' mice.m5$m
#' mice.m5$iteration
#' mice.m5$rhat

#**********************************************************
fit_mice_for_diag = function (..., iter10 = FALSE,
                              Rhat.threshold = 1.10, final.maxit = 100){

  current.maxit <- 10

  current.max.Rhat <- 999

  df.Rhats <- tibble::tibble()

  while (current.max.Rhat > Rhat.threshold) {

    .mice <- mice::mice(seed = 7191992, m = 5, maxit = current.maxit,
                        printFlag = FALSE, diagnostics = TRUE, ...)

    if (iter10 == TRUE) return(.mice)

    Rhats <- miceadds::Rhat.mice(.mice) %>%
      cbind(maxiter = current.maxit)

    current.max.Rhat <- Rhats %>%
      dplyr::select(Rhat.M.imp, Rhat.Var.imp) %>%
      purrr::map_dbl(max, na.rm = TRUE) %>%
      max()

    cat("----------------------------------")
    cat("\n")
    cat(paste0("Finished running maxiter of ...", current.maxit))
    cat("\n")
    cat(paste0("This yielded a max(Rhat) of... ", round(current.max.Rhat, 2)))
    cat("\n")
    cat("----------------------------------")
    cat("\n")

    df.Rhats <- df.Rhats %>%
      dplyr::bind_rows(Rhats)

    if (current.maxit >= final.maxit) break

    current.maxit <- current.maxit + 5
  }

  .mice$rhat <- df.Rhats

  .mice
}
#**********************************************************
