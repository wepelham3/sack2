#' Plot imputation convergence for \code{mice} model
#'
#' @param mids result of call to \code{mice()}.
#' @param vars Character vector of names of variables to plot.
#' @export
#' @examples
#' df.imp <- mice(boys, method = c("pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "polr", "pmm", "pmm"))
#' plot_mice_convergence(df.imp, c("hgt", "wgt"))

#**********************************************************
plot_mice_convergence = function(mids, vars) {

  for (i in 1:length(vars)) {

        if (mids$nmis[[vars[[i]]]] == 0) next

        plot(mids, vars[[i]],
             main = paste0(vars[[i]], "\n[nmis = ", mids$nmis[[vars[[i]]]], "]"),
             layout = c(2, 1)) %>%
            print()
    }
}
#**********************************************************
