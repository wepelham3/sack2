#' Plot imputation convergence for \code{mice} model
#'
#' @param mids.object \code{mids} object containing variables to plot.
#' @param vars Character vector of names of variables to plot.
#' @export
#' @examples
#' plot_mice_convergence(df.imp, c("dv1", "dv2", "dv3"))

#**********************************************************
plot_mice_convergence = function(mids.object, vars) {
    for (i in 1:length(vars)) {

        if (mids.object$nmis[[vars[[i]]]] == 0) next

        plot(mids.object, vars[[i]],
             main = paste0(vars[[i]], "\n[nmis = ", mids.object$nmis[[vars[[i]]]], "]"),
             layout = c(2, 1)) %>%
            print()
    }
}
#**********************************************************
