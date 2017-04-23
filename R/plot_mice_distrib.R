#' Plot imputed vs. raw data for \code{mice} model
#'
#' Will use the \code{mice} plotting methods for continuous variables,
#' and the \code{sack2} custom plotting methods for categorical variables
#' (as indicated by the imputation method used).
#'
#' @param mids.object \code{mids} object containing variables to plot.
#' @param vars Character vector of names of variables to plot.
#' @export
#' @examples
#' plot_mice_distrib(df.imp, c("cont.dv1", "dummy.dv2", "multicat.dv3"))

#**********************************************************
plot_mice_distrib = function(mids.object, vars) {

    for (i in 1:length(vars)) {
        if (mids.object$nmis[[vars[[i]]]] == 0) next
            else if (mids.object$method[[vars[[i]]]] == "logreg") {
                plot_mice_distrib_binary(mids.object, vars[[i]]) %>%
                    print()
            } else if (mids.object$method[[vars[[i]]]] %in% c("polr", "polyreg")) {
                # don't send this to print() because it already calls print in the function
                plot_mice_distrib_poly(mids.object, vars[[i]])
            } else {
                lattice::bwplot(mids.object,
                                as.formula(paste(vars[[i]], "~ .imp")),
                                main = paste0(vars[[i]], "\n[nmis = ", mids.object$nmis[[vars[[i]]]], "]")) %>%
                    print()
                if (mids.object$nmis[[vars[[i]]]] > 1) {
                    lattice::densityplot(mids.object,
                                         as.formula(paste(".imp ~", vars[[i]])),
                                         main = paste0(vars[[i]], "\n[nmis = ", mids.object$nmis[[vars[[i]]]], "]")) %>%
                        print()
                }
            }
    }
}
#**********************************************************
