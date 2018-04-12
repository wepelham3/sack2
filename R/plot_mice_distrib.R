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
#' boys2 <- cbind(boys, dummy = rbinom(nrow(boys), 1, .5))
#' df.imp <- mice(boys2, method = c("pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "polr", "pmm", "pmm", "logreg"))
#' plot_mice_distrib(df.imp, c("hgt", "dummy", "phb"))

#**********************************************************
plot_mice_distrib = function(mids.object, vars) {

  nobs <- nrow(mids.object$data)

    for (i in 1:length(vars)) {

      varname <- vars[[i]]

      method <- mids.object$method[[varname]]

      nmis <- mids.object$nmis[[varname]]

      npres <- nobs - nmis

      title <- paste0(varname,
                      "\n[nmis = ", nmis, ", ",
                      "npres = ", npres, "]")

        if (nmis == 0) next

            else if (method == "logreg") {

                plot_mice_distrib_binary(mids.object, varname) %>%
                    print()

            } else if (method %in% c("polr", "polyreg")) {

                # don't send this to print() because it already calls print in the function
                plot_mice_distrib_poly(mids.object, varname)

            } else {

                lattice::bwplot(mids.object,
                                as.formula(paste(varname, "~ .imp")),
                                main = title) %>%
                    print()

                if (nmis > 1) {

                    lattice::densityplot(mids.object,
                                         as.formula(paste(".imp ~", varname)),
                                         main = title) %>%
                        print()
                }
            }
    }
}
#**********************************************************

library(mice)
boys2 <- cbind(boys, dummy = rbinom(nrow(boys), 1, .5))
df.imp <- mice(boys2, method = c("pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "polr", "pmm", "pmm", "logreg"))
plot_mice_distrib(df.imp, c("hgt", "dummy", "phb"))
