#' Tidy the result of a \code{mitools} analysis
#'
#' Produce a tidy dataframe of the results of a multiple imputation analysis conducted with \code{mitools} package.
#'
#' @param MIresults (i.e., the output of a \code{mitools::MIcombine} call).
#' @export
#' @examples
#' library(mice)
#' library(mitools)
#'
#' imp <- mice(boys)
#' imp.long <- complete(imp, action = "long")
#' imp.list <- imputationList(split(imp.long, imp.long$.imp))
#'
#' mi.results <- MIcombine(with(imp.list, glm(hgt ~ wgt)))
#'
#' tidy_MIresults(mi.results)

tidy_MIresults = function(MIresults) {

    summary = summary(MIresults)

    tidy.results = summary %>%
        tibble::rownames_to_column("term") %>%
        dplyr::rename(estimate = results,
                      std.error = se,
                      lo = `(lower`,
                      hi = `upper)`) %>%
        cbind(miss.info = MIresults$missinfo) %>%
        cbind(df = MIresults$df) %>%
        dplyr::mutate(p.value = 2 * (1 - pt(abs(estimate / std.error), df))) %>%
        dplyr::select(term, estimate, std.error, df, p.value, lo, hi, miss.info)

    return(tidy.results)
}
