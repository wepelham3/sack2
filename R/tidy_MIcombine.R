#' Combine and tidy the result of a \code{mitools} analysis
#'
#' Produce a tidy dataframe of the results of a multiple imputation analysis
#' conducted with \code{mitools} package.
#'
#' @param results An \code{imputationResultList}, or list of results of an
#' analysis on a series of multiply imputed datasets.
#' @export
#' @examples
#' library(mice)
#' library(mitools)
#'
#' imp <- mice(boys)
#' imp.long <- complete(imp, action = "long")
#' imp.list <- imputationList(split(imp.long, imp.long$.imp))
#'
#' results <- with(imp.list, glm(hgt ~ wgt))
#'
#' tidy_MIcombine(results)

tidy_MIcombine = function(results) {

  MIresults = mitools::MIcombine(results)

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
