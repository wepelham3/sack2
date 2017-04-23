#' Tidy the result of a \code{mitools} analysis
#'
#' Produce a tidy dataframe of the results of a multiple imputation analysis conducted with \code{mitools} package.
#'
#' @param MIresults ((i.e., the output of a \code{mitools::MIcombine} call).
#' @export
#' @examples
#' # non-operative
#' # with(list.of.imps, glm(y ~ x)) %>%
#' #    MIcombine() %>%
#' #    tidy_MIresults()

tidy_MIresults = function(MIresults) {

    summary = summary(MIresults)

    tidy.results = summary %>%
        tibble::rownames_to_column("term") %>%
        dplyr::rename(estimate = results,
                      std.error = se,
                      lo = `(lower`,
                      hi = `upper)`,
                      miss.info = missInfo) %>%
        cbind(df = MIresults$df) %>%
        dplyr::mutate(p.value = 2 * (1 - pt(abs(estimate / std.error), df))) %>%
        dplyr::select(term, estimate, std.error, df, p.value, lo, hi, miss.info)

    return(tidy.results)
}
