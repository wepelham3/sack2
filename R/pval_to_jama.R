#' Convert p values to JAMA format
#'
#' Converts a vector of p values into character vector that reflect
#' JAMA journal standards for the reporting of p values.
#'
#' @param p Vector of p values.
#' @export
#' @examples
#' pval_to_jama(c(.50, .0004, .000000001, .098, .044, .90))
#**********************************************************
pval_to_jama <- function(p){

  out <- dplyr::case_when(p < .001 ~ "<.001",
                          p < .01 ~ sprintf("%0.3f", p),
                          p >= .01 ~ sprintf("%0.2f", p))

  stringr::str_replace(out, "^0\\.", ".")

}
#**********************************************************
