#' Convert p values to symbols
#'
#' Converts a vector of p values into character vector of the
#' symbols ***, **, *, †.
#'
#' @param p Vector of p values.
#' @export
#' @examples
#' pval_to_symbol(c(.50, .0004, .098, .044))

#**********************************************************
pval_to_symbol = function(p) {
  ifelse(p < .001, "***",
         ifelse(p < .01, "**",
                ifelse(p < .05, "*",
                       ifelse(p < .10, "†", ""))))
}
#**********************************************************
