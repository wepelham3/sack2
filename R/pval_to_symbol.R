#' Convert p values to symbols
#'
#' Converts a vector of p values into character vector of the
#' symbols ***, **, *, †.
#'
#' @param p Vector of p values.
#' @param marginal boolean. Should return dagger for p < .10?
#' @param ns boolean. Should return "ns" for not significant or be empty character string?
#' @export
#' @examples
#' pval_to_symbol(c(.50, .0004, .098, .044))
#' pval_to_symbol(c(.50, .0004, .098, .044), marginal = FALSE)
#' pval_to_symbol(c(.50, .0004, .098, .044), marginal = FALSE, ns = TRUE)

#**********************************************************
pval_to_symbol <- function(p, marginal = TRUE, ns = FALSE){

  if (ns == TRUE) ns <- "ns"
  if (ns == FALSE) ns <- ""

  if (marginal == TRUE){

    p <- ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*",
                              ifelse(p < .10, "†", ns))))

  }

  if (marginal == FALSE){

    p <- ifelse(p < .001, "***",
                ifelse(p < .01, "**",
                       ifelse(p < .05, "*", ns)))
  }

  p
}
#**********************************************************
