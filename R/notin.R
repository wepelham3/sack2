#' Test whether an element is NOT in a set
#'
#' A negated \code{%in%} for convenience.  Avoids ugly expressions with the negation
#' not immediately apparent.
#'
#' @export
#' @examples
#' 6 %in% c(6, 8, 10)
#' ! 6 %in% c(6, 8, 10)
#' 6 %notin% c(6, 8, 10)

#**********************************************************
`%notin%` = function(x, table) {
  match(x, table, nomatch = 0L) == 0L
}
#**********************************************************
