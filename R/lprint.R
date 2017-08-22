#' Print all rows of a tibble
#'
#' "Long print" to override standard tibble behavior.
#'
#' Functionally, an alternative to \code{View}.
#'
#' @export
#' @examples
#' tibble <- tibble::as_tibble(mtcars)
#'
#' lprint(tibble)
#'
#' # cf. suppression behavior
#' print(tibble)

#**********************************************************
lprint = function(tibble) {
  print(tibble, n = 10000)
}
#**********************************************************
