#' Print tibble with all rows shown
#'
#' *L*ong *p*rint to override standard tibble behavior.
#'
#' Functionally, an alternative to \code{View}.
#'
#' @export
#' @examples
#' tibble <- tibble::as_tibble(mtcars)
#'
#' lp(tibble)
#'
#' # cf. suppression behavior
#' print(tibble)

#**********************************************************
lp = function(tibble) {
  print(tibble, n = 10000)
}
#**********************************************************
