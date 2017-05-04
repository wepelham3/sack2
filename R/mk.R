#' Source \code{MAKEFILE.R}
#'
#' Ignores case.
#'
#' @export
#' @examples
#' mk()

#**********************************************************
mk = function() {
  source(list.files(pattern = "MAKEFILE.R", ignore.case = TRUE))
}
#**********************************************************
