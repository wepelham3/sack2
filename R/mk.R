#' Source \code{RUNME.R} or \code{MAKEFILE.R}
#'
#' Ignores case.
#'
#' @export
#' @examples
#' mk()

#**********************************************************
mk = function() {

  file <- list.files(pattern = "RUNME.R|MAKEFILE.R", ignore.case = TRUE)

  if (length(file) == 1){
    source(file)
  } else {
    stop("Error: either no RUNME.R or MAKFILE.R, or multiple matches.")
  }

}
#**********************************************************
