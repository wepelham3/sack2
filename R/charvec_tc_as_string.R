#' Send character vector to clipboard as string of collapse values for pasting
#'
#' Experimental. Addresses common situation in which you have a vector of
#' strings that you want to turn into a list like so
#' \code{c("element1", "element2", ...)} for pasting into code.
#'
#' @param x Character vector
#' @param width Width at which to wrap
#' @export
#' @examples
#' charvec_tc_as_string(c("asdjalksjdlaksjd", "12312093u10923012", "asd"))

#**********************************************************
charvec_tc_as_string = function(x, width = 60){

  x <- paste0("'", x, "'")
  x <- paste0(x, collapse = ", ")
  x <- stringr::str_wrap(x, width = width)

  clipr::write_clip(x)
}
#**********************************************************
