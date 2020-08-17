#' Copy character vector to clipboard as string of collapsed values for pasting
#'
#' Takes a character vector, quotes each element and concatenates them, then
#' copies the collapsed string to clipboard.
#'
#' Fast way to create code like \code{c("element1", "element2", ...)}
#' for pasting into code.
#'
#' @param x Character vector
#' @export
#' @return A string of quoted elements of \code{x}, separated by
#' commas and linebreaks.
#' @examples
#' charvec_tc(c("asdjalksjdlaksjd", "12312093u10923012", "asd"))

#**********************************************************
charvec_tc <- function(x){

  if ( ! is.character(x)) stop("x is not of type character.")

  x <- paste0('"', x, '"')
  x <- paste0(x, collapse = ",\n")

  clipr::write_clip(x)
}
#**********************************************************
