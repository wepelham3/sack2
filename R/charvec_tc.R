#' Copy character vector to clipboard for pasting
#'
#' Concatenate the elements of a character vector and copy
#' the collapsed string to the clipboard.
#'
#' Fast way to create text like \code{c("element1", "element2", ...)}
#' for pasting into code.
#'
#' @param x Character vector
#' @param quote Put quotation marks around elements?
#' @param linebreaks Put linebreaks after each element?
#' @export
#' @return Nothing - copies result to clipboard.
#' @examples
#' x <- c("asdjalksjdlaksjd", "12312093u10923012", "asd")
#' charvec_tc(x)
#' charvec_tc(x, quote = FALSE, linebreaks = TRUE)
#' charvec_tc(x, quote = TRUE, linebreaks = FALSE)
#' charvec_tc(x, quote = FALSE, linebreaks = FALSE)

#**********************************************************
charvec_tc <- function(x, quote = TRUE, linebreaks = TRUE){

  if ( ! is.character(x)) stop("x is not of type character.")

  if (quote == TRUE){

    x <- paste0("\"", x, "\"")

  }

  if (linebreaks == TRUE){

    x <- paste0(x, collapse = ",\n")

  }

  if (linebreaks == FALSE){

    x <- paste0(x, collapse = ", ")

  }

  clipr::write_clip(x)
}
#**********************************************************
