#' Pad a number with leading zeroes
#'
#' @param x Numeric vector to pad with leading zeroes
#' @export
#' @examples
#' pad_number(c(1, 2, 12))
#' pad_number(c(1, 2, 120))
#'

#**********************************************************
pad_number <- function(x) {

  if ( ! is.numeric(x)) stop("x is not numeric")

  if (length(na.omit(x)) == 0) stop("x has no non-missing values")

  stringr::str_pad(x,
                   width = floor(log10(max(na.omit(x)))) + 1,
                   side = 'left',
                   pad = '0')

}
#**********************************************************

