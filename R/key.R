#' Create a random key
#'
#' Create a random sequence of alphanumeric characters.
#'
#' @param length Length of key
#' @export
#' @examples
#' key()
#' key(length = 10)

#**********************************************************
key <- function(length = 5) {

  stringi::stri_rand_strings(n = 1,
                             length = length)

}
#**********************************************************
