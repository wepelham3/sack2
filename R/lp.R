#' Print dataframe with all rows shown
#'
#' *L*ong *p*rint to override standard data.frame() and tibble() printing behavior.
#'
#' Functionally, an alternative to \code{View}.
#'
#' @param x \code{data.frame()} or \code{tibble()}
#' @export
#' @examples
#'
#' x <- do.call(rbind, replicate(20, as.matrix(mtcars), simplify = FALSE))
#'
#' x <- as.data.frame(x)
#'
#' print(x)
#' lp(x)
#'
#' x <- tibble::as_tibble(x)
#'
#' print(x)
#' lp(x)
#'
#**********************************************************
lp <- function(x){

  if (tibble::is_tibble(x)) print(x, n = 1e5)

  else if (is.data.frame(x)) print(x, max = 1e5)

  else stop("x is neither data.frame() nor tibble()")

}
#**********************************************************
