#' source_lines
#'
#' Source specific lines in an R file.
#'
#' From https://gist.github.com/christophergandrud/1eb4e095974204b12af9.
#'
#' @param file character string with the path to the file to source.
#' @param lines numeric vector of lines to source in \code{file}.
#' @examples
#' source_lines("load-data.R", 1 : 20)

source_lines <- function(file, lines){
    source(textConnection(readLines(file)[lines]))
}
