#' Source specific lines in an R file
#'
#' @param file character string with the path to the file to source.
#' @param start row number of first line in \code{file} to source.
#' @param end row number of last line in \code{file} to source.
#' @export
#' @examples
#' # non-operative
#' # source_lines("load-data.R", start = 50)

source_lines = function (file, start = NULL, end = NULL){

  if (length(start) > 1) stop("Length of start > 1.")
  if (length(end) > 1) stop("Length of end > 1.")

  lines <- readLines(file)

  if (is.null(start)) start <- 1

  if (is.null(end)) end <- length(lines)

  source(textConnection(lines[start:end]))
}
