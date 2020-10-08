#' Create a timestamp
#'
#' Create a timestamp with date and time, formatted as such: 2020_10_07_215301.
#'
#' @export
#' @examples
#' tstamp()

#**********************************************************
tstamp <- function() {

  format(Sys.time(),
         "%Y_%m_%d_%H%M%S")

}
#**********************************************************
