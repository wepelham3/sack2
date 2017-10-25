#' Find and replace patterns in names of a dataframe
#'
#' Designed for use in \code{magrittr} pipelines. Uses \code{stringr::str_replace}.
#'
#' @param data Dataframe.
#' @param pattern Regex pattern
#' @param replacement Regex replacement
#' @export
#' @examples
#' df <- data.frame(idnum = c(1008, 1009, 1010, 1011),
#'                  height.t1 = c(42, 64, 12, 88),
#'                  height.t2 = c(50, 66, 15, 95))
#'
#' df %>%
#'   fnr_names("\\.t", "\\.time") %>%
#'   fnr_names("height", "height.cm")

#**********************************************************
fnr_names = function(data, pattern, replacement){

  if (!is.data.frame(data)) stop ("Input is not a dataframe.")

  old.names <- names(data)

  new.names <- stringr::str_replace(old.names, pattern, replacement)

  names(data) <- new.names

  data
}
#**********************************************************
