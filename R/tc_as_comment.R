#' Write a string to the clipboard formatted as an 80-wide R comment
#'
#' Wraps \code{x} to maximum line width of 80 and adds
#' \code{#} symbol at beginning of each line. Useful for long documentation.
#'
#' @param x string to be formatted into an R comment
#'
#' @export
#' @examples
#' tc_as_comment("Call me Ishmael. Some years ago - never mind how long precisely -
#' having little or no money in my purse, and nothing particular to interest me on shore,
#' I thought I would sail about a little and see the watery part of the world. It
#' is a way I have of driving off the spleen, and regulating circulation.")

#**********************************************************
tc_as_comment <- function(x){

  if ( ! is.character(x)) stop("x is not of type character")
  if (length(x) != 1) stop("length of x does not equal 1")

  x <- stringr::str_wrap(x,
                         width = 78)

  x <- stringr::str_replace_all(x,
                                "\n",
                                "\n# ")

  x <- paste0("# ", x)

  clipr::write_clip(x)

}
#**********************************************************
