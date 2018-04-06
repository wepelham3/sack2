#' Write to clipboard
#'
#' Shorthand wrapper around \code{clipr::write_clip}.
#'
#' @export
#' @examples
#' # tc(df)

#**********************************************************
tc = function(content){

  clipr::write_clip(content)

}
#**********************************************************
