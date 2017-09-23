#' Save session info as .txt file
#'
#' Will be named \code{session-info.txt}.
#'
#' @export
#' @examples
#' save_session_info()

#**********************************************************
save_session_info = function(){
  sink("session-info.txt")
  print(devtools::session_info())
  sink()
}
#**********************************************************
