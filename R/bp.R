#' Make a noise
#'
#' Uses \code{beepr} to make a noise.
#'
#' @export
#' @examples
#' bp()

#**********************************************************
bp = function() {
  beepr::beep(sound = 5)
}
#**********************************************************
