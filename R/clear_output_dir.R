#' Delete any visible files in output/
#'
#' For use in standard repo structure.
#'
#' @export
#' @examples
#' clear_output_dir()

#**********************************************************
clear_output_dir = function() {

  files <- list.files("output/", full.names = TRUE)

  file.remove(files)

}
#**********************************************************
