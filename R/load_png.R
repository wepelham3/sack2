#' Load a .png file into graphics device
#'
#' For interactive use, when checking that size parameters
#' produce usable graphic for a figure.
#'
#' @param path Path to \code{.png} file.
#' @export
#' @examples
#' # not run
#' # load_png("output/figure1.png")

#**********************************************************
load_png = function(path){

  print(ggplot2::ggplot()) # send grey background to plot device

  png <- png::readPNG(path)

  grid::grid.raster(png)
}
#**********************************************************
