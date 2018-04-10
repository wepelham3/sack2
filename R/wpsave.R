#' ggsave() wrapper that saves as .png+.pdf then loads to viewer
#'
#' Wraps \code{ggsave()}, but saves plot as both \code{.pdf}
#' and \code{.png}, then loads plot to viewer.
#'
#' @param .path Path at which to save file
#' @param .height Height in inches
#' @param .width Width in inches
#' @export
#' @examples
#' # not run
#' # wpsave("output/figure1", .height = 4, .width = 4, )

#**********************************************************
wpsave = function(.path, .height, .width){

  ggsave(filename = paste0(.path, ".pdf"),
         width = .width, height = .height)

  ggsave(filename = paste0(.path, ".png"),
         width = .width, height = .height)

  print(ggplot2::ggplot())

  png <- png::readPNG(paste0(.path, ".png"))

  grid::grid.raster(png)
}
#**********************************************************
