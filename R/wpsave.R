#' Wrap ggsave() to save as .png & .pdf, then load to viewer
#'
#' Wraps \code{ggsave()}, but saves plot as both \code{.pdf}
#' and \code{.png}, then loads plot to viewer.
#'
#' @param path Path at which to save file. Note that if file extension is
#' specified, it will be stripped.
#' @param height Height in inches.
#' @param width Width in inches.
#' @param dpi Dots per inch, used for \code{.png}
#' @export
#' @examples
#' # not run; all three will produce same output
#' # wpsave("output/figure1", height = 4, width = 4)
#' # wpsave("output/figure1", height = 4, width = 4, dpi = 1000)

#**********************************************************
wpsave = function(path, height, width, dpi = 300){

  pattern <- "\\.pdf$|\\.png$"

  if (grepl(pattern, x = path)){
    warning("Stripped file extension from path argument.")
  }

  path <- gsub(pattern, "", x = path)

  ggsave(filename = paste0(path, ".pdf"),
         width = width,
         height = height)

  ggsave(filename = paste0(path, ".png"),
         width = width,
         height = height,
         dpi = dpi)

  print(ggplot2::ggplot())

  png <- png::readPNG(paste0(path, ".png"))

  grid::grid.raster(png)
}
#**********************************************************
