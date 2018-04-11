#' Save dataframe as both .csv and .xlsx
#'
#' Convenience function to save a dataframe as both \code{.csv}
#' and \code{.xlsx}. Useful when working with others who prefer \code{.xlsx}.
#'
#' @param data Dataframe to write to disk.
#' @param path Path at which to save file. Note that if file extension is
#' specified, it will be stripped.
#' @export
#' @examples
#' write_wp(mtcars, "data.csv")

#**********************************************************
write_wp = function(data, path){

  pattern <- "\\.csv$|\\.xlsx$"

  if (grepl(pattern, x = path)){
    warning("Stripped file extension from path argument.")
  }

  path <- gsub(pattern, "", x = path)

  readr::write_csv(data, paste0(path, ".csv"))

  writexl::write_xlsx(data, paste0(path, ".xlsx"))

}
#**********************************************************
