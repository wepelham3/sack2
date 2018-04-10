#' Save dataframe as both .csv and .xlsx
#'
#' Convenience function to save a dataframe as both \code{.csv}
#' and \code{.xlsx}. Useful when working with others who prefer \code{.xlsx}.
#'
#' @param .data Dataframe to write to disk.
#' @param .path Path at which to save file. Note that extension
#' (e.g., \codes{.pdf}) should not be included.
#' @export
#' @examples
#' write_wp()

#**********************************************************
write_wp = function(.data, .path){

  readr::write_csv(.data, paste0(.path, ".csv"))

  writexl::write_xlsx(.data, paste0(.path, ".xlsx"))

}
#**********************************************************
