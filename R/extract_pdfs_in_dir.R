#' Gather all \code{.pdf} files in a directory, recursively
#'
#' For use with Zotero, which exports PDFs of a batch of articles into
#' individual folders. Copies of all PDFs in directory will be placed into
#' a new folder at top of directory named \code{extracted-pdfs}.
#'
#' @param dir Directory to search for PDFs
#' @export
#' @examples
#' # non-operative
#' # extract_pdfs_in_dir(dir = "~/Downloads/syllabus-causalinference")

#**********************************************************
extract_pdfs_in_dir <- function(dir){

  outdir <- paste0(dir, "/extracted-pdfs")

  dir.create(outdir)

  paths <- list.files(dir,
                      full.names = TRUE,
                      recursive = TRUE)

  pdf.paths <- stringr::str_subset(paths, "pdf$")

  pdf.filenames <- stringr::str_extract(pdf.paths,
                                        "([^\\/]+$)")

  file.copy(from = pdf.paths,
            to = paste0(outdir, "/", pdf.filenames))
}
#**********************************************************
