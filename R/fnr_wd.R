#' Find and replace string in all \code{.R} files in directory
#'
#' Only looks at \code{.R} files. Recursive.
#'
#' @param existing.string Pattern passed to \code{gsub}.
#' @param replacement.string Replacement passed to \code{gsub}.
#' @export
#' @examples
#' # non-operative
#' # fnr_wd("var1", "number.arrests")
#' # fnr_wd("rating", "teacher.rating)

#**********************************************************
fnr_wd = function(existing.string, replacement.string) {

  files = list.files(pattern = ".R$",
                     recursive = TRUE,
                     ignore.case = TRUE)

  n.replaced <- 0

  for (i in files) {

    content <- readLines(i)

    n.replaced <- n.replaced + sum(stringr::str_count(content, existing.string))

    content.new <- gsub(existing.string, replacement.string, content)
    cat(content.new, file = i, sep = "\n")
  }

  cat(paste0("Finished, replaced ", n.replaced, " instances of '", existing.string, "' in ", getwd(), "."))
}
#**********************************************************
