#' Set up basic file structure for new repo
#'
#' Creates \code{data/} and \code{output/} folders, plus \code{README.md},
#' \code{RUNME.R}, and \code{01-build-dataset.R}.
#'
#' @export
#' @examples
#' set_up_new_repo()

#**********************************************************
set_up_new_repo = function() {

  file.create("RUNME.R")

  # README may exist from initializing on Github
  if (! file.exists("README.md")) file.create("README.md")

  file.create("01-build-dataset.R")

  dir.create("data")

  dir.create("output")

  sink("output/.gitignore")
  cat("# Ignore everything in this directory\n")
  cat("*\n")
  cat("# Except this file\n")
  cat("!.gitignore")
  sink()

}
#**********************************************************
