#' Add variable labels from a .sav file or labelled dataframe
#'
#' Appends a column of variable labels, given a column of variable names and a \code{.sav} file
#' or labelled dataframe to get the labels from.
#'
#' @param data Dataframe to append a column of labels to.
#' @param match String indicating the name of the column in \code{data} that contains the variable names.
#' @param from Either a string indicating path to a \code{.sav} file or a "labelled" dataframe.
#' @export
#' @examples
#' # SPSS metadata will be blank, since it is an R dataset
#'
#' library(haven)
#' write_sav(mtcars, "mtcars.sav")
#' labelled.df <- read_sav("mtcars.sav")
#'
#' # using a path to .sav file
#' sack2::describe_df(labelled.df) %>%
#'  add_sav_labels(match = "variable", from = "mtcars.sav")
#'
#' # using a labelled dataframe
#' sack2::describe_df(labelled.df) %>%
#'  add_sav_labels(match = "variable", from = labelled.df)

#**********************************************************
add_sav_labels = function(data, match, from){

  if (is.data.frame(from)) {
    labelled.data = from
  }
  else if (file.exists(from)) {
    labelled.data = haven::read_sav(from)
  }
  else {
    stop("Input is neither a valid path nor an existing dataframe in R environment.")
  }

  label.dictionary <- tibble::tibble(name = names(labelled.data),
                                     label = sjlabelled::get_label(labelled.data)) %>%
    plyr::rename(c("name" = match))

  data <- data %>%
    dplyr::left_join(label.dictionary, by = match)

  return(data)

}
#**********************************************************
