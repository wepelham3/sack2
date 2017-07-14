#' Inspect labeling metadata from .sav file
#'
#' Function that returns dataframe with name, label, values, and value
#' labels for all variables in an SPSS data file.
#'
#' @param path.or.data Either a string indicating location of .sav file or a "labelled" dataframe
#' already imported into R environment.
#' @param describe Logical indicating whether to append descriptive information on each variable,
#' in addition to the SPSS metadata. (Calls \code{describe_df()} if so).
#' @export
#' @examples
#' # SPSS metadata will be blank, since it is an R dataset
#' library(haven)
#' write_sav(mtcars, "mtcars.sav")
#' inspect_sav("mtcars.sav", describe = TRUE)

#**********************************************************
inspect_sav = function(path.or.data, describe = FALSE) {

    if (is.data.frame(path.or.data)) {
        labelled.data = path.or.data
    } else if (file.exists(path.or.data)) {
        labelled.data = haven::read_sav(path.or.data)
    } else {
        stop("Input is neither a valid path nor an existing dataframe in R environment.")
    }

    data = tibble::data_frame(name = names(labelled.data),
                            label = sjlabelled::get_label(labelled.data),
                            mapping = "",
                            values = lapply(labelled.data, sjlabelled::get_values),
                            value.labels = sjlabelled::get_labels(labelled.data))

    # fill new column mapping values to value labels
    for (i in 1 : nrow(data)) {
        if (length(unlist(data$values[i])) == 0 |
            length(unlist(data$value.labels[i])) == 0) next
        data$mapping[i] = paste(
            paste(unlist(data$values[i]),
                  unlist(data$value.labels[i]), sep = " = "),
            collapse = ", ")
    }

    if (describe == TRUE){
      data = data %>%
        dplyr::left_join(describe_df(labelled.data) %>%
                           dplyr::rename(name = variable))
    }

    return(data)
}
#**********************************************************
