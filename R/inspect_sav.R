#' inspect_sav
#'
#' Function that returns dataframe with name, label, values, and value
#' labels for all variables in an SPSS data file.
#'
#' @param path.or.df Either a string indicating location of .sav file or a "labelled" dataframe
#' already imported into R environment.
#' @param view Logical indicating whether to return the dataframe or send to View().
#' @export
#' @examples
#' inspect_sav("my-data.sav")
#' inspect_sav(my.df)
#' inspect_sav(my.df, view = TRUE)

#**********************************************************
inspect_sav = function(path.or.df, view = FALSE) {

    if (is.data.frame(path.or.df)) {
        labelled.df = path.or.df
    } else if (file.exists(path.or.df)) {
        labelled.df = haven::read_sav(path.or.df)
    } else {
        stop("Input is neither a valid path nor an existing dataframe in R environment.")
    }

    df = tibble::data_frame(name = names(labelled.df),
                            label = sjmisc::get_label(labelled.df),
                            mapping = "",
                            values = lapply(labelled.df, sjmisc::get_values),
                            value.labels = sjmisc::get_labels(labelled.df))

    # fill new column mapping values to value labels
    for (i in 1 : nrow(df)) {
        if (length(unlist(df$values[i])) == 0 |
            length(unlist(df$value.labels[i])) == 0) next
        df$mapping[i] = paste(
            paste(unlist(df$values[i]),
                  unlist(df$value.labels[i]), sep = " = "),
            collapse = ", ")
    }

    if (view == FALSE) return(df)
    else View(df)
}
#**********************************************************
