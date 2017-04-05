#' plot_mice_distrib_binary
#'
#' Plot imputed vs. raw data for a single binary variable in \code{mice} models.
#'
#' @param mids.object \code{mids} object containing variables to plot.
#' @param var.to.plot String of name of the single variable to plot.
#' @export
#' @examples
#' plot_mice_distrib_binary(df.imp, "asian")
#' plot_mice_distrib_binary(df.imp, "ever.medicated")

#**********************************************************
plot_mice_distrib_binary = function(mids.object, var.to.plot) {

    # create df with TRUE/FALSE indicator of whether that row's value on the variable
    # was present in the original data
    df.value.present = tibble::data_frame(present = !is.na(mids.object$data[[var.to.plot]])) %>%
        tibble::rownames_to_column(".id") %>%
        dplyr::mutate(.id = as.factor(.id))

    # create df for plotting
    df.to.plot = mids.object %>%
        mice::complete(action = "long", include = TRUE) %>%
        # add the response indicator for every row
        dplyr::left_join(df.value.present, by = ".id") %>%
        dplyr::select_(.dots = c(".imp", var.to.plot, "present")) %>%
        # retain only truly present values when .imp = 0
        # and only truly missing values when .imp > 0
        dplyr::filter((.imp == 0 & present == TRUE) | (.imp != 0 & present == FALSE)) %>%
        # group and summarise counts for plotting
        dplyr::group_by_(.dots = c(".imp", var.to.plot, "present")) %>%
        dplyr::summarise(count = n()) %>%
        dplyr::ungroup() %>%
        # add back zero-counts so that .imps with no imputed values of 1
        # still show up on plots
        tidyr::complete_(cols = c(".imp", var.to.plot), fill = list(count = 0)) %>%
        dplyr::group_by(.imp) %>%
        dplyr::mutate(total = sum(count),
                      prop = count / total) %>%
        dplyr::ungroup() %>%
        dplyr::filter_(paste0(var.to.plot, " == 1"))

    ggplot2::ggplot(data = df.to.plot, aes_string(x = ".imp", y = "prop")) +
      ggplot2::geom_point(data = filter(df.to.plot, .imp == 0), size = 4, color = "blue") +
      ggplot2::geom_point(data = filter(df.to.plot, .imp != 0), size = 4, color = "red") +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::geom_hline(yintercept = df.to.plot %>% filter(.imp == 0) %>% .$prop, color = "blue") +
      ggplot2::labs(x = "imputation number",
                    y = "proportion where value = 1\n",
                    title =  paste0(var.to.plot, "\n[nmis = ", mids.object$nmis[[var.to.plot]], "]"))

}
#**********************************************************
