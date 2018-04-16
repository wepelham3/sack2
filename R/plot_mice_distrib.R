#' Plot imputed vs. raw data for \code{mice} model
#'
#' Chooses the type of plot based on \code{method} used for each variable.
#'
#' @param mice \code{mids} object containing variables to plot.
#' @param varlist Character vector of names of variables to plot.
#' @export
#' @examples
#' boys2 <- cbind(boys, dummy = c(NA, NA, NA, rbinom(nrow(boys) - 3, 1, .5))
#' mice <- mice(boys2, method = c("pmm", "pmm", "pmm", "pmm", "pmm", "pmm", "polr", "pmm", "pmm", "logreg"))
#' plot_mice_distrib(mice, c("hgt", "dummy", "phb"))

#**********************************************************
plot_mice_distrib = function(mice, varlist){

  if (is.mids(mice) == FALSE) stop("First argument is not a mids object.")

  # get metadata applicable to all variables in varlist

  data <- mice$data

  nobs <- nrow(data)

  formatting <- list(
    ggplot2::scale_color_manual(values = c("observed" = "blue",
                                           "imputed" = "red")),
    ggplot2::guides(color = FALSE),
    ggplot2::theme_bw(),
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "black",
                                                        fill = NA,
                                                        size = 2))
  )

  # define internal function that makes plots for one variable

  plot_variable = function(varname){

    nmis <- mice$nmis[varname]

    if (nmis == 0) return()

    npres <- nobs - nmis

    method <- mice$method[varname]

    subtitle <- paste0("[npres = ", npres, ", ",
                       "nmis = ", nmis, "]")

    # collect data common to all plots

    .df.obs <- data.frame(.imp = 0,
                          y = data[, varname]) %>%
      dplyr::filter(!is.na(y))

    .df.imp <- mice$imp[[varname]] %>%
      tidyr::gather(.imp, y)

    .df.plot <- rbind(.df.obs, .df.imp) %>%
      dplyr::mutate(source = ifelse(.imp > 0, "imputed", "observed"))

    if (method == "logreg"){

      # make point plot for binary X

      .df.plot.binary <- .df.plot %>%
        dplyr::group_by(.imp, source) %>%
        dplyr::summarise(y = mean(y)) %>%
        dplyr::ungroup()

      .plot.binary <-
        ggplot2::ggplot(data = .df.plot.binary,
                        ggplot2::aes(x = .imp, y = y, color = source)) +
        ggplot2::geom_point(size = 4) +
        ggplot2::geom_hline(yintercept = dplyr::filter(.df.plot.binary, .imp == 0)$y) +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::labs(y = "proportion where value = 1",
                      title = varname,
                      subtitle = subtitle) +
        formatting

      return(.plot.binary)

    }

    if (method %in% c("polr", "polyreg")){

      # make point plot for polytomous X

      .df.plot.poly <- .df.plot %>%
        dplyr::rename(value = y) %>%
        dplyr::group_by(.imp, source) %>%
        dplyr::do(count(., value)) %>%
        dplyr::mutate(total.n = sum(n)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(proportion = n / total.n)

      .plot.stacked <-
        ggplot2::ggplot(data = .df.plot.poly,
                        ggplot2::aes(x = .imp, y = n, fill = value)) +
        ggplot2::geom_bar(stat = "identity", position = "fill") +
        ggplot2::geom_vline(xintercept = 1.5, size = 2) +
        ggplot2::scale_fill_brewer(palette = "Set1") +
        ggplot2::scale_y_continuous(labels = scales::percent) +
        ggplot2::labs(y = "proportion",
                      title = varname,
                      subtitle = subtitle) +
        formatting

      .plot.poly <-
        ggplot2::ggplot(data = .df.plot.poly,
                        ggplot2::aes(x = .imp, y = proportion, color = source)) +
        ggplot2::facet_grid(. ~ value,
                            labeller = "label_both") +
        ggplot2::geom_point(size = 4) +
        ggplot2::geom_hline(data = .df.plot.poly %>%
                              dplyr::filter(.imp == 0),
                            ggplot2::aes(yintercept = proportion)) +
        ggplot2::labs(y = "proportion where value = 1",
                      title = varname,
                      subtitle = subtitle) +
        formatting

      return(list(.plot.stacked,
                  .plot.poly))

    }

    # make boxplot for continuous X

    .plot.box <-
      ggplot2::ggplot(data = .df.plot,
                      ggplot2::aes(x = .imp, y = y, color = source)) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(y = varname,
                    title = varname,
                    subtitle = subtitle) +
      formatting

    # make density plot for continuous x

    .plot.density <-
      ggplot2::ggplot(data = .df.plot,
                      ggplot2::aes(x = y, group = .imp, color = source)) +
      ggplot2::geom_density() +
      ggplot2::labs(y = varname,
                    title = varname,
                    subtitle = subtitle) +
      formatting

    list(.plot.box, .plot.density)

  }

  lapply(varlist, plot_variable)

}
#**********************************************************
