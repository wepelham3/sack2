#' Get tidy training performance of a \code{caret::train} object
#'
#' Function to return a tidy dataframe with performance information
#' from a model trained by \code{caret::train}.
#'
#' Writing this as a function that tidily gets performance (rather than tidies
#' performance returned by \code{getTrainPerf}) is necessary for access to the
#' \code{.$trainingData} for \code{nobs}.
#'
#' @param trained.model Result of a \code{train()} call in caret.
#' @export
#' @examples
#' model1 <- caret::train(data = mtcars, mpg ~ ., method = "glm")
#' get_train_perf(model1)

#**********************************************************
get_train_perf = function(trained.model) {
  trained.model %>%
    caret::getTrainPerf() %>%
    dplyr::select(-method) %>%
    cbind(nobs = nrow(trained.model$trainingData)) %>%
    tidyr::gather(metric, value) %>%
    dplyr::mutate(metric = stringr::str_replace_all(metric, c("^Train" = "")))
}
#**********************************************************
