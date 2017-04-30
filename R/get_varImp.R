#' Get tidy variable importance information out of model trained by \code{caret}
#'
#' Function to return a tidy dataframe with variable importance information
#' from a model trained by \code{caret}.  Appends the algorithm and outcome information.
#'
#' @param trained.model Result of a \code{train()} call in caret.
#' @export
#' @examples
#' model <- caret::train(data = mtcars, mpg ~ ., importance = TRUE)
#' get_varImp(model)

#**********************************************************
get_varImp = function(trained.model) {
  caret::varImp(trained.model) %>%
    .$importance %>%
    tibble::rownames_to_column(var = "predictor") %>%
    dplyr::rename(importance = Overall) %>%
    dplyr::mutate(algorithm = trained.model$method,
                  outcome = trained.model$call %>%
                    as.character() %>%
                    .[2] %>%
                    stringr::word(1))
}
#**********************************************************
