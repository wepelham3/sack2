#' Get tidy variable importance information from model trained by \code{caret}
#'
#' Function to return a tidy dataframe with variable importance information
#' from a model trained by \code{caret}.  Appends the algorithm and outcome information.
#'
#' Note that an error will be thrown if the method used does not permit a \code{varImp} call,
#' that is, does not yield variable importance scores.  Currently shows a warning and returns
#' an empty dataframe whenever the call to \code{caret::varImp()} returns an error (e.g.,
#' if that algorithm has no variable importance metrics).
#'
#' Writing this as a function that tidily gets variable importance (rather than tidies variable
#' importance returned by \code{varImp}) is necessary for the \code{mutate}+\code{map} paradigm,
#' as calls will otherwise be interrupted by errors.
#'
#' @param trained.model Result of a \code{train()} call in caret.
#' @export
#' @examples
#' model1 <- caret::train(data = mtcars, mpg ~ ., method = "glm")
#' get_varImp(model1)
#'
#' # this one will show a warning and return empty dataframe
#' model2 <- caret::train(data = mtcars, mpg ~ ., method = "ranger")
#' get_varImp(model2)

#**********************************************************
get_varImp = function(trained.model) {

  varimp.results = try({caret::varImp(trained.model)}, silent = TRUE)

  # issue warning and return NULL if varImp() did not work
  if(inherits(varimp.results, "try-error") == TRUE) {
    warning(paste0("Call of caret::varImp() on model with method ", trained.model$method, " returned an error."))
    return(data.frame())
  }

  varimp.results %>%
    .$importance %>%
    tibble::rownames_to_column(var = "predictor") %>%
    dplyr::rename(importance = Overall) %>%
    dplyr::mutate(algorithm = trained.model$method,
                  outcome = trained.model$terms %>%
                    as.character() %>%
                    .[2])
}
#**********************************************************
