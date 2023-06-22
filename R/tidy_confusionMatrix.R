#' Tidy report of performance in \code{caret::confusionMatrix()}
#'
#' Extracts all performance metrics embedded in the \code{confusionMatrix}, then removes
#' spaces and capitalization from names for ease in follow-up analyses.
#'
#' @param confusionMatrix The result of a call to \code{caret::confusionMatrix()}
#' @export
#' @examples
#'
#' library(caret)
#'
#' lvs <- c("normal", "abnormal")
#' truth <- factor(rep(lvs, times = c(86, 258)),
#'                 levels = rev(lvs))
#' pred <- factor(
#'   c(
#'     rep(lvs, times = c(54, 32)),
#'     rep(lvs, times = c(27, 231))),
#'   levels = rev(lvs))
#'
#' xtab <- table(pred, truth)
#'
#' mat <- confusionMatrix(xtab)
#'
#' print(mat)
#'
#' tidy_confusionMatrix(mat)

#**********************************************************
tidy_confusionMatrix = function(confusionMatrix) {
  tibble::tibble(metric = attr(c(confusionMatrix$overall, confusionMatrix$byClass), "names"),
                     value = c(confusionMatrix$overall, confusionMatrix$byClass)) %>%
    dplyr::mutate(metric = stringr::str_to_lower(metric) %>%
                    stringr::str_replace_all("\\ ", "\\."))
}
#**********************************************************
