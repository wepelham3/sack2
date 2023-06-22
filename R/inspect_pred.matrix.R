#' Inspect the prediction matrix before imputation
#'
#' Function to check the prediction matrix before imputation.
#' Returns dataframe with following values for each variable in the model:
#' the number of missings, the number of variables used to impute it,
#' and the number of variables it is used to impute.
#'
#' @param data Dataframe that will be used in \code{mice} model.
#' @export
#' @examples
#' pred.matrix = quickpred(boys, mincor = 0.10, minpuc = 0.50)
#' inspect_pred.matrix(boys, pred.matrix)

#**********************************************************
inspect_pred.matrix = function(data, pred.matrix) {
    tibble::tibble(variable = names(data),
                   count.NA = colSums(is.na(data)),
                   prop.NA = colMeans(is.na(data)) %>% round(2),
                   imputed.by = rowSums(pred.matrix),
                   used.in = colSums(pred.matrix))
}
#**********************************************************
