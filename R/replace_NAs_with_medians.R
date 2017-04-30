#' Replace missing values in a dataframe with medians
#'
#' Function to replace missing values in each variable with the median value on that variable.
#' Factor and character columns will be ignored.
#'
#' @param data Dataframe.
#' @export
#' @examples
#' mice::boys
#' replace_NAs_with_medians(mice::boys)

#**********************************************************
replace_NAs_with_medians = function(data) {

  # display warning if NAs will be left in the resulting dataset
  if (any(purrr::map_lgl(data, ~ (is.character(.x) | is.factor(.x)) & sum(is.na(.x)) > 0))) {
    warning(paste("Dataframe contains factor or character variables with NAs.",
                  "These NAs will remain in the resulting dataframe.", sep = "\n  "))
  }

  data[] = lapply(data, function(x) {
    if (is.factor(x)) x
    else if (is.character(x)) x
    else {
      x[is.na(x)] <- median(x, na.rm = TRUE)
      x
    }
  })

  return(data)
}
#**********************************************************
