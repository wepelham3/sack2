#' Return a count of each missing data pattern in dataframe
#'
#' Function to return a dataframe with a count of rows with
#' each unique missing data pattern.
#'
#' @param data Dataframe.
#' @export
#' @examples count_NA_patterns(mice::boys)

#**********************************************************
count_NA_patterns = function(data){

  vars <- names(data)

  data %>%
    purrr::map_df(~ ifelse(is.na(.x), ".", "X")) %>%
    dplyr::count_(var = vars)
}
#**********************************************************
