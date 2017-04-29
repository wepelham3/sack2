#' Return tidy summary of NAs in dataframe
#'
#' Function to return a dataframe with description of frequency of NAs
#' in target dataframe.
#'
#' @param data Dataframe.
#' @export
#' @examples describe_NA(mice::boys)

#**********************************************************
describe_NA = function(data) {
    tibble::data_frame(variable = names(data),
                       count.NA = colSums(is.na(data)),
                       prop.NA = colMeans(is.na(data)) %>% round(2))
}
#**********************************************************
