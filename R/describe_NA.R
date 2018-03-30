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

  count.NA <- colSums(is.na(data))

  prop.NA <- round(colMeans(is.na(data)), 2)

  names(count.NA) <- NULL
  names(prop.NA) <- NULL

  return(count.NA)
  data.frame(variable = names(data),
             count.NA,
             prop.NA)

}
#**********************************************************
