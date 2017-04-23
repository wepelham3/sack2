#' Count unique values of vector
#'
#' Function to count number of unique values in a vector.
#'
#' @param vector Vector.
#' @export
#' @examples n_unique(vector)

#**********************************************************
n_unique = function(vec) {
  length(unique(vec))
}
#**********************************************************
