#' Count unique values of vector
#'
#' Function to count number of unique values in a vector.
#'
#' @param vector Vector.
#' @export
#' @examples
#' n_unique(rnorm(10))
#' n_unique(rbinom(10, 1, .5))

#**********************************************************
n_unique = function(vec) {
  length(unique(vec))
}
#**********************************************************
