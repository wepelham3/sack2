#' Return tidy summary of vector
#'
#' Function to return a dataframe with descriptives for a target vector
#' Mirrors \code{describe_df}, but for standalone vectors.
#'
#' Note that the count in the \code{distinct} column excludes values of \code{NA}.
#'
#' @param x Vector.
#' @param digits Optional argument to round output to a specified number of digits.
#' @export
#' @examples
#' describe_vec(mtcars$mpg)
#' describe_vec(mtcars$mpg, 2)
#' describe_vec(mice::boys$hc)
#'
#' # handling of a factor
#' describe_vec(mice::boys$gen)
#'
#' # NAs are excluded from the count of 'distinct'
#' describe_vec(c(0, 1, 1, 0, NA))
#'
#' # a common use case, distinct from describe_df()
#' describe_vec(mtcars$hp / mtcars$wt)

#**********************************************************
describe_vec = function(x, digits = NULL) {

  n <- sum(!is.na(x))
  nmis <- sum(is.na(x))

  distinct <- length(na.omit(unique(x)))

  results <- tibble::data_frame(n, nmis, distinct)

  if (is.numeric(x)){
    mean <- mean(x, na.rm = TRUE)
    sd <- sd(x, na.rm = TRUE)
    min <- min(x, na.rm = TRUE)
    p10 <- quantile(x, probs = 0.10, na.rm = TRUE)
    p25 <- quantile(x, probs = 0.25, na.rm = TRUE)
    median <- quantile(x, probs = 0.50, na.rm = TRUE)
    p75 <- quantile(x, probs = 0.75, na.rm = TRUE)
    p90 <- quantile(x, probs = 0.90, na.rm = TRUE)
    max <- max(x, na.rm = TRUE)

    results <- cbind(results,
                     mean, sd, min, p10, p25, median, p75, p90, max)
  }

  if (!is.null(digits)){
    results <- sack2::round0(results, digits)
  }

  rownames(results) <- c() # inherits rownames from the quantile() calls

  return(results)
}
#**********************************************************
