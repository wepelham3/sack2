#' Return tidy summary of vector
#'
#' Function to return a one-row dataframe with descriptives for a target vector
#' Mirrors \code{describe_df}, but for standalone vectors.
#'
#' Will coerce using \code{as.vector()} to handle factors or labelled vectors.
#' Count in the \code{distinct} column excludes values of \code{NA}.
#'
#' @param x Vector.
#' @param digits Optional argument to round output to a specified number of digits.
#' @export
#' @examples
#' describe(mtcars$mpg)
#' describe(mtcars$mpg, 2)
#' describe(mice::boys$hc)
#'
#' # handling of a factor
#' describe(mice::boys$gen)
#'
#' # NAs are excluded from the count of 'distinct'
#' describe(c(0, 1, 1, 0, NA))
#'
#' # a common use case, distinct from describe_df()
#' describe(mtcars$hp / mtcars$wt)

#**********************************************************
describe = function(x, digits = NULL) {


  if (is.data.frame(x)) stop("x is a dataframe")

  x <- as.vector(x)

  if ( ! is.vector(x)) stop("x could not be coerced to a vector.")

  n <- sum(!is.na(x))
  nmis <- sum(is.na(x))

  distinct <- length(na.omit(unique(x)))

  results <- tibble::tibble(n, nmis, distinct)

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

  results
}
#**********************************************************
