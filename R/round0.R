#' Round all numeric columns in a dataframe
#'
#' @importFrom magrittr "%>%"
#' @param data Dataframe whose numeric columns should be rounded.
#' @param digits Number of digits to round to.
#' @export
#' @examples
#' mtcars
#' round0(mtcars, 1)

#**********************************************************
round0 <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

    df[ , nums] <- round(df[ , nums], digits = digits)

    (df)
}
#**********************************************************
