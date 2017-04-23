#' Round all numeric columns in a dataframe
#'
#' @param data Dataframe whose numeric columns should be rounded.
#' @param digits Number of digits to round to.
#' @export
#' @examples round0(df, 0)
#' round0()

#**********************************************************
round0 <- function(df, digits) {
    nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))

    df[ , nums] <- round(df[ , nums], digits = digits)

    (df)
}
#**********************************************************
