#' Combined two character vectors into code declaring a named character vector on the clipboard
#'
#' Takes two character vectors as inputs, writes code declaring a named character vector onto the clipboard. Useful for creating 1:1 maps when recoding variable names.
#'
#' @param names character vector with the names for the named character vector written to clipboard
#' @param values character vector with the values for the named character vector written to clipboard
#' @export
#' @examples
#'
#' old.varnames <- c("v1", "v2", "v3")
#' new.varnames <- c("age", "height", "weight")
#'
#' charvecs_to_named_charvec_tc(new.varnames, old.varnames)
#'
#' # Now paste to see that this text is on your clipboard:
#' # c('age' = 'v1',
#' #   'height' = 'v2',
#' #   'weight' = 'v3')


#**********************************************************
charvecs_to_named_charvec_tc <- function(names, values) {

  if ( ! is.character(names) | ! is.character(values)) stop("Both names and values must be character vectors.")

  if (length(names) != length(values)) stop("names and values are of unequal length.")

  paste0("'", names, "' = '", values, "'") %>%
    paste0(collapse = ",\n") %>%
    {paste0("c(", ., ")")} %>%
    sack2::tc()

}
#**********************************************************
