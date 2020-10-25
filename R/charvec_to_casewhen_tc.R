#' Create a \code{case_when()} statement for a set of values and copy it to the clipboard
#'
#' @description
#' Takes a vector of values, creates a corresponding \code{case_when()} statement
#' for those values, and copies that statement to the clipboard
#'
#' @param x vector of values... there will be one \code{case_when()} line per unique value
#' @param varname string indicating name of variable for \code{case_when} statement
#' @param sort boolean. Should values in \code{x} be sorted before creating \code{case_when()} lines?
#'
#' @export
#' @examples
#' # charvec_to_casewhen_tc(c("northeast", "west", "south"),
#' #                        varname = "xvar")
#' #
#' # charvec_to_casewhen_tc(c("northeast", "west", "south"),
#' #                        varname = "xvar",
#' #                        sort = FALSE)
#' #
#' # charvec_to_casewhen_tc(c(1, 2, 3),
#' #                        varname = "xvar")
#' #
#' # charvec_to_casewhen_tc(c(1, 2, NA, 3),
#' #                        varname = "xvar")
#' #
#' # charvec_to_casewhen_tc(c(1, 2, NA, 3, 2),
#' #                        varname = "xvar")
#**********************************************************
charvec_to_casewhen_tc <- function(x, varname = "variable", sort = TRUE){

  x <- unique(x)

  x.has.NA <- anyNA(x)

  x <- na.omit(x)

  if (sort == TRUE){

    x <- sort(x)

  }

  if (is.character(x)){

    x <- paste0("'", x, "'")

  }

  lines <- paste0(varname, " == ", x, " ~ ")

  if (x.has.NA == TRUE){

    lines <- c(paste0("is.na(", varname, ") ~ "),
               lines)

  }

  lines <- paste0(lines, collapse = ",\n")

  out <- paste0("case_when(",
                lines,
                ")")

  clipr::write_clip(out)
}
#**********************************************************
