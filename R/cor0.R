#' Create tidy summary of pairwise correlations of vars in a dataframe
#'
#' Wraps typical workflow of
#' \code{df \%>\% as.matrix() \%>\% Hmisc::rcorr() \%>\% broom::tidy()}.
#' Respects grouped dataframes a la \code{dplyr}.
#'
#' Drops all factors with more than 2 levels and throws a warning.
#' Silently converts factors with 2 levels to numerics via \code{as.numeric(x) - 1}.
#'
#' @param .data Dataframe.
#' @param pattern Optional vector of patterns to match for (i.e., filter for) in
#' the resulting two columns with variable names. Useful if only interested in
#' correlations with a specific variable (e.g., the outcome).
#' @export
#' @examples
#' cor0(mtcars)
#'
#' # with factors and missing data
#' cor0(mice::boys)
#'
#' # with a filter applied
#' cor0(mtcars, "mpg")
#' cor0(mtcars, c("mpg", "cyl"))

#**********************************************************
cor0 = function(.data, pattern = NULL) {

  
  .groups <- NULL
  
  if (dplyr::is_grouped_df(.data) == TRUE){
    
    .groups <- dplyr::groups(.data)
    
    warning("describe_df respected the following grouping vars: ",
            paste0(.groups, collapse = ", "))
    
  }
  
  # drop columns that are non-numeric
  
  cols.to.drop <- purrr::discard(.data,
                                 ~ is.numeric(.x) | (is.factor(.x) & nlevels(.x) == 2)) %>%
    names()
  
  cols.to.drop <- setdiff(cols.to.drop, .groups)

  ncols.dropped <- length(cols.to.drop)

  if(ncols.dropped > 0) {
    
    warning(paste0("Dropped ", ncols.dropped, " non-numeric columns with more than two values before computing correlations:\n    ",
                   paste0(cols.to.drop, collapse = ", ")))
  
  }

  # recode any binary factors to 0/1 numerics
  
  cols.to.binarize <- keep(.data,
                           ~ is.factor(.x) & nlevels(.x) == 2) %>% 
    names() %>% 
    setdiff(.groups)
  
  .data <- .data %>%
    dplyr::mutate_at(cols.to.binarize,
                     ~ as.numeric(.x) - 1)
  
  # define internal function that gets the tidy correlation table
  
  get_correlations = function(.data){
    
    .data.numeric <- .data %>%
      ungroup() %>% 
      select_if(is.numeric)
    
    if (ncol(.data.numeric) != ncol(.data) - ncols.dropped - length(.groups)) {
      
      stop("Fewer columns than expected were passed to Hmisc::rcorr().")
      
    }
    
    .data.numeric %>% 
      as.matrix() %>%
      Hmisc::rcorr() %>%
      broom::tidy() 
    
  }
  
  # apply internal function either by group or to single dataframe
  
  if (dplyr::is_grouped_df(.data) == TRUE){
    
    results <- dplyr::do(.data,
                         get_correlations(.)) %>%
      dplyr::ungroup()

  } else {
    
    results <- get_correlations(.data)
    
  }
  
  # filter results based on pattern argument
  
  if (! is.null(pattern)) {

    results <- results %>%
      dplyr::filter(stringr::str_detect(column1, paste(pattern, collapse = "|")) |
                    stringr::str_detect(column2, paste(pattern, collapse = "|")))

  }

  results
}
#**********************************************************
