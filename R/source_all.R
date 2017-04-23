#' Source all R scripts within a folder
#'
#' irectly copied from miceadds package (https://github.com/cran/miceadds);
#' that package loaded too many other extraneous things.
#' @param folder Target folder with scripts to be sourced.
#' @export
#' @examples source_all("helpers/")
#' source_all()

#**********************************************************
# function for sourcing all files within a folder
source_all <- function( path , grepstring= "\\.R" , print.source=TRUE ){

    # vector version of grep
    grep.vec <- function( pattern.vec , x , operator="AND"){
      x0 <- x
      xv <- NULL
      for (vv in 1:(length(pattern.vec) ) ){
        if (operator == "AND"){
          x <- x[ grep( pattern.vec[vv] , x ) ]
        } else {
          xv <- union( xv ,x0[ grep( pattern.vec[vv] , x0 ) ] )
        }
      }
      if (operator!="AND"){ x <- xv }
      index.x <- which( x0 %in% x )
      res <- list( "x" = x , "index.x" = index.x )
      return(res)
    }

    #        files <- list.files( path , grepstring )
    files <- list.files( path  )
    files <- grep.vec(  grepstring , files , "OR")$x
    #		files <- files[ grep( "\\.R" , files ) ]
    for ( ff in files ){
        source( file.path( path , ff ) )
        if ( print.source ){
            cat( paste( "*** source" , ff ) , "\n")
            utils::flush.console()
        }
    }
}
#**********************************************************
