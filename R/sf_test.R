#' Test sf class
#'
#' @param x sf, sfc or sfg
#'
#' @return TRUE or FALSE
#' @export
#' @name sfTest
#' @rdname sfTest
is.sf <- function(x){
    'sf' %in% tolower(class(x))
}
#' @rdname sfTest
#' @export
is.sfc<-function(x){
    'sfc' %in% tolower(class(x))
}
#' @rdname sfTest
#' @export
is.sfg<-function(x){
    'sfg' %in% tolower(class(x))
}


