#' Extract Well-Know Text from sf
#'
#' @param sf an sf object
#' @param nrow one or more numbers
#'
#' @return Well-known tex, see \code{\link[sf]{st_as_text}}
#' @export
#'
#' @examples
#' sfp=sf_point(c(1,2),c(4,5))
#' sf_row(sfp,1)
#' sf_row(sfp,c(1,2))
sf_row<-function(sf,nrow){
    if (!any(('sf' %in% class(sf)))){
        if (chinese()) stop(tmcn::toUTF8('\u8F93\u5165\u7684\u7C7B\u578B\u662Fsf'))
        if (!chinese()) stop('The class of input must be sf.')
    }
    if (max(nrow) > nrow(sf)){
        if (chinese()) stop(tmcn::toUTF8('\u884C\u6570\u8D85\u754C'))
        if (!chinese()) stop('nrow is beyond')
    }
    res=list()
    for (i in nrow) {
        listi=list(sf[[1]][[i]])
        names(listi)=i
        res=c(res,listi)
    }
    res
}
