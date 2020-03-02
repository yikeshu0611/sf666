#' Create sf Polygon
#'
#' @param ... one or more two-column matrix or list of two-column matrix
#' @param colname colname for simple feature geometry list column
#' @param row.names rownames
#' @param crs see \code{\link[sf]{sf}}
#' @importFrom sf st_multipolygon
#' @details sf_polygon() is a cellection of st_sf(), st_sfc(), st_polygon,
#'     st_multipolygon() from 'sf' package. The input must be matrix or list.
#'     One independent matrix or list will be treated as a sf row.
#'     The output is an sf object. The output has no sfc or sfg.
#' @return an sf object
#' @export
#'
#' @examples
#' library(sf666)
#' # remeber: a matrix, a map, is a row
#' # if you want to put them together, use list() to treat them into a list
#'
#' p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
#' p2 <- rbind(c(1,1), c(1,2), c(2,2), c(1,1))
#' # single row
#' sf_polygon(p1)
#' # two rows
#' sf_polygon(p1,p2)
#' # use list() to combine them together
#' sf_polygon(list(p1,p2))
#'
#' p3 <- rbind(c(3,0), c(4,0), c(4,1), c(3,1), c(3,0))
#' p4 <- rbind(c(3.3,0.3), c(3.8,0.3), c(3.8,0.8), c(3.3,0.8), c(3.3,0.3))[5:1,]
#' p5 <- rbind(c(3,3), c(4,2), c(4,3), c(3,3))
#' # a matix, a map, is an af row
#' # five rows, five maps
#' sf_polygon(p1,p2,p3,p4,p5)
#' # put p1 and p2 together: four rows
#' sf_polygon(list(p1,p2),p3,p4,p5)
#'
#' # again, put p3 and p4 together: three rows
#' # the difference from st_multipolygon for sf package is:
#' # sf_polygon can be given matrix and list, each will be treated as a row
#' # if you want to put them together, list them again
#' sf_polygon(list(p1,p2), list(p3,p4), p5)
#'
#' # again, put list(p1,p2) and list(p3,p4) together in one row
#' sf_polygon(list(list(p1,p2), list(p3,p4)), p5)
sf_polygon <- function(...,colname='polygon',row.names,crs=NA_agr_){
    dd<-list(...)
    for (i in 1:length(dd)) {
        if (i==1) res=NULL
        ddi=dd[[i]]
        if (class(ddi)=='matrix'){
            sfcres=paste0('st_sf(',colname,'=st_sfc(st_polygon(list(ddi))),row.names=row.names,crs=crs)')
            res=rbind(res,eval(parse(text = sfcres)))
        }else if (class(ddi)=='list'){
            if (all(sapply(ddi, class)=='matrix')){
                # a list contain of many matrix
                sfcres=paste0('st_sf(',colname,'=st_sfc(st_polygon(ddi)),row.names=row.names,crs=crs)')
                res=rbind(res,eval(parse(text = sfcres)))
            }else if (all(sapply(ddi, class)=='list')){
                sfcres=paste0('st_sf(',colname,'=st_sfc(st_multipolygon(ddi)),row.names=row.names,crs=crs)')
                res=rbind(res,eval(parse(text = sfcres)))
            }
        }
    }
    res
}
