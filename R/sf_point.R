#' Create sf Point
#'
#' @param ... two-numeric vectors or two-column matrix or list of them
#' @param colname colname for simple feature geometry list column
#' @param rownames rownames for each sf rows
#' @param crs see \code{\link[sf]{sf}}
#' @details sf_point() is a cellection of st_sf(), st_sfc(), st_point,
#'     st_multipoint() from 'sf' package. The input must be two-numeric
#'     vectors or two-column matrix or list of them. One independent
#'     vector or matrix or list will be treated as an sf row. If you want
#'     to put them together, use list() function for them. The output is
#'     only sf object. The output has no sfc or sfg.
#' @return an sf object
#' @export
#'
#' @examples
#' library(sf666)
#'
#' # remeber one vector or one matrix will
#' # be treated as a row in sf
#'
#' # create one point: 1 row
#' sf_point(c(1,1))
#'
#' # create two points: 2 rows
#' sf_point(c(1,1), c(2,4))
#'
#' # create two points: 1 rows
#' # list() changes the two vectors into a matrix
#'
#' sf_point(list(c(1,1), c(2,4)))
#'
#' # create three points: 2 rows
#' mt1=rbind(c(1,1), c(2,4))
#' sf_point(mt1,c(1,2))
#'
#' # if you want to put them together, list them
#' sf_point(list(c(1,2),mt1))
#'
#' # create many points by more than one matrix
#' # one matrix is a row of sf
#' mt2=rbind(c(1,5),c(2,6),c(5,7))
#' sf_point(mt1,mt2)
#' # if you want to put them together, list them
#' sf_point(list(mt1,mt2),c(4,9),mt1)
sf_point<-function(...,colname='point',rownames, crs = NA_crs_){
    matrix<-list(...)
    # process one list to be one matrix
    if (any(sapply(matrix, class)=='list')){
        for (i in 1:length(matrix)) {
            if (class(matrix[[i]])=='list'){
                mt_list =  matrix[[i]]
                if (length(mt_list)==1){
                    matrix[[i]]=mt_list[[1]]
                }else{
                    for (j in 1:length(mt_list)) {
                        if (j==1) mt_rbind=NULL
                        mt_rbind=rbind(mt_rbind,mt_list[[j]])
                    }
                    matrix[[i]]=mt_rbind
                }
            }
        }
    }
    mti<-function(matrix,colname=colname,rownames=rownames){
        if (class(matrix)=='numeric'){
            if (length(matrix)==2){
                sfi=st_sfc(st_point(matrix),crs = crs)
                text=paste0('st_sf(',colname,'=sfi,row.names = rownames)')
                res=eval(parse(text = text))
                return(res)
            }else{
                if (chinese()) stop(tmcn::toUTF8('\u53EA\u80FD\u4F7F\u75282\u4E2A\u6570\u5B57\u6765\u521B\u5EFA1\u4E2A\u70B9,\u4F8B\u5982sf_point(c(1,2))'))
                if (!chinese()) stop('One point can only be created by 2 numeber, eg: sf_point(c(1,2))')
            }
        }else if (is.matrix(matrix)){
            if (ncol(matrix) != 2) stop('matrix must have 2 columns')
            if (nrow(matrix)==1){
                sfi=st_sfc(st_point(c(matrix[,1],matrix[,2])),crs = crs)
                text=paste0('st_sf(',colname,'=sfi,row.names = rownames)')
                res=eval(parse(text = text))
                return(res)
            }else{
                sfi=st_sfc(st_multipoint(matrix),crs = crs)
                text=paste0('st_sf(',colname,'=sfi,row.names = rownames)')
                res=eval(parse(text = text))
                return(res)
            }
        }else{
            if (chinese()) stop(tmcn::toUTF8('sf_point()\u7684\u8F93\u5165\u5FC5\u987B\u662F1\u4E2A\u6216\u591A\u4E2A2\u5217\u77E9\u9635,\u4EA6\u6216\u662F1\u4E2A\u542B\u67092\u4E2A\u6570\u5B57\u7684\u5411\u91CF'))
            if (!chinese()) stop('The input of sf_point() must be 1 or more two-column matrix or a two-number vector.')
        }
    }
    for (i in 1:length(matrix)) {
        if (i==1) res=NULL
        res=rbind(res,mti(matrix=matrix[[i]],colname=colname,rownames=rownames))
    }
    return(res)
}
