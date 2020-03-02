#' Create sf Line
#'
#' @param ... two-numeric vectors or two-column matrix or list of them
#' @param colname colname for simple feature geometry list column
#' @param rownames rownames for each sf rows
#' @param crs see \code{\link[sf]{sf}}
#' @importFrom sf st_linestring st_multilinestring
#' @details sf_line() is a cellection of st_sf(), st_sfc(), st_linestring,
#'     st_multilinestring() from 'sf' package. The input must be two-numeric
#'     vectors or two-column matrix or list of them. One independent
#'     vector or matrix or list will be treated as an sf row. If you want
#'     to put them together, use list() function for them. The output is
#'     only sf object. The output has no sfc or sfg.
#' @return an sf object
#' @export
#'
#' @examples
#'
#' library(sf666)
#'
#' # 1.remeber one vector or one matrix will
#' # be treated as a row in sf
#' # 2.remeber one list contain at least two points
#'
#' # create one line: if only one two-numeric vector is given
#' # it will be cbinded will c(0,0)
#' sf_line(c(1,1))
#'
#' # create two lines: 2 rows
#' # if two two-numeric vectors are given
#' # they will be cbinded will c(0,0)
#' sf_line(c(1,1), c(2,4))
#'
#' # if you want to combine together
#' # list() changes the two vectors into a matrix
#' sf_line(list(c(1,1), c(2,4)))
#'
#' # create one line: one matrix is a row
#' mt1=rbind(c(1,1), c(2,4),c(8,9))
#' sf_line(mt1)
#'
#' # if you want to put them together, list them
#' sf_line(list(c(1,2),mt1))
#'
#' # create many lines by more than one matrix
#' # one matrix is a row of sf
#' mt2=rbind(c(1,5),c(2,6),c(5,7))
#' sf_line(mt1,mt2)
#'
#' # if you want to put them together, list them
#' mt3=rbind(c(8,7),c(13,15))
#' sf_line(list(mt1,mt2),c(4,9),mt3)
sf_line<-function(...,colname='geometry',rownames, crs = NA_crs_){
    matrix=list(...)
    mti<-function(matrix,colname=colname,rownames=rownames){
        if (class(matrix)=='numeric'){
            if (length(matrix)==2){
                #if you give one line, we line c(0,0) and it
                sfi=st_sfc(st_linestring(rbind(c(0,0),matrix)),crs = crs)
                text=paste0('st_sf(',colname,'=sfi,row.names = rownames)')
                res=eval(parse(text = text))
                return(res)
            }else{
                if (chinese()) stop(tmcn::toUTF8('\u53EA\u80FD\u4F7F\u75282\u4E2A\u6570\u5B57\u6765\u521B\u5EFA1\u4E2A\u70B9,\u4F8B\u5982sf_line(c(1,2))'))
                if (!chinese()) stop('One line can only be created by 2 numeber, eg: sf_line(c(1,2))')
            }
        }else if (is.matrix(matrix)){
            if (ncol(matrix) != 2) stop('matrix must have 2 columns')
            if (nrow(matrix)==1){
                sfi=st_sfc(st_linestring(rbind(c(0,0),matrix[1,])),crs = crs)
                text=paste0('st_sf(',colname,'=sfi,row.names = rownames)')
                res=eval(parse(text = text))
                return(res)
            }else{
                sfi=st_sfc(st_linestring(matrix),crs = crs)
                text=paste0('st_sf(',colname,'=sfi,row.names = rownames)')
                res=eval(parse(text = text))
                return(res)
            }
        }else if(is.list(matrix)){
            #check whether two-numeric vector exist in the list
            #if exist, rbind c(0,0) and it
            for (i in 1:length(matrix)) {
                if (class(matrix[[i]])=='numeric'){
                    if (length(matrix[[i]])==2){
                        matrix[[i]]=rbind(c(0,0),matrix[[i]])
                    }
                }
            }
            sfi=st_sfc(st_multilinestring(matrix),crs = crs)
            text=paste0('st_sf(',colname,'=sfi,row.names = rownames)')
            res=eval(parse(text = text))
            return(res)
        }else{
            if (chinese()) stop(tmcn::toUTF8('sf_line()\u7684\u8F93\u5165\u5FC5\u987B\u662F1\u4E2A\u6216\u591A\u4E2A2\u5217\u77E9\u9635,\u4EA6\u6216\u662F1\u4E2A\u542B\u67092\u4E2A\u6570\u5B57\u7684\u5411\u91CF'))
            if (!chinese()) stop('The input of sf_line() must be 1 or more two-column matrix or a two-number vector.')
        }
    }
    for (i in 1:length(matrix)) {
        if (i==1) res=NULL
        res=rbind(res,mti(matrix=matrix[[i]],colname=colname,rownames=rownames))
    }
    return(res)
}
