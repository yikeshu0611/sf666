#' Generate Triangle Geometry
#'
#' @param ncol number of columns, default is 1
#' @param nrow number of rows, default is 1
#' @param w weight of each triangle, default is 1
#' @param h height of each triangle, default is 1
#'
#' @return triangle geometry
#' @export
#'
#' @examples
#' triangle()
triangle <- function(ncol=1,nrow=1,w=1,h=1) {
    for (j in 1:nrow) {
        if (j==1) triangel=NULL
        for (i in 1:ncol) {
            p1=c(i*w-w,j*h-h)
            p2=c(i*w  ,j*h-h)
            p3=c(i*w  ,j*h)
            tri_mt=rbind(p1,p2,p3,p1)
            tr_i=st_sf(tri=st_sfc(st_polygon(list(tri_mt))))
            triangel=rbind(tr_i,triangel)
        }
    }
    triangel
}
