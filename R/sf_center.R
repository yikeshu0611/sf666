#' Center of sf
#'
#' @param sf an sf object
#' @param level 1 or 2. If level equals 1, center of each sf object will be
#'     retruned. If level equals 2, center of all sf object will be returned.
#'
#' @return center coordinate
#' @export
#'
#' @examples
#' library(sf666)
#' tri=triangle()
#' sf_center(tri)
sf_center <- function(sf,level=c("1","2")){
    if (!is.character(level)) level=as.character(level)
    level=match.arg(level)
    if (level==1){
        st_centroid(sf)
    }else if (level==2){
        cent1=st_centroid(sf)
        coord=st_coordinates(cent1)
        if (any(coord[1,]!=coord[nrow(coord),])){
            coord=rbind(coord,coord[1,])
        }
        cent2=st_centroid(sf_point(coord))
        st_crs(cent2)=st_crs(sf)
        cent2
    }
}
