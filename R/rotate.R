#' Rotate
#' @description Rotate two-dimension simple feature.
#' @param sf an sf object
#' @param center can be center1 or center2 or any other two-numeric values
#' @param angel between -180 and 180
#' @name rotate
#' @rdname rotate
#' @importFrom sf NA_agr_ NA_crs_ st_centroid "st_crs<-"
#' @importFrom sf st_geometry "st_geometry<-" st_geometry_type st_multipoint st_point
#' @importFrom sf st_polygon st_sf st_sfc st_coordinates st_crs
#' @return rotated geometry
#' @export
#'
#' @examples
#' library(ggplot2)
#' library(sf666)
#' tri=triangle()
#' ggplot()+
#'     geom_sf(data = tri)+
#'     geom_sf(data = rotate(tri,center = c(0,1),angel = 90))

rotate <- function(sf,center='center2',angel){
    crs=st_crs(sf)
    a=angel/180*pi
    a2=c(cos(a),-sin(a),
         sin(a),cos(a))
    mt=matrix(a2,byrow = T,nrow = 2)
    if (is.character(center)){
        if (center[1]=='center1'){
            center=sf_center(st_geometry(sf))
        }else if (center[1]=='center2'){
            center=sf_center(st_geometry(sf),2)
        }
    }
    if (any(c('sf','sfc','sfg') %in% class(center))){
        if (all(st_geometry_type(center)=='POINT')){
            center=st_coordinates(center)
        }else{
            if (chinese()) stop(paste0(tmcn::toUTF8('sf\u7C7B\u578B\u4E2D\u5FC3\u70B9\u7684geometry\u5FC5\u987B\u662FPOINT')))
            if (!chinese()) stop('center geometry must be point')
        }
    }
    st_geometry(sf)=(st_geometry(sf)-center)*mt+center
    st_crs(sf)=crs
    sf
}
#' @export
#' @rdname rotate
#' @param center_angel a list of center and angle, eg. list('center2',30)
#'
#' ggplot()+
#'     geom_sf(data = tri)+
#'     geom_sf(data = tri %rotate% list(c(0,1),90))
#'
#' #more complicated example
#' library(ggplot2)
#' library(sf666)
#' library(tmcn)
#' province=mapdata[mapdata$Layer==unique(mapdata$Layer)[1],]
#' ggplot()+
#'     geom_sf(data = province,fill='red')+
#'     geom_sf(data = rotate(province,30,c(80,20)),
#'             fill='red')+
#'     geom_sf(data = rotate(province,60,c(80,20)),
#'             fill='red')+
#'     geom_sf(data = rotate(province,90,c(80,20)),
#'             fill='red')+
#'     geom_sf(data = rotate(province,120,c(80,20)),
#'             fill='red')+
#'     geom_sf(data = rotate(province,150,c(80,20)),
#'             fill='red')+
#'     geom_sf(data = rotate(province,180,c(80,20)),
#'             fill='red')+
#'     geom_sf(data = rotate(province,210,c(80,20)),
#'             fill='red')+
#'     geom_sf(data = rotate(province,240,c(80,20)),
#'             fill='red')+
#'     geom_sf(data = rotate(province,270,c(80,20)),
#'             fill='red')+
#'     geom_sf(data = rotate(province,300,c(80,20)),
#'             fill='red')+
#'     geom_sf(data = rotate(province,330,c(80,20)),
#'             fill='red')+
#'     theme_void()+
#'     geom_sf_text(data = txt2,label='Great China',
#'                  size=13)
"%rotate%" <- function(sf,center_angel=list(center='center2',angel)){
    crs=st_crs(sf)
    angel=center_angel[[2]]
    center=center_angel[[1]]
    a=angel/180*pi
    a2=c(cos(a),-sin(a),
         sin(a),cos(a))
    mt=matrix(a2,byrow = T,nrow = 2)
    if (is.character(center)){
        if (center[1]=='center1'){
            center=sf_center(st_geometry(sf))
        }else if (center[1]=='center2'){
            center=sf_center(st_geometry(sf),2)
        }
    }
    if (any(c('sf','sfc','sfg') %in% class(center))){
        if (all(st_geometry_type(center)=='POINT')){
            center=st_coordinates(center)
        }else{
            if (chinese()) stop(tmcn::toUTF8('sf\u7C7B\u578B\u4E2D\u5FC3\u70B9\u7684geometry\u5FC5\u987B\u662FPOINT'))
            if (!chinese()) stop('center geometry must be point')
        }
    }
    st_geometry(sf)=(st_geometry(sf)-center)*mt+center
    st_crs(sf)=crs
    sf
}
