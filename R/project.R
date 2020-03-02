#' Projection
#'
#' @param sf as sf object
#' @param k projected line slop
#' @param b projected line intercept
#' @param angle projected direction angle, one numeric or 'orth', default or 'x' or 'y'
#' @name project
#' @importFrom sf st_bbox st_cast
#' @rdname project
#' @return an MULTIPOINT sf
#' @export
#'
#' @examples
#' library(sf666)
#' library(ggplot2)
#' tri=triangle()
#' ggplot()+
#'     geom_sf(data=tri)+
#'     geom_sf(data=project(tri,k=1,b=3))
#' ggplot()+
#'     geom_sf(data=tri)+
#'     geom_sf(data=project(tri,k=1,b=3,angle=30))
project <- function(sf,k,b,angle='orth'){
    if (k==0){
        pr1=st_geometry(sf)*matrix(c(1,0,0,0),ncol = 2)+matrix(c(0,b),ncol = 2)
        pr2=st_cast(pr1,'MULTIPOINT')
        st_crs(pr2)=st_crs(sf)
        pr2
    }else if (k=='x'){
        pr1=st_geometry(sf)*matrix(c(1,0,0,0),ncol = 2)+matrix(c(0,b),ncol = 2)
        pr2=st_cast(pr1,'MULTIPOINT')
        st_crs(pr2)=st_crs(sf)
        pr2
    }else if (k=='y'){
        pr1=st_geometry(sf)*matrix(c(0,0,0,1),ncol = 2)+matrix(c(b,0),ncol = 2)
        pr2=st_cast(pr1,'MULTIPOINT')
        st_crs(pr2)=st_crs(sf)
        pr2
    }else{
        a1=k
        c1=b
        b1=-1
        if (angle=='orth'){
            k2=-1/k
        }else{
            k2=tan(angle/180*pi)
        }
        pr1=st_geometry(sf)*matrix(c(-k2,-k2,1,1),ncol=2,byrow = T)-b
        pr2=pr1*matrix(c(1,k),ncol=2)/(k-k2)+c(0,b)
        pr3=st_cast(pr2,'MULTIPOINT')
        st_crs(pr3)=st_crs(sf)
        pr3
    }
}
#' @param value must be a list with length 2
#' @export
#' @rdname project
#' @examples
#' #project to x-axis
#' ggplot()+
#'     geom_sf(data=tri)+
#'     geom_sf(data=px(tri,1))
px <- function(sf,value=0){
    pr1=st_geometry(sf)*matrix(c(1,0,0,0),ncol = 2)+matrix(c(0,value),ncol = 2)
    pr2=st_cast(pr1,'MULTIPOINT')
    st_crs(pr2)=st_crs(sf)
    pr2
}
#' @export
#' @rdname project
#' @examples
#' #project to x-axis
#' ggplot()+
#'     geom_sf(data=tri)+
#'     geom_sf(data=tri %px% 1)
"%px%" <- function(sf,value=0){
    pr1=st_geometry(sf)*matrix(c(1,0,0,0),ncol = 2)+matrix(c(0,value),ncol = 2)
    pr2=st_cast(pr1,'MULTIPOINT')
    st_crs(pr2)=st_crs(sf)
    pr2
}
#' @export
#' @rdname project
#' @examples
#' #project horizontally
#' ggplot()+
#'     geom_sf(data=tri)+
#'     geom_sf(data=ph(tri,1))
ph <- function(sf,value){
    pr1=st_geometry(sf)*matrix(c(1,0,0,0),ncol = 2)+matrix(c(0,value),ncol = 2)
    pr2=st_cast(pr1,'MULTIPOINT')
    st_crs(pr2)=st_crs(sf)
    pr2
}
#' @export
#' @rdname project
#' @examples
#' #project horizontally
#' ggplot()+
#'     geom_sf(data=tri)+
#'     geom_sf(data=tri %ph% 1)
"%ph%" <- function(sf,value){
    pr1=st_geometry(sf)*matrix(c(1,0,0,0),ncol = 2)+matrix(c(0,value),ncol = 2)
    pr2=st_cast(pr1,'MULTIPOINT')
    st_crs(pr2)=st_crs(sf)
    pr2
}
#' @export
#' @rdname project
#' @examples
#' #project to y-axis
#' ggplot()+
#'     geom_sf(data=tri)+
#'     geom_sf(data=py(tri,1))
py <- function(sf,value){
    pr1=st_geometry(sf)*matrix(c(0,0,0,1),ncol = 2)+matrix(c(value,0),ncol = 2)
    pr2=st_cast(pr1,'MULTIPOINT')
    st_crs(pr2)=st_crs(sf)
    pr2
}
#' @export
#' @rdname project
#' @examples
#' #project to y-axis
#' ggplot()+
#'     geom_sf(data=tri)+
#'     geom_sf(data=tri %py% 1)
"%py%" <- function(sf,value){
    pr1=st_geometry(sf)*matrix(c(0,0,0,1),ncol = 2)+matrix(c(value,0),ncol = 2)
    pr2=st_cast(pr1,'MULTIPOINT')
    st_crs(pr2)=st_crs(sf)
    pr2
}
#' @export
#' @rdname project
#' @examples
#' #project vertically
#' ggplot()+
#'     geom_sf(data=tri)+
#'     geom_sf(data=pv(tri,1))
pv <- function(sf,value){
    pr1=st_geometry(sf)*matrix(c(0,0,0,1),ncol = 2)+matrix(c(value,0),ncol = 2)
    pr2=st_cast(pr1,'MULTIPOINT')
    st_crs(pr2)=st_crs(sf)
    pr2
}
#' @export
#' @rdname project
#' @examples
#' #project vertically
#' ggplot()+
#'     geom_sf(data=tri)+
#'     geom_sf(data=tri %pv% 1)
"%pv%" <- function(sf,value=0){
    pr1=st_geometry(sf)*matrix(c(0,0,0,1),ncol = 2)+matrix(c(value,0),ncol = 2)
    pr2=st_cast(pr1,'MULTIPOINT')
    st_crs(pr2)=st_crs(sf)
    pr2
}
