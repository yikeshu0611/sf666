#' @title Translate
#' @param sf an sf object
#' @param step numeric. In tx(), %tx%, ty() and %ty%, step must be one number.
#'     In txy() and %txy%, step must be two numbers. The first is x and the other
#'     is y. In tal() and %tal%, step must be two numbers too, the first is angel,
#'     ranging from -180 to 180, the other is the length that you want to translate.
#' @name translate
#' @rdname translate
#' @aliases translate-along-x-axis
#' @export
#'
tx <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(step,0),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-along-x-axis-infix
"%tx%" <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(step,0),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname translate
#' @aliases translate-horizontal
#' @export
#'
th <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(step,0),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-horizontal-infix
"%th%" <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(step,0),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-along-y-axis
ty <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(0,step),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-along-y-axis-infix
"%ty%" <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(0,step),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-vertical
tv <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(0,step),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-vertical-infix
"%tv%" <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(0,step),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-up
tu <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(0,step),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-up-infix
"%tu%" <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(0,step),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @name translate
#' @export
#' @aliases translate-right
tr <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(step,0),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-right-infix
"%tr%" <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(step,0),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @name translate
#' @export
#' @aliases translate-left
tl <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(-step,0),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-left-infix
"%tl%" <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(-step,0),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-down
td <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(0,-step),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-down-infix
"%td%" <- function(sf,step){
    if (length(step) != 1){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59821'))
        if (!chinese()) stop('length of step must be 1')
    }
    res=st_geometry(sf) + matrix(c(0,-step),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-along-xy-axis
txy <- function(sf,step){
    if (any(class(step) != 'numeric',
            length(step) !=2)){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F2\u4E2A\u6570\u5B57,\u5982c(1,2)'))
        if (!chinese()) stop('step must be two numbers, ig: c(1,2)')
    }
    res=st_geometry(sf) + matrix(c(step[1],step[2]),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-along-xy-axis-infix
"%txy%" <- function(sf,step){
    if (any(class(step) != 'numeric',
            length(step) !=2)){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F2\u4E2A\u6570\u5B57,\u5982c(1,2)'))
        if (!chinese()) stop('step must be two numbers, ig: c(1,2)')
    }
    res=st_geometry(sf) + matrix(c(step[1],step[2]),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}

#' @export
#' @rdname translate
#' @aliases translate-by-angle-and-length
tal <- function(sf,step){
    if (any(class(step) != 'numeric',
            length(step) !=2)){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F2\u4E2A\u6570\u5B57,\u5982c(60,2)'))
        if (!chinese()) stop('step must be two numbers, ig: c(60,2)')
    }
    angle=step[1]/180*pi
    step=c(step[2]*cos(angle),step[2]*sin(angle))
    res=st_geometry(sf) + matrix(c(step[1],step[2]),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @export
#' @rdname translate
#' @aliases translate-by-angle-and-length-infix
"%tal%" <- function(sf,step){
    if (any(class(step) != 'numeric',
            length(step) !=2)){
        if (chinese()) stop(tmcn::toUTF8('step\u5FC5\u987B\u662F2\u4E2A\u6570\u5B57,\u5982c(60,2)'))
        if (!chinese()) stop('step must be two numbers, ig: c(60,2)')
    }
    angle=step[1]/180*pi
    step=c(step[2]*cos(angle),step[2]*sin(angle))
    res=st_geometry(sf) + matrix(c(step[1],step[2]),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname translate
#' @examples
#' library(sf666)
#' library(ggplot2)
#' #generate a triangle
#' tri=triangle()
#'
#' #base map
#' ggplot()+
#'     geom_sf(data = tri,fill='red')
#'
#' # translate along x-axis
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = tx(tri,2))
#'
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = tri %tx% 2)
#'
#'
#' # translate along y-axis
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = ty(tri,2))
#'
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = tri %ty% 2)
#'
#' # translate along x-axis and y-axis
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = txy(tri,c(1,2)))
#'
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = tri %txy% c(1,2))
#'
#'
#'
#' # translate by angle and length
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = tal(tri,c(45,2)))
#'
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = tri %tal% c(60,2))
#' \donttest{
#' # more complicated examples
#' library(sf)
#' library(ggnewscale)
#' nc <- st_read(system.file("shape/nc.shp", package="sf"))
#'
#' # origin map
#' ggplot()+
#'     geom_sf(data = nc)
#' # translate 10, along x
#' ggplot()+
#'     geom_sf(data = nc)+
#'     geom_sf(data = txy(nc,c(10,0)))
#'
#'
#' # translate 4, along y
#' ggplot()+
#'     geom_sf(data = nc)+
#'     geom_sf(data = txy(nc,c(0,4)))
#'
#'
#' # translate 10 along x and 4, along y
#' ggplot()+
#'     geom_sf(data = nc)+
#'     geom_sf(data = txy(nc,c(10,4)))
#'
#' # combine them
#' ggplot()+
#'     geom_sf(data = nc)+
#'     geom_sf(data = txy(nc,c(10,0)))+
#'     geom_sf(data = txy(nc,c(0,4)))+
#'     geom_sf(data = txy(nc,c(10,4)))
#'
#' # translate nc 5 towards angle of 30 degrees
#'
#' ggplot()+
#'     geom_sf(data = nc)+
#'     geom_sf(data = tal(nc,c(30,5)))
#'
#' # by ggnewscale, you can add diffrent scales
#' ggplot()+
#'     #origion
#'     geom_sf(data = nc,aes(fill=AREA))+
#'     #right
#'     new_scale_fill()+
#'     geom_sf(data = txy(nc,c(10,0)),aes(fill=PERIMETER))+
#'     scale_fill_gradientn(colors=c('black','red'))+
#'     #down
#'     new_scale_fill()+
#'     geom_sf(data = ty(nc,-4),aes(fill=CRESS_ID))+
#'     scale_fill_gradientn(colors=c('black','green'))+
#'     #right and down
#'     new_scale_fill()+
#'     geom_sf(data = txy(nc,c(10,-4)),aes(fill=SID74))+
#'     scale_fill_gradientn(colors=c('black','yellow'))+
#'
#'     theme_void()
#' }
translateexample <- function(){
    'this is for example'
}
