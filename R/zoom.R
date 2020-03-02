#' Zoom
#'
#' @param sf an sf object
#' @param ratio ratio
#' @param center can be center1 or center2 or any other two-numeric values
#' @return a zoomed sf object
#' @export
#'
#' @examples
#' library(sf666)
#' library(ggplot2)
#' tri=triangle()
#' ggplot()+
#'     geom_sf(data = tri)+
#'     geom_sf(data = zoom(tri,0.5),fill='yellow')
zoom<-function(sf,ratio=0.5,center=c(0,0)){
    if (! 'sf' %in% class(sf)){
        if (chinese()) stop(tmcn::toUTF8('zoom\u7684\u5BF9\u8C61\u5FC5\u987B\u662Fsf'))
        if (!chinese()) stop('object of zoom must be sf')
    }
    if (length(ratio) != 1 |
        length(ratio) != nrow(sf)){
        if (chinese()) stop(tmcn::toUTF8('ratio\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59820.5;\u6216\u8005\u548Csf\u7684\u5217\u6570\u76F8\u540C'))
        if (!chinese()) stop('length of ratio must be 1 or equal to nrow of sf')
    }
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
    if (class(center)=='numeric'){
        if (length(center)==2){
            center=matrix(center,ncol = 2)
        }else{
            if (chinese()) stop(tmcn::toUTF8('center\u5FC5\u987B2\u4E2A\u6570\u636E,\u5982center=c(0,0)'))
            if (!chinese()) stop('center must be two number, eg: center=c(0,0)')
        }
    }
    if (nrow(center) != 1 |
        nrow(center) != nrow(sf)){
        if (chinese()) stop(tmcn::toUTF8('center\u5FC5\u987B\u662F2\u4E2A\u6570\u5B57,\u5982c(0,0);\u6216\u8005\u548Csf\u7684\u5217\u6570\u76F8\u540C'))
        if (!chinese()) stop('nrow of center must be two-numeric or equal to nrow of sf')
    }
    res=(st_geometry(sf)-center)*ratio+center
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname zoom
#' @param change a list of ration or center
#' @export
"%z%"<-function(sf,change=list(ratio=0.5,center=c(0,0))){
    ratio=change[[1]]
    center=change[[2]]
    if (length(ratio) != 1){
        if (chinese()) stop(tmcn::toUTF8('ratio\u5FC5\u987B\u662F1\u4E2A\u6570\u5B57,\u59820.5'))
        if (!chinese()) stop('length of ratio must be 1')
    }
    res=st_geometry(sf)*ratio
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}

#' @rdname zoom
#' @aliases zoom-towards-x-axis
#' @export
zx<-function(sf,ratio=1,value=0){
    center=c(value,mean(st_bbox(sf)[c('xmin','xmax')]))
    center=matrix(center,ncol = 2)
    res=(st_geometry(sf)-center)*ratio+center
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname zoom
#' @aliases zoom-towards-x-axis-infex
#' @export
"%zx%"<-function(sf,change=list(ratio=1,value=0)){
    if (!is.list(change)){
        if (chinese()) stop(tmcn::toUTF8('change\u5FC5\u987B\u662Flist,\u5982sf %zx% list(0.5,0)'))
        if (chinese()) stop('change must be list, eg: sf %zx% list(0.5,0)')
    }
    ratio=change[[1]]
    value=change[[2]]
    center=c(value,mean(st_bbox(sf)[c('xmin','xmax')]))
    center=matrix(center,ncol = 2)
    res=(st_geometry(sf)-center)*ratio+center
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname zoom
#' @aliases zoom-horizontal
#' @export
zh<-function(sf,ratio=1,value=0){
    center=c(value,mean(st_bbox(sf)[c('xmin','xmax')]))
    center=matrix(center,ncol = 2)
    res=(st_geometry(sf)-center)*ratio+center
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname zoom
#' @aliases zoom-horizontal-infex
#' @export
"%zh%"<-function(sf,change=list(ratio=1,value=0)){
    if (!is.list(change)){
        if (chinese()) stop(tmcn::toUTF8('change\u5FC5\u987B\u662Flist,\u5982sf %zx% list(0.5,0)'))
        if (chinese()) stop('change must be list, eg: sf %zx% list(0.5,0)')
    }
    ratio=change[[1]]
    value=change[[2]]
    center=c(value,mean(st_bbox(sf)[c('xmin','xmax')]))
    center=matrix(center,ncol = 2)
    res=(st_geometry(sf)-center)*ratio+center
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}

#' @rdname zoom
#' @aliases zoom-towards-y-axis
#' @export
zy<-function(sf,ratio=1,value=0){
    center=c(mean(st_bbox(sf)[c('ymin','ymax')]),value)
    center=matrix(center,ncol = 2)
    res=(st_geometry(sf)-center)*ratio+center
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname zoom
#' @aliases zoom-towards-y-axis
#' @export
"%zy%"<-function(sf,change=list(ratio=1,value=0)){
    ratio=change[[1]]
    value=change[[2]]
    center=c(mean(st_bbox(sf)[c('ymin','ymax')]),value)
    center=matrix(center,ncol = 2)
    res=(st_geometry(sf)-center)*ratio+center
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname zoom
#' @param value value
#' @aliases zoom-vertical
#' @export
zv<-function(sf,ratio=1,value=0){
    center=c(mean(st_bbox(sf)[c('ymin','ymax')]),value)
    center=matrix(center,ncol = 2)
    res=(st_geometry(sf)-center)*ratio+center
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname zoom
#' @aliases zoom-vertical
#' @export
"%zv%"<-function(sf,change=list(ratio=1,value=0)){
    ratio=change[[1]]
    value=change[[2]]
    center=c(mean(st_bbox(sf)[c('ymin','ymax')]),value)
    center=matrix(center,ncol = 2)
    res=(st_geometry(sf)-center)*ratio+center
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
