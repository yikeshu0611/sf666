#' @title Mirror Reflection Transformation
#' @param sf an sf object
#' @param k slope
#' @param b intercept
#'
#' @return an mirrored sf object
#' @details Mirror refletion transformation is one of the most important
#' transformations. This function is only used for simple feature with two
#' dimensions. Argument k is the slope of the straight line. Argument b is
#' the intercept. So, we build the formula of the line constructed by k and b,
#' as follows
#' \deqn{y = k*x + b}
#' which can be transformed into
#' \deqn{k*x + (-1)*y + b = 0}
#' Then, we use the formal equation
#' \deqn{a*x + b*y + c = 0}
#' to calculate the coordinates of the mirrored point B(x_{1},y_{1}) from
#' original point A(x_{0}, y_{0})
#' \deqn{x_{1} =\frac{(b^{2} - a^{2})*x_{0} - 2*a*b*y_{0} - 2*a*c}{a^{2} + b^{2}}}
#' \deqn{y_{1} =\frac{(a^{2} - b^{2})*y_{0} - 2*a*b*x_{0} - 2*b*c}{a^{2} + b^{2}}}
#' @export
#'
mirror<-function(sf,k,b){
    #model is:            ax + by + c =0
    #in this function is: y = kx + b
    #that is:             kx + (-1)y + b =0
    #so
    a=k
    c=b # be careful give value of b to c first
    b=-1
    #mirror:
    #          [b^2-a^2,   -2*a*b ]
    # [x0,y0] *|                  |
    #          [-2*a*b,    a^2-b^2]
    mt=st_geometry(sf) * matrix(c(b^2-a^2,  -2*a*b,
                                  -2*a*b,   a^2-b^2),
                                ncol = 2,byrow = T)
    dif=mt-matrix(c(2*a*c,2*b*c),nrow = 1)
    res=dif/(a^2+b^2)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @param kb two numbers for k and b, default is c(1,0)
#' @rdname mirror
#' @export
"%mir%"<-function(sf,kb=c(k=1,b=0)){
    if (length(kb) != 2) stop('kb must have 2 numbers')
    a=kb[1]
    c=kb[2]
    b=-1
    mt=st_geometry(sf) * matrix(c(b^2-a^2,  -2*a*b,
                                  -2*a*b,   a^2-b^2),
                                ncol = 2,byrow = T)
    dif=mt-matrix(c(2*a*c,2*b*c),nrow = 1)
    res=dif/(a^2+b^2)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname mirror
#' @export
mx <-function(sf){
    res=st_geometry(sf)*matrix(c(1,0,
                                 0,-1),
                               nrow=2,
                               byrow=TRUE)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}

#' @param anyvalue will be ignored
#' @rdname mirror
#' @export
"%mx%"<-function(sf,anyvalue){
    res=st_geometry(sf)*matrix(c(1,0,
                                 0,-1),
                               nrow=2,
                               byrow=TRUE)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}

#' @rdname mirror
#' @export
my<-function(sf){
    res=st_geometry(sf)*matrix(c(-1,0,
                                 0,1),
                               nrow=2,
                               byrow=TRUE)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname mirror
#' @export
"%my%"<-function(sf,anyvalue){
    res=st_geometry(sf)*matrix(c(-1,0,
                                 0,1),
                               nrow=2,
                               byrow=TRUE)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname mirror
#' @export
mxy<-function(sf){
    res=st_geometry(sf)*matrix(c(-1,0,
                                 0,-1),
                               nrow=2,
                               byrow=TRUE)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname mirror
#' @export
"%mxy%"<-function(sf,anyvalue){
    res=st_geometry(sf)*matrix(c(-1,0,
                                 0,-1),
                               nrow=2,
                               byrow=TRUE)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @param h horizontal value for mirror, ig. y value
#' @rdname mirror
#' @export
mh<-function(sf,h){
    res=st_geometry(sf)*matrix(c(1,0,
                                 0,-1),
                               nrow=2,
                               byrow=TRUE)
    res=res+matrix(data = c(0,2*h),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname mirror
#' @export
"%mh%"<-function(sf,h){
    res=st_geometry(sf)*matrix(c(1,0,
                                 0,-1),
                               nrow=2,
                               byrow=TRUE)
    res=res+matrix(data = c(0,2*h),nrow = 1)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @param v vertical value, ig. x value
#' @rdname mirror
#' @export
mv<-function(sf,v){
    res=st_geometry(sf)*matrix(c(-1,0,
                                 0,1),
                               ncol=2,
                               byrow=TRUE)
    res=res+matrix(c(2*v,0),ncol=2)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
#' @rdname mirror
#' @export
#' @examples
#' library(sf666)
#' library(ggplot2)
#' #generate a triangle
#' tri=triangle()
#'
#' # use basic mirror
#'
#' ggplot()+
#'     geom_sf(data = tri,fill='red')
#'
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = mirror(tri,k=1,b=0),fill='blue')
#'
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = mirror(tri,k=2,b=1),fill='blue')
#'
#' # infix %mir%
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = tri %mir% c(1,0),fill='blue')
#'
#' #mirrored by x-axis
#' #prefix mx()
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = mx(tri),fill='blue')
#'
#' #infix %mx%
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = tri %mx% 1,fill='blue')
#'
#' #mirrored by y-axis
#' #prefix my()
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = my(tri),fill='blue')
#'
#' #infix %my%
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = tri %my% 1,fill='blue')
#'
#' #mirrored by xy-axis, means origianl point O.
#' #prefix mxy()
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = mxy(tri),fill='blue')
#'
#' #infix %mxy%
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = tri %mxy% 1,fill='blue')
#'
#' #mirrored by x-axis
#' #prefix mh()
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = mh(tri,1),fill='blue')
#'
#' #infix %mh%
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = tri %mh% 1,fill='blue')
#'
#' #mirrored by x-axis
#' #prefix mv()
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = mv(tri,1),fill='blue')
#'
#' #infix %mv%
#' ggplot()+
#'     geom_sf(data = tri,fill='red')+
#'     geom_sf(data = tri %mv% 1,fill='blue')
"%mv%"<-function(sf,v){
    res=st_geometry(sf)*matrix(c(-1,0,
                                 0,1),
                               ncol=2,
                               byrow=TRUE)
    res=res+matrix(c(2*v,0),ncol=2)
    st_crs(res)=st_crs(sf)
    if (any('sf' %in% class(sf))){
        st_geometry(sf)=res
        return(sf)
    }else{
        return(res)
    }
}
