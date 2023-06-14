#k=sf_point(c(1,2),c(4,5),c(2.5,4))
#k
#k2=cbind(k,x=c('tt','k','m'))
#k2$x2=c(1,2,3)
#k2$x3=as.character(c('a','b','c'))
#library(ggplot2)
#
#ggplot(data = k2)+
#    geom_sf()+
#    stat_sf_text(
#        aes(color=x2),
#
#        label=k2$x3)

#' @rdname stat_sf_666
#' @export
stat_sf_text <- function(mapping = aes(), data=NULL,
                         label=NULL,coord_xy=NULL,coord_row=NULL,
                         position = "identity", na.rm = FALSE,
                         show.legend = NA, inherit.aes = TRUE,
                         ...) {
    stat_label=label
    #mapping = modifyList(mapping)
    layer_sf(
        stat = StatSfText,
        data = data,
        mapping = mapping,
        geom = GeomText,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(coord_xy=coord_xy,coord_row=coord_row,
                      stat_label=stat_label,label=label,
                      na.rm = na.rm, ...
        )
    )
}
#' @importFrom ggplot2 GeomLine GeomPoint
#' @rdname stat_sf_666
#' @export
StatSfText<-ggproto(
    "StatSfText", Stat,
    default_aes = aes(x = after_stat(x), y = after_stat(y)),
    required_aes = c("geometry"),
    compute_group = function(data, scales,
                             stat_label=stat_label,
                             coord_xy=coord_xy,
                             coord_row=coord_row) {
        data=st_as_sf(data)
        data <- suppressWarnings(st_centroid(st_point_on_surface(data)))
        data$x <- st_coordinates(data)[,1] #x
        data$y <- st_coordinates(data)[,2] #y
        data=as.data.frame(data)
        if (!is.null(coord_xy)){
            pose_xy=strsplit(coord_xy,'->')
            for (i in 1:length(pose_xy)) {
                res.i=pose_xy[[i]]
                xy.un=unlist(strsplit(res.i[2],','))
                xy.un2=do::Replace0(xy.un,c('c','\\(','\\)'))
                xy=as.numeric(as.character(xy.un2))
                data[stat_label==res.i[1],c('x','y')]=xy
            }
        }else if (!is.null(coord_row)){
            pose_xy=strsplit(coord_row,'->')
            for (i in 1:length(pose_xy)) {
                res.i=pose_xy[[i]]
                xy.un=unlist(strsplit(res.i[2],','))
                xy.un2=do::Replace0(xy.un,c('c','\\(','\\)'))
                xy=as.numeric(as.character(xy.un2))
                data[as.numeric(as.character(res.i[1])),c('x','y')]=xy
            }
        }
        return(data)
    }
)
