#' @rdname stat_sf_666
#' @export
#' @export
#'
#' @examples
#' provclass=unique(mapdata$Layer)[1]
#' prov=mapdata[mapdata$Layer==provclass,]
#' ggplot(data = prov)+
#'     geom_sf()+
#'     stat_sf_point()
#'
#' # use prepare function to generate a new data for point
#' # which is a data frame
#' # so you can adjust the point coord
#' # then use prepare_sf_point() function to add point
#' prepare_data=prepare_point_data(prov)
#' prepare_data$x=prepare_data$x+50
#' ggplot(data = prov)+
#'     geom_sf()+
#'     prepare_sf_point(data=prepare_data)
stat_sf_point <- function(mapping = aes(), data=NULL,
                                position = "identity", na.rm = FALSE,
                                show.legend = NA, inherit.aes = TRUE,
                                ...) {
    layer_sf(
        stat = StatSfPoints,
        data = data,
        mapping = mapping,
        geom = GeomPoint,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            ...
        )
    )
}
#' @importFrom ggplot2 ggproto aes Stat
#' @rdname stat_sf_666
#' @export
StatSfPoints<-ggproto(
        "StatSfPoints", Stat,
        default_aes = aes(x = after_stat(x), y = after_stat(y)),
        required_aes = c("geometry"),
        compute_group = function(data, scales) {
            data=st_as_sf(data)
            data <- suppressWarnings(st_centroid(st_point_on_surface(data)))
            data$x <- st_coordinates(data)[,1] #x
            data$y <- st_coordinates(data)[,2] #y
            data=as.data.frame(data)
            data
        }
)

#' @rdname stat_sf_666
#' @export
prepare_sf_point <- function(data=NULL,mapping = aes(),
                                  position = "identity", na.rm = FALSE,
                                  show.legend = NA, inherit.aes = TRUE,
                                  ...) {
    mapping=modifyList(mapping,aes_string(x='x',y='y'))
    layer_sf(
        stat = "identity",
        data = data,
        mapping = mapping,
        geom = GeomPoint,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            ...
        )
    )
}
