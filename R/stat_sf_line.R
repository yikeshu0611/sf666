#' @rdname stat_sf_666
#' @export
stat_sf_line <- function(mapping = aes(), data=NULL,
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE,
                          ...) {
    layer_sf(
        stat = StatSfLine,
        data = data,
        mapping = mapping,
        geom = GeomLine,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            ...
        )
    )
}
#' @importFrom ggplot2 GeomLine GeomPoint
#' @rdname stat_sf_666
#' @export
StatSfLine<-ggproto(
    "StatSfLine", Stat,
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
prepare_sf_line <- function(data=NULL,mapping = aes(),
                             position = "identity", na.rm = FALSE,
                             show.legend = NA, inherit.aes = TRUE,
                             ...) {
    mapping=modifyList(mapping,aes_string(x='x',y='y'))
    layer_sf(
        stat = "identity",
        data = data,
        mapping = mapping,
        geom = GeomLine,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            na.rm = na.rm,
            ...
        )
    )
}
