#' Prepare Data for sf Curve
#'
#' @param data data with two simple feature columns
#' @name prepare
#' @rdname prepare
#' @return data with xstart, ystart, xend and yend
#' @export
#'
prepare_curve_data <- function(data){
    sfc_check=sapply(1:ncol(data),function(i) is.sfc(data[[i]]))
    if (sum(sfc_check) != 2){
        if (chinese()) stop(tmcn::toUTF8('\u8F93\u5165\u7684\u6570\u636E\u5FC5\u987B\u53EA\u67092\u5217sfc\u5BF9\u8C61'))
        if (!chinese()) stop('Only two sfc columns can be included in data')
    }
    sfc_col=(1:ncol(data))[sfc_check]
    sfc1=st_centroid(st_point_on_surface(data[[sfc_col[1]]]))
    sfc2=st_centroid(st_point_on_surface(data[[sfc_col[2]]]))
    coord1=st_coordinates(sfc1)
    coord2=st_coordinates(sfc2)
    coord_dup=rowSums(coord1 == coord2)>1
    if (any(coord_dup)){
        row_dup=(1:nrow(data))[coord_dup]
        row_dup=paste(row_dup,collapse = ', ')
        if (chinese()) stop(tmcn::toUTF8('\u4EE5\u4E0B\u884C\u7684\u8D77\u70B9\u548C\u91CD\u70B9\u76F8\u540C: '),row_dup)
        if (!chinese()) stop('The following rows contain the same origion and destination: ',row_dup)
    }
    data$xstart=coord1[,1]
    data$ystart=coord1[,2]
    data$xend=coord2[,1]
    data$yend=coord2[,2]
    data
}


#' @rdname prepare
#' @export
prepare_point_data <- function(data){
    data <- suppressWarnings(st_centroid(st_point_on_surface(data)))
    data$x <- st_coordinates(data)[,1] #x
    data$y <- st_coordinates(data)[,2] #y
    as.data.frame(data)
}
#' @rdname prepare
#' @export
prepare_line_data <- function(data){
    data <- suppressWarnings(st_centroid(st_point_on_surface(data)))
    data$x <- st_coordinates(data)[,1] #x
    data$y <- st_coordinates(data)[,2] #y
    as.data.frame(data)
}
