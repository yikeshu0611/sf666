#' Create Curve
#'
#'
#' @inheritParams ggplot2::geom_segment
#' @param CurveData you can give data to this parameter instead of data parameter. Defaulted is NULL.
#'     You can use prepare_curve_data() function to generate this data.
#' @return Curve line
#' @importFrom sf st_as_sf st_point_on_surface
#' @export
#' @rdname stat_sf_666
#' @export
#' @examples
#' library(sf666)
#' p1=sf_point(c(1,2))
#' p2=sf_point(c(2,3))
#' df=cbind(p1,p2)
#' library(ggplot2)
#' ggplot()+
#'     stat_sf_curve(data=df)
#'
#'
#' df=cbind(p1,p2,atr=1)
#' library(ggplot2)
#' ggplot()+
#'     stat_sf_curve(data=df,aes(color=atr))
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
#'
#' # example for stat_sf_line
#' lined=sf_point(c(1,2),c(3,4))
#' ggplot(lined)+
#'     stat_sf_line()
#'
#' ggplot(data = prov)+
#'     geom_sf()+
#'     stat_sf_line()
#'
#'
#' # example for path
#'
#' prov$bigin=prov$Name[1]
#' prov$bigincoord=prov$geometry[1]
#' prov=prov[-1,]
#' data=prov
#' prepare_curve_data(prov)
#' ggplot(prov)+
#'     geom_sf()+
#'     stat_sf_curve(data = prov) #data must be give in stat_sf_curve
stat_sf_curve <- function (mapping = aes(), data = NULL, CurveData=NULL,stat = "identity", position = "identity",
                           ..., curvature = 0.5, angle = 90, ncp = 5, arrow = NULL,
                           arrow.fill = NULL, lineend = "butt", na.rm = FALSE, show.legend = NA,
                           inherit.aes = TRUE){
    if (is.null(CurveData)){
        CurveData=prepare_curve_data(data)
        CurveData=as.data.frame(CurveData)
    }
    mapping=modifyList(mapping,aes_string(x='xstart',y='ystart',xend='xend',yend='yend'))
    layer(data = CurveData, mapping = mapping, stat = stat, geom = GeomCurve,
          position = position, show.legend = show.legend, inherit.aes = inherit.aes,
          params = list(arrow = arrow, arrow.fill = arrow.fill,
                        curvature = curvature, angle = angle, ncp = ncp,
                        lineend = lineend, na.rm = na.rm, ...))
}
