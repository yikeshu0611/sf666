#' read gdw file
#' gdw is file of getdata graph digitizer
#' @param path path of gdw file
#' @importFrom rvest read_html html_elements html_attr html_text
#'
#' @return sf object
#' @export
#'
read.gdw <- function(path){
    html <- read_html(path)

    # mf, deltaX, deltaY
    xmin <- html |>
        html_elements(xpath = '//xmin')
    (mf <- html_attr(xmin,'mf') |>
            as.numeric())
    (deltaX <- html_text(xmin) |>
            do::Replace0(',.*') |>
            as.numeric())
    (deltaY <- html_text(xmin) |>
            sub(pattern = ',',replacement = ';') |>
            do::Replace0(',.*','.*;') |>
            as.numeric())



    xmin <- html |>
        html_elements(xpath = '//xmin') |>
        html_text() |>
        do::Replace0('.*,') |>
        as.numeric()
    xmax <- html |>
        html_elements(xpath = '//xmax') |>
        html_text() |>
        do::Replace0('.*,') |>
        as.numeric()
    ymin <- html |>
        html_elements(xpath = '//ymin') |>
        html_text() |>
        do::Replace0('.*,') |>
        as.numeric()
    ymax <- html |>
        html_elements(xpath = '//ymax') |>
        html_text() |>
        do::Replace0('.*,') |>
        as.numeric()
    lines <- html |>
        html_elements(xpath = '//line')
    cd <- lapply(1:length(lines), function(i){
        cd <- lines[[i]] |>
            html_elements(xpath = 'points') |>
            html_text() |>
            paste0(collapse = ',') |>
            strsplit(',') |>
            do::list1() |>
            as.numeric() |>
            matrix(ncol = 2,byrow = T) |>
            do::give_names(c('x','y'))
        cd[,'x'] <- abs((cd[,'x']-deltaX))/mf
        cd[,'y'] <- abs((deltaY-cd[,'y']))/mf

        cf <- suppressWarnings(sf_polygon(cd))

        caption <- lines[[i]] |> html_elements(xpath = 'caption') |> html_text()
        cbind(caption,cf)
    })
    cf <- do.call(what = rbind,cd)
    attr(cf[[attr(cf, "sf_column")]],'bbox') <- c(xmin=xmin,ymin=ymin,xmax=xmax,ymax=ymax)
    cf
}

