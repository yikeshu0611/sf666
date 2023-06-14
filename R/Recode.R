#' Recode categorical variable
#'
#' @param x dataframe or vector
#' @param ... code::value
#' @param cat logical
#' @param to.numeric logcial
#' @param order logical
#'
#' @return vector
#' @export
#'
Recode <- function(x,...,cat=TRUE,to.numeric=FALSE,order=F){
    replace <- c(...)
    # replace <- replace1
    if (is.data.frame(x)){
        xh <- substitute(x) |> deparse() |> paste0(collapse = '')
        for (i in 1:ncol(x)){
            if (i==1) cmd <- c()
            if (tolower(colnames(x)[i]) %in% c('seqn','year','sdmvpsu','sdmvstra')) next(i)
            if (is.numeric(x[,i])){
                next(i)
            }else{
                level <- levels(x[,i])
                if (is.null(level)) level <- do::unique_no.NA(as.character(x[,i]))
                if (order) level <- do::increase(level)
                if (anyNA(x[,i])) level <- c(level,'NA')
                replace <- level |>
                    paste0('::') |>
                    paste0(collapse = '", \n\t"') |>
                    sprintf(fmt = '"%s",\n\tto.numeric = FALSE)')
                cmdi <- paste0(xh,'$',colnames(x)[i],' <- Recode','(',xh,'$',colnames(x)[i],',\n\t',replace,'\n')
                if (is.null(cmd)){
                    cmd <- cmdi
                }else{
                    cmd <- paste0(cmd,'\n',cmdi)
                }
            }
        }
        xx <- rstudioapi::getActiveDocumentContext()
        for (i in (xx$selection[[1]]$range$start[[1]]):1) {

            f6 <- do::Trim_left(xx$contents[i])
            if (f6 == ''){
                next(i)
            }else if (do::left(f6,7) == 'Recode('){
                rstudioapi::setCursorPosition(list(c(i,1)),xx$id)
                rstudioapi::insertText('# ')

                rstudioapi::setCursorPosition(list(c(i+1,1)),xx$id)
                rstudioapi::insertText(text = cmd)
                break(i)
            }else if (do::left(f6,7) == 'nhanesR::Recode('){
                rstudioapi::setCursorPosition(list(c(i,1)),xx$id)
                rstudioapi::insertText('# ')

                rstudioapi::setCursorPosition(list(c(i+1,1)),xx$id)
                rstudioapi::insertText(text = cmd)
                break(i)
            }
        }
        invisible(cmd)
    }else if (is.character(x) | is.factor(x)){
        if (is.null(replace)){
            xh <- substitute(x) |> deparse() |> paste0(collapse = '')
            level <- levels(x)
            if (is.null(level)) level <- do::unique_no.NA(as.character(x))
            if (order) level <- do::increase(level)
            if (anyNA(x)) level <- c(level,'NA')
            replace <- level |>
                paste0('::') |>
                paste0(collapse = '", \n\t"') |>
                sprintf(fmt = '"%s",\n\tto.numeric = FALSE)')
            cmd <- paste0(xh,' <- Recode','(',xh,',\n\t',replace,'\n')

            xx <- rstudioapi::getActiveDocumentContext()
            for (i in (xx$selection[[1]]$range$start[[1]]):1) {

                f6 <- do::Trim_left(xx$contents[i])
                if (f6 == ''){
                    next(i)
                }else if (do::left(f6,7) == 'Recode('){
                    rstudioapi::setCursorPosition(list(c(i,1)),xx$id)
                    rstudioapi::insertText('# ')

                    rstudioapi::setCursorPosition(list(c(i+1,1)),xx$id)
                    rstudioapi::insertText(text = cmd)
                    break(i)
                }else if (do::left(f6,7) == 'nhanesR::Recode('){
                    rstudioapi::setCursorPosition(list(c(i,1)),xx$id)
                    rstudioapi::insertText('# ')

                    rstudioapi::setCursorPosition(list(c(i+1,1)),xx$id)
                    rstudioapi::insertText(text = cmd)
                    break(i)
                }
            }
            invisible(cmd)
        }else{
            (factorck <- is.factor(x))
            if (factorck){
                (from <- do::Replace0(replace,' {0,}:: {0,}.*'))
                (ck <- lapply(from, function(i){
                    x == i & !is.na(x)
                }))
                for (i in 1:length(ck)) {
                    (to <- do::Replace0(replace[i],'.*:: {0,}'))
                    if (to=='') next(i)
                    if (to == 'NA') to <- NA
                    levels(x)[levels(x)==from[i]] <- to
                }
                if (any(from =='NA')){
                    to <- do::Replace0(replace[from =='NA'],'.*:: {0,}')
                    if (to != ''){
                        levels(x) <- unique(c(levels(x),to))
                        x[is.na(x)] <- to
                    }
                }
            }else{
                x <- as.character(x)
                (from <- do::Replace0(replace,' {0,}:: {0,}.*'))
                (ck <- lapply(from, function(i){
                    if(i=='NA'){
                        is.na(x)
                    }else{
                        x == i & !is.na(x)
                    }
                }))
                for (i in 1:length(ck)) {
                    (to <- do::Replace0(replace[i],'.*:: {0,}'))
                    if (to=='') next(i)
                    if (to == 'NA') to <- NA
                    x[ck[[i]]] <- to
                }
            }
            if (to.numeric){
                to_numeric(x)
            }else{
                return(x)
            }

        }
    }
}
