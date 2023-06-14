#' factor
#'
#' @param x character
#'
#' @return factor
#' @export
#'
Factor <- function(x){
    dropna <- function(x) x[!is.na(x)]
    v <- do::Replace0(paste0(deparse(substitute(x)),collapse = ''),' ')
    if (is.data.frame(x)){
        cmd <- ''
        for (i in colnames(x)) {
            if (is.character(x[,i]) | is.factor(x[,i])){
                string <- unique(x[,i]) |> dropna() |> paste0(collapse = "', '") |> sprintf(fmt = "c('%s')")
                append(cmd) <- sprintf('%s$%s <- factor(%s$%s, %s)',v,i,v,i,string)
            }
        }
        cmd <- do::rm_nchar(cmd,0)
        context <- rstudioapi::getActiveDocumentContext()
        start_line <- context$selection[[1]]$range$start[[1]]
        rstudioapi::insertText(text = cmd)
        rstudioapi::setCursorPosition(position =  c( start_line-1, 1), id = NULL )
        rstudioapi::insertText(text = '# ')
        invisible()
    }else{
        lv <- levels(x)
        if (is.null(lv)) lv <- do::unique_no.NA(x)
        string1 <- sprintf('%s <- factor(%s, levels = c(%s))',v,v,paste0(sprintf('"%s"',lv),collapse = ','))
        string <- paste0(string1,'\n')
        context <- rstudioapi::getActiveDocumentContext()
        start_line <- context$selection[[1]]$range$start[[1]]
        start_char <- context$selection[[1]]$range$end[[2]]
        rstudioapi::insertText(text = string)
        rstudioapi::setCursorPosition(position =  c( start_line-1, 1), id = NULL )
        rstudioapi::insertText(text = '# ')
        invisible()
    }
}
