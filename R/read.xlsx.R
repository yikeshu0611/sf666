#' Read xlsx data
#' @description Read xlsx data and return dataframe by package 'readxl'
#' @param path see \code{\link[readxl]{read_excel}}
#' @param sheet see \code{\link[readxl]{read_excel}}
#' @param range see \code{\link[readxl]{read_excel}}
#' @param col_names see \code{\link[readxl]{read_excel}}
#' @param col_types see \code{\link[readxl]{read_excel}}
#' @param na see \code{\link[readxl]{read_excel}}
#' @param trim_ws see \code{\link[readxl]{read_excel}}
#' @param skip see \code{\link[readxl]{read_excel}}
#' @param n_max see \code{\link[readxl]{read_excel}}
#' @param guess_max see \code{\link[readxl]{read_excel}}
#' @param progress see \code{\link[readxl]{read_excel}}
#' @param .name_repair see \code{\link[readxl]{read_excel}}
#' @importFrom readxl readxl_progress
#' @return a dataframe, for more to see \code{\link[readxl]{read_excel}}
#' @export
read.xlsx <- function (path, sheet = NULL, range = NULL, col_names = TRUE,
          col_types = NULL, na = "", trim_ws = TRUE, skip = 0, n_max = Inf,
          guess_max = min(1000, n_max), progress = readxl_progress(),
          .name_repair = "unique"){
    res=readxl::read_xlsx(path, sheet, range, col_names,
                      col_types, na, trim_ws, skip, n_max,
                      guess_max, progress,
                      .name_repair =)
    data.frame(res,stringsAsFactors = FALSE,check.names = TRUE)
}
