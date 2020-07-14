#' Get Date
#' @param without_punct default is FALSE. If true, returns date in YYYYmmdd format. Otherwise YYYY-mm-dd format.
#'@export

date_this <- function(without_punct = FALSE) {
        if (without_punct == FALSE) {
                return(as.character(Sys.Date()))
        } else {
                return(as.character(format(Sys.Date(), "%Y%m%d")))
        }
}
