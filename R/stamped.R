#' Get Timestamp
#'@param without_punct defaults to FALSE. If true removes all punctuation so output is YYYYmmddHHMMSS format.
#'@export

stamp_this <- function(without_punct = FALSE) {
        if (without_punct == FALSE) {
                return(as.character(Sys.time()))
        } else {
                return(format(Sys.time(), "%Y%m%d%H%M%S"))
        }
}
