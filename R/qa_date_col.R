#' Returns dataframe of the outputs for all the parsing options for a date vector
#' @importFrom lubridate ymd
#' @importFrom lubridate mdy
#' @import dplyr
#' @export

qa_date_col <-
        function(dataframe, date_col) {
                ##Prep
                date_col <- enquo(date_col)

                ##Converting to character to check formating and parsing using lubridate functions
                dataframe_01 <-
                        dataframe %>%
                        dplyr::select(!!date_col) %>%
                        dplyr::mutate(AS_CHAR = as.character(!!date_col)) %>%
                        dplyr::mutate(AS_NUM = as.numeric(AS_CHAR)) %>%
                        dplyr::mutate(YMD = lubridate::ymd(AS_CHAR, quiet = TRUE)) %>%
                        dplyr::mutate(MDY = lubridate::mdy(AS_CHAR, quiet = TRUE)) %>%
                                        dplyr::mutate(R_ORIGIN = as.Date(AS_NUM, origin = "1970-01-01")) %>%
                                        dplyr::mutate(R_ORIGIN_VALID = R_ORIGIN < Sys.Date()) %>%
                                        dplyr::mutate(EXCEL_ORIGIN = as.Date(AS_NUM, origin = "1900-01-01")) %>%
                                        dplyr::mutate(EXCEL_ORIGIN_VALID = EXCEL_ORIGIN < Sys.Date())

                return(dataframe_01)
        }

