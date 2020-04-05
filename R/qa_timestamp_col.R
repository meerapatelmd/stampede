#' Returns a dataframe of the different parsing methods for a timestamp column
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate mdy_hms
#' @import dplyr
#' @export


qa_timestamp_col <-
        function(dataframe, timestamp_col) {
                ##Prep
                timestamp_col <- enquo(timestamp_col)

                ##Converting to character to check formating and parsing using lubridate functions
                dataframe_01 <-
                        dataframe %>%
                        dplyr::select(!!timestamp_col) %>%
                        dplyr::mutate(AS_CHAR = as.character(!!timestamp_col)) %>%
                        dplyr::mutate(AS_NUM = as.numeric(AS_CHAR)) %>%
                        dplyr::mutate(YMD_HMS = lubridate::ymd_hms(AS_CHAR, quiet = TRUE)) %>%
                        dplyr::mutate(YMD_HM = lubridate::ymd_hm(AS_CHAR, quiet = TRUE)) %>%
                        dplyr::mutate(MDY_HMS = lubridate::mdy_hms(AS_CHAR, quiet = TRUE)) %>%
                        dplyr::mutate(MDY_HM = lubridate::mdy_hms(AS_CHAR, quiet = TRUE)) %>%
                        dplyr::mutate(R_ORIGIN = as.Date(AS_NUM, origin = "1970-01-01")) %>%
                        dplyr::mutate(R_ORIGIN_VALID = R_ORIGIN < Sys.Date()) %>%
                        dplyr::mutate(EXCEL_ORIGIN = as.Date(AS_NUM, origin = "1900-01-01")) %>%
                        dplyr::mutate(EXCEL_ORIGIN_VALID = EXCEL_ORIGIN < Sys.Date())

                return(dataframe_01)
        }
