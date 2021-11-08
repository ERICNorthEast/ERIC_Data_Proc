#' formats Dates as far as possible given the variety that may be presented
#'
#' @param dateCol Column containing date values
#'
#' @return
#' @export
#'
#' @examples
formatDates <- function(dateCol) {
  result <- stringr::str_replace_all(
    ifelse(is.na(
      ifelse(stringr::str_length(dateCol)==4 & dateCol<3000,dateCol,
             as.character(format(janitor::excel_numeric_to_date(as.numeric(dateCol)),"%d/%m/%Y"))
      )),dateCol,
      ifelse(stringr::str_length(dateCol)==4 & dateCol<3000,dateCol,
             as.character(format(janitor::excel_numeric_to_date(as.numeric(dateCol)),"%d/%m/%Y"))
      ))
    ,"\\.","/")
}
