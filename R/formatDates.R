#' formats Dates as far as possible given the variety that may be presented
#' Seems to correct 1905 dates - e.g. 12/07/1905 was correctly changed to 2020
#'
#' @param dateCol Column containing date values
#'
#' @return
#' @export
#'
#' @examples
#'e <- c('Pieris rapae','Turdus merula')
#'f <- c('Small White','Blackbird')
#'g <- c('Mike Jeffries','Alnwick Wildlife Group')
#'h <- c('Morpeth Town','Morpeth Town')
#'i <- c('04/09/2010','04/09/2010')

#'df <- data.frame(e,f,g,h,h,i)


#'names(df) <- c('Taxon.Lati', 'Taxon.Comm', 'Sample.Rec', 'Sample.Loc', 'Sample.L_1', 'Sample.Dat')
#' df$'Sample.Dat'<-formatDates(df$'Sample.Dat')
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
