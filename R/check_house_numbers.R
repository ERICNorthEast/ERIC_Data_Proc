#' Checks for house numbers in location name column. Deals with them as far as possible
#'
#' @param inputdata
#' @param locsToReplace - tibble of locations to replace
#' @param locsToIgnore - tibble locations to ignore
#'
#' @return
#' @export
#'
#' @examples
#'
check_house_numbers <- function(inputdata, locsToReplace, locsToIgnore) {


  #Replace locations on our replace list
  inputdata <- dplyr::left_join(inputdata,locsToReplace)
  inputdata$`Location Name` <- ifelse(is.na(inputdata$Replacement),inputdata$`Location Name`,inputdata$Replacement)

  #Remove house numbers at start of string
  inputdata$`Location Name` <- stringr::str_remove(inputdata$`Location Name`,"^[0-9]{1,3}[,\\s]")

  #Check is what's left after removing numbers we're happy with
  check <- inputdata$`Location Name`
  for (r in ERIC_Data_Proc:::allowed_numbers) {
    check <- stringr::str_remove_all(check,r)
  }


  #Are there any numbers left to worry about?
  inputdata$flag1 <- stringr::str_detect(check,"[0-9]+") & is.na(match(inputdata$`Location Name`,table = locsToIgnore$`Location name`))
  return(inputdata)

}
