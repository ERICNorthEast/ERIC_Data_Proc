#' Format data for output for an SLA and perform some checks
#'
#' @param raw_data
#' @param OutputCols
#' @param newColNames
#'
#' @return
#' @export
#'
#' @examples
format_and_check_SLA_data <- function(raw_data,OutputCols,newColNames) {

  locsToReplace <- setup_locs_to_replace()
  locsToIgnore <- setup_locs_to_ignore()
  swearWords <- setup_profanity_config()
  recordersToIgnore <- setup_recorders_to_ignore()


  #AED Not sure on ordering the data
  outputdata <- dplyr::distinct(raw_data[with(raw_data,order(raw_data$Taxon.grou,raw_data$Taxon.Lati)),] )

  outputdata <- dplyr::select(outputdata,dplyr::all_of(unlist(OutputCols)))
  #Rename cols
  colnames(outputdata) <- unlist(newColNames)
  outputdata <- check_house_numbers(outputdata,locsToReplace,locsToIgnore)

  #Check for 0 counts that we haven't already sorted
  outputdata$flag2 <- tolower(outputdata$Abundances) =="0 count"


  #Check for swear words
  outputdata$flag3 <- stringr::str_detect(tolower(outputdata$Comments),paste(c(swearWords$word),collapse = "|"))


  #Check for e-mail addresses
  outputdata$flag4 <- stringr::str_detect(outputdata$Comments,"@")
  outputdata$flag5 <- stringr::str_detect(outputdata$Recorder,"@") & is.na(match(outputdata$`Recorder`,table = recordersToIgnore$`Recorder`))

  return (outputdata)
}
