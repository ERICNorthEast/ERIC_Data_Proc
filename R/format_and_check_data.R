#' Format the data for output and perform some checks
#'
#' @param raw_data Set of species data
#' @param OutputCols List of the columns to be output
#' @param newColNames Labels for output columns
#' @param sensitivecheck Highlight sensitive species or not
#'
#' @return
#' @export
#'
#' @examples
format_and_check_data <- function(raw_data,OutputCols,newColNames, sensitivecheck) {

  locsToReplace <- setup_locs_to_replace()
  locsToIgnore <- setup_locs_to_ignore()
  swearWords <- setup_profanity_config()
  recordersToIgnore <- setup_recorders_to_ignore()

  #Get the columns we're going to output  sort by taxon group & latin name & discard duplicates
  # outputdata <- dplyr::distinct(raw_data[with(raw_data,order(raw_data$Taxon.grou,raw_data$Taxon.Lati)),] magrittr::%>% dplyr::select(all_of(OutputCols)))
  outputdata <- dplyr::distinct(raw_data[with(raw_data,order(raw_data$Taxon.grou,raw_data$Taxon.Lati)),] )

  outputdata <- dplyr::select(outputdata,dplyr::all_of(unlist(OutputCols)))
  # colnames(outputdata) <- newColNames
  colnames(outputdata) <- unlist(newColNames)


  #Code to check house nums


  outputdata <- check_house_numbers(outputdata,locsToReplace,locsToIgnore)

  #Check for 0 counts that we haven't already sorted
  outputdata$flag2 <- tolower(outputdata$Abundances) =="0 count"


  #Check for swear words
  outputdata$flag3 <- stringr::str_detect(tolower(outputdata$Comments),paste(c(swearWords$word),collapse = "|"))


  #Check for e-mail addresses
  outputdata$flag4 <- stringr::str_detect(outputdata$Comments,"@")
  outputdata$flag5 <- stringr::str_detect(outputdata$Recorder,"@") & is.na(match(outputdata$`Recorder`,table = recordersToIgnore$`Recorder`))

  #Sensitive species check
  if (sensitivecheck) {

    sens_config <- setup_sensitive_config()
    sensitive_species <- sens_config["sensitive_species"]
    sensitive_desigs <- sens_config["sensitive_desigs"]
    outputdata$flag6 <-stringr::str_detect(outputdata$'Latin Name',paste(unlist(sensitive_species),collapse = '|')) | stringr::str_detect(outputdata$Designations, paste(unlist(sensitive_desigs),collapse = '|'))
  }

  return(outputdata)
}
