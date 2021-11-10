
#' Read list of locations to replace from a CSV file
#'
#' @return
#' @export
#'
#' @examples locs <- setup_locs_to_replace()
setup_locs_to_replace <- function(){
  file_name <-  "LocationsToReplace.csv"
  file_path <- system.file("extdata",file_name,package = "ERICDataProc")
  #locsToReplace <- readr::read_csv(file_path)
  locsToReplace <- read.csv(file_path)
  locsToReplace <- dplyr::rename(locsToReplace,'Location Name' = 'Location.Name')

  return(locsToReplace)
}
