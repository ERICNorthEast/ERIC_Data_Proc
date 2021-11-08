
#' Read list of locations to replace from a CSV file
#'
#' @return
#' @export
#'
#' @examples
setup_locs_to_replace <- function(){
  file_name <-  "LocationsToReplace.csv"
  file_path <- system.file("extdata",file_name,package = "ERIC_Data_Proc")
  locsToReplace <- readr::read_csv(file_path)

  return(locsToReplace)
}
