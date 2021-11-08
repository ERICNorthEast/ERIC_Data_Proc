
#' Real list of recorders to ignore from a csv file
#'
#' @return
#' @export
#'
#' @examples
setup_recorders_to_ignore <- function(){
  file_name <-  "RecordersToIgnore.csv"
  file_path <- system.file("extdata",file_name,package = "ERIC_Data_Proc")
  recordersToIgnore <- readr::read_csv(file_path)

  return(recordersToIgnore)
}
