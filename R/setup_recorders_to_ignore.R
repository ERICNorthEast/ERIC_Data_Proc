
#' Real list of recorders to ignore from a csv file
#'
#' @return
#' @export
#'
#' @examples recs <- setup_recorders_to_ignore()
setup_recorders_to_ignore <- function(){
  file_name <-  "RecordersToIgnore.csv"
  file_path <- system.file("extdata",file_name,package = "ERICDataProc")
  #recordersToIgnore <- readr::read_csv(file_path)
  recordersToIgnore <- read.csv(file_path)

  return(recordersToIgnore)
}
