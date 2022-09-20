#' Read list of invasive species to flag on input
#' @return
#' @export
#'
#' @examples values <- setup_invasives_config()
setup_invasives_config <- function(){
  file_name <-  "Invasives.csv"
  file_path <- system.file("extdata",file_name,package = "ERICDataProc")

  invasives <- read.csv(file_path)

  return(invasives)
}
