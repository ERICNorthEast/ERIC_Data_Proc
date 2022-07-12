#' Read list of scientific phrases to flag on input
#' @return
#' @export
#'
#' @examples values <- setup_scientific_config()
setup_scientific_config <- function(){
  file_name <-  "Scientific.csv"
  file_path <- system.file("extdata",file_name,package = "ERICDataProc")

  scientific <- read.csv(file_path)

  return(scientific)
}
