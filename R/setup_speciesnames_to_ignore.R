
#' Real list of species names to ignore from a csv file
#'
#' @return
#' @export
#'
#' @examples spcs <- setup_speciesnames_to_ignore()
setup_speciesnames_to_ignore <- function(){
  file_name <-  "SpeciesNamesToIgnore.csv"
  file_path <- system.file("extdata",file_name,package = "ERICDataProc")

  speciesnamesToIgnore <- read.csv(file_path)

  return(speciesnamesToIgnore)
}
