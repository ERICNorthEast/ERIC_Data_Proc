#' Read list of species to flag on input
#' @return
#' @export
#'
#' @examples words <- setup_species_config()
setup_species_config <- function(){
  file_name <-  "SpeciesToFlag.csv"
  file_path <- system.file("extdata",file_name,package = "ERICDataProc")

  species <- read.csv(file_path)

  return(species)
}
