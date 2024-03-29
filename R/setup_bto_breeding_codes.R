
#' Read list of BTO breeding codes from a CSV file
#'
#' @return
#' @export
#'
#' @examples breeding_codes <- setup_bto_breeding_codes()
setup_bto_breeding_codes <- function(){
    file_name <-  "BTOBreedingCodes.csv"
    file_path <- system.file("extdata",file_name,package = "ERICDataProc")

    btoBreedingCodes <- read.csv(file_path)
    btoBreedingCodes <- dplyr::rename(btoBreedingCodes,'Breeding status' = 'Number')
    btoBreedingCodes$`Breeding status` <- as.character(btoBreedingCodes$`Breeding status`)
    return(btoBreedingCodes)
}
