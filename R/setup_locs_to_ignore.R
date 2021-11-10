
#' Read list of locations to ignore from a CSV file
#'
#' @return
#' @export
#'
#' @examples locs <- setup_locs_to_ignore()
setup_locs_to_ignore <- function(){
    file_name <-  "LocationsToIgnore.csv"
    file_path <- system.file("extdata",file_name,package = "ERICDataProc")
    locsToIgnore <- readr::read_csv(file_path)

    return(locsToIgnore)
}
