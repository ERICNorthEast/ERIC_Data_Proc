#' Rename colums
#'
#' @param data_set Set of species data
#'
#' @return
#' @export
#'
#' @examples
rename_columns <- function(data_set) {

  #We want standard column names

  names(data_set)[tolower(names(data_set)) == "observer name"] <- "Recorder"
  names(data_set)[tolower(names(data_set)) == "observer"] <- "Recorder"
  names(data_set)[tolower(names(data_set)) == "recorder"] <- "Recorder"

  names(data_set)[tolower(names(data_set)) == "common name"] <- "Common Name"
  names(data_set)[tolower(names(data_set)) == "species"] <- "Common Name"

  names(data_set)[tolower(names(data_set)) == "species name"] <- "Species Name"
  names(data_set)[tolower(names(data_set)) == "latin name"] <- "Species Name"
  names(data_set)[tolower(names(data_set)) == "scientific name"] <- "Species Name"

  names(data_set)[tolower(names(data_set)) == "date"] <- "Date"
  names(data_set)[tolower(names(data_set)) == "when"] <- "Date"

  names(data_set)[tolower(names(data_set)) == "grid reference"] <- "Grid Reference"
  names(data_set)[tolower(names(data_set)) == "grid ref"] <- "Grid Reference"
  names(data_set)[tolower(names(data_set)) == "gridref"] <- "Grid Reference"
  names(data_set)[tolower(names(data_set)) == "gr"] <- "Grid Reference"
  names(data_set)[tolower(names(data_set)) == "gridreference"] <- "Grid Reference"

  names(data_set)[tolower(names(data_set)) == "location"] <- "Location Name"
  names(data_set)[tolower(names(data_set)) == "location name"] <- "Location Name"

  names(data_set)[tolower(names(data_set)) == "abundances"] <- "Abundances"
  names(data_set)[tolower(names(data_set)) == "abundance"] <- "Abundances"

  names(data_set)[tolower(names(data_set)) == "comments"] <- "Comments"
  names(data_set)[tolower(names(data_set)) == "comments"] <- "Comments"


  return(data_set)
}
