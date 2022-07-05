#' Rename colums
#'
#' @param data_set Set of species data
#' @param data_format Source format of the data
#'
#' @return
#' @export
#'
#' @examples
rename_columns <- function(data_set,data_format) {

  #We want standard column names
  if (data_format == "std") {
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

    names(data_set)[tolower(names(data_set)) == "comment"] <- "Comments"
    names(data_set)[tolower(names(data_set)) == "comments"] <- "Comments"
  }

  if (data_format == "irecord") {
    names(data_set)[tolower(names(data_set)) == "recorder"] <- "Recorder"

    names(data_set)[tolower(names(data_set)) == "common name"] <- "Common Name"

    names(data_set)[tolower(names(data_set)) == "species"] <- "Species Name"
    names(data_set)[tolower(names(data_set)) == "taxon"] <- "Species Name"

    names(data_set)[tolower(names(data_set)) == "date to"] <- "Date"

    names(data_set)[tolower(names(data_set)) == "output map ref"] <- "Grid Reference"

    names(data_set)[tolower(names(data_set)) == "site name"] <- "Location Name"

    names(data_set)[tolower(names(data_set)) == "count of sex or stage"] <- "Abundances"

    names(data_set)[tolower(names(data_set)) == "comment"] <- "Comments"

  }
  if (data_format == "eric") {
    names(data_set)[tolower(names(data_set)) == "recorder"] <- "Recorder"

    names(data_set)[tolower(names(data_set)) == "common name"] <- "Common Name"

    names(data_set)[tolower(names(data_set)) == "species"] <- "Species Name"

    names(data_set)[tolower(names(data_set)) == "date to"] <- "Date"

    names(data_set)[tolower(names(data_set)) == "output map ref"] <- "Grid Reference"

    names(data_set)[tolower(names(data_set)) == "site name"] <- "Location Name"

    names(data_set)[tolower(names(data_set)) == "count of sex or state"] <- "Abundances"

    names(data_set)[tolower(names(data_set)) == "comment"] <- "Comments"

  }
  if (data_format == "bt") {
    names(data_set)[tolower(names(data_set)) == "observer name"] <- "Recorder"

    names(data_set)[tolower(names(data_set)) == "species"] <- "Common Name"

    names(data_set)[tolower(names(data_set)) == "scientific name"] <- "Species Name"

    names(data_set)[tolower(names(data_set)) == "date"] <- "Date"

    names(data_set)[tolower(names(data_set)) == "grid reference"] <- "Grid Reference"

    names(data_set)[tolower(names(data_set)) == "place"] <- "Location Name"

    names(data_set)[tolower(names(data_set)) == "count"] <- "Abundances"

    names(data_set)[tolower(names(data_set)) == "comment"] <- "Comments"

  }
  return(data_set)
}
