#' Rename colums
#'
#' @param data_set Set of species data
#' @param data_format Source format of the data
#'
#' @return
#' @export
#'
#' @examples
#' #'sn <- c('Pieris rapae','Turdus merula')
#'cn <- c('Small White','Blackbird')
#'rec <- c('Mike Jeffries','Alnwick Wildlife Group')
#'loc <- c('Morpeth Town','Morpeth Town')
#'dt <- c('04/09/2010','04/09/2010')
#'gr <- c('NZ18X','NZ18X')
#'com <- c('Butterfly Conservation','Alnwick Wildlife Group')

#'ab <- c('2 Count','1 Count')

#'data <- data.frame(rec,cn,sn,dt,gr,loc,ab,com)
#'colmames(data) <- c("observer","Common Name","latin Name", "Date", "Grid Ref", "Location", "Abundance","Comments")
#' result <- rename_columns(data,'std')
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

    #If we haven't got both species columns add the missing one
    if (!all(c("Common Name","Species Name") %in% colnames(data_set)))
    {
      if ("Common Name" %in% colnames(data_set))
      {
        data_set$'Species Name' <- ""
      } else
      {
        data_set$'Common Name' <- ""
      }
    }
  }

  if (data_format == "irecord") {
    names(data_set)[tolower(names(data_set)) == "recorder"] <- "Recorder"

    names(data_set)[tolower(names(data_set)) == "common name"] <- "Common Name"

    names(data_set)[tolower(names(data_set)) == "species"] <- "Species Name"
    names(data_set)[tolower(names(data_set)) == "taxon"] <- "Species Name"

    #names(data_set)[tolower(names(data_set)) == "date to"] <- "Date"
    names(data_set)[tolower(names(data_set)) == "date interpreted"] <- "Date"

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
