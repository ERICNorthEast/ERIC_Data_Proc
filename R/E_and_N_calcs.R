#' Calculate eastings and northings based on grid ref
#'
#' @param species_data Species data including a grid ref
#'
#' @return
#' @export
#'
#' @examples
#'
#'


E_and_N_calcs<- function(species_data) {

  # Easting & northing calculations
  species_data$`Sample.Spa` <- stringr::str_trim(stringr::str_replace_all(species_data$`Sample.Spa`," ",""))

  species_data$prefix <- stringr::str_sub(species_data$`Sample.Spa`,end=2)

  species_data <- dplyr::left_join(species_data,ERICDataProc:::prefixes,by="prefix")
  species_data$Letter <- tolower(stringr::str_sub(species_data$`Sample.Spa`,5,5))
  species_data <- dplyr::left_join(species_data,ERICDataProc:::suffices,b="Letter")


  species_data$Buffer <- ifelse(stringr::str_length(species_data$`Sample.Spa`)==5,1000,(1*10^(5-(stringr::str_length(species_data$`Sample.Spa`)-2)/2))/2)
  species_data$Eastings <- ifelse(stringr::str_length(species_data$`Sample.Spa`)==5,stringr::str_c(stringr::str_sub(species_data$pvalue,1,1),stringr::str_sub(species_data$`Sample.Spa`,3,2+(stringr::str_length(species_data$`Sample.Spa`)-2)/2),as.character(species_data$E)),stringr::str_pad(stringr::str_c(stringr::str_sub(species_data$pvalue,1,1),stringr::str_sub(species_data$`Sample.Spa`,3,2+(stringr::str_length(species_data$`Sample.Spa`)-2)/2)),6,"right","0"))
  species_data$Northings <- ifelse(stringr::str_length(species_data$`Sample.Spa`)==5,stringr::str_c(stringr::str_sub(species_data$pvalue,2,2),stringr::str_sub(stringr::str_sub(species_data$`Sample.Spa`,3+(stringr::str_length(species_data$`Sample.Spa`)-2)/2),1,1),as.character(species_data$N)),stringr::str_pad(stringr::str_c(stringr::str_sub(species_data$pvalue,2,2),stringr::str_sub(species_data$`Sample.Spa`,3+(stringr::str_length(species_data$`Sample.Spa`)-2)/2)),6,"right","0"))

  species_data$Central_eastings <- ifelse(stringr::str_length(species_data$`Sample.Spa`)==5,as.numeric(species_data$Eastings),as.numeric(species_data$Eastings)+as.numeric(species_data$Buffer))
  species_data$Central_northings <- ifelse(stringr::str_length(species_data$`Sample.Spa`)==5,as.numeric(species_data$Northings),as.numeric(species_data$Northings)+as.numeric(species_data$Buffer))

  species_data$Precision <- species_data$Buffer * 2

  return(species_data)
}

