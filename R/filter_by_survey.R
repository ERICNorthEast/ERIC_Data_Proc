#' Remove data from a couple of surveys
#'
#' @param raw_data
#'
#' @return filtered data
#' @export
#'
#' @examples
filter_by_survey <- function(raw_data){
  raw_data <- dplyr::filter(raw_data,!(raw_data$Survey.Run=='Northumbrian Water' & raw_data$Taxon.Comm=='Violet Crystalwort'))
  filtered_data <- dplyr::filter(raw_data,!(raw_data$Survey.Run=='Tom Tams' & raw_data$Taxon.Comm=='Bedstraw Hawk-moth' & ((stringr::str_length(raw_data$Sample.Spa)==5 & stringr::str_sub(raw_data$Sample.Spa,end=4)=='NU14') | (stringr::str_c(stringr::str_sub(raw_data$Sample.Spa,end=3),stringr::str_sub(raw_data$Sample.Spa,start=(stringr::str_length(raw_data$Sample.Spa)+4)/2,end=(stringr::str_length(raw_data$Sample.Spa)+4)/2))=='NU14'))))

  return(filtered_data)
}
