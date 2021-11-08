#' Filter data by a given distance from a popint defined by eastings and northings
#'
#' @param species_data  Species data
#' @param distance radius to remove beyond
#' @param grid_ref central grid ref
#'
#' @return
#' @export
#'
#' @examples

filter_by_distance <- function(species_data,distance,grid_ref) {

  dist_limit <- (distance * 1000) + 100
  pinpoint <- dplyr::filter(ERICDataProc:::gridLetters,ERICDataProc:::gridLetters$Letters==stringr::str_sub(grid_ref,1,2))
  gr_n <- stringr::str_pad(paste0(pinpoint$North,stringr::str_sub(grid_ref,(3+stringr::str_length(grid_ref)+2)/2)),6,"right",pad="0")
  gr_e <- stringr::str_pad(paste0(pinpoint$East,stringr::str_sub(grid_ref,3,(stringr::str_length(grid_ref)+2)/2)),6,"right",pad="0")

  species_data$distance <- sqrt((as.numeric(species_data$Eastings) - as.numeric(gr_e))^2 + (as.numeric(species_data$Northings) - as.numeric(gr_n))^2)
  species_data$distance <- as.integer(ifelse(stringr::str_length(species_data$Sample.Spa)<8,'',species_data$distance+0.5))
  species_data$distance[is.na(species_data$distance)] <- ''


  species_data <- dplyr::filter(species_data,(species_data$distance=="" | as.numeric(species_data$distance)<dist_limit))

  species_data$distance <- type.convert(species_data$distance)

  return(species_data)
}
