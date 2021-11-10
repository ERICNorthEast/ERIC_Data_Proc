#' Filter data by a given distance from a point defined by eastings and northings
#'
#' @param species_data  Species data
#' @param distance radius to remove beyond
#' @param grid_ref central grid ref
#'
#' @return
#' @export
#'
#' @examples
#'a <- c('','BirdsDir-A2.2')
#'b <- c('','')
#'c <- c('','')
#'d <- c('insect - butterfly','bird')
#'e <- c('Pieris rapae','Turdus merula')
#'f <- c('Small White','Blackbird')
#'g <- c('Mike Jeffries','Alnwick Wildlife Group')
#'h <- c('Morpeth Town','Morpeth Town')
#'i <- c('04/09/2010','04/09/2010')
#'j <- c('NZ18X','NZ18X')
#'k <- c('Butterfly Conservation','Alnwick Wildlife Group')
#'l <- c('Butterfly Conservation','Alnwick Wildlife Group')
#'m <- c('2 Count','1 Count')
#'n <- c('Considered Correct','Considered Correct')
#'o <- c(419000,419000)
#'p <- c(585000,585000)
#'q <- c(1000,1000)
#'r <- c('layer','layer')
#'s <- c('path','path')

#'df <- data.frame(a,b,c,d,e,f,a,g,h,h,i,j,k,l,m,n,o,p,q,r,s)


#'names(df) <- c('All.Design', 'Wildlife..', 'Wildlife_1', 'Taxon.grou', 'Taxon.Lati', 'Taxon.Comm', 'Obs.Commen', 'Sample.Rec', 'Sample.Loc', 'Sample.L_1', 'Sample.Dat', 'Sample.Spa', 'Survey.Run', 'Survey.Nam', 'Obs.Abunda', 'Determinat', 'Eastings', 'Northings', 'Buffer', 'layer', 'path')
#'data <- filter_by_distance(df,1.0,"NZ123456")

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
