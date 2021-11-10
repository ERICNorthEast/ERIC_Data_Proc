#' Remove data from a couple of surveys
#'
#' @param raw_data
#'
#' @return filtered data
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


#'names(df) <- c('All.Design', 'Wildlife..', 'Wildlife_1', 'Taxon.grou', 'Taxon.Lati', 'Taxon.Comm', 'Obs.Commen', 'Sample.Rec', 'Sample.Loc', 'Sample.L_1', 'Sample.Dat', 'Sample.Spa', 'Survey.Run', 'Survey.Nam', 'Obs.Abunda', 'Determinat', 'Central_Ea', 'Central_No', 'Buffer', 'layer', 'path')
#' data <- filter_by_survey(df)
filter_by_survey <- function(raw_data){
  raw_data <- dplyr::filter(raw_data,!(raw_data$Survey.Run=='Northumbrian Water' & raw_data$Taxon.Comm=='Violet Crystalwort'))
  filtered_data <- dplyr::filter(raw_data,!(raw_data$Survey.Run=='Tom Tams' & raw_data$Taxon.Comm=='Bedstraw Hawk-moth' & ((stringr::str_length(raw_data$Sample.Spa)==5 & stringr::str_sub(raw_data$Sample.Spa,end=4)=='NU14') | (stringr::str_c(stringr::str_sub(raw_data$Sample.Spa,end=3),stringr::str_sub(raw_data$Sample.Spa,start=(stringr::str_length(raw_data$Sample.Spa)+4)/2,end=(stringr::str_length(raw_data$Sample.Spa)+4)/2))=='NU14'))))

  return(filtered_data)
}
