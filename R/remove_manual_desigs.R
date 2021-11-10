#' Filter out designations we used to remove manually
#'
#' @param pandn_data data to process
#' @param config_vals list of config values
#'
#' @return
#' @export
#'
#' @examples
#' config_keys <- c("FEP_DESIG","WNIO1_DESIG","WNIO2_DESIG","WNIO81_DESIG","WNIO82_DESIG","WNIO5_DESIG","WALES_DESIG")

#' FEP_DESIG <- "FEP-001"
#' WNIO1_DESIG <- "W(NI)O-Sch1_part1"
#' WNIO2_DESIG <- "W(NI)O-Sch1_part2"
#' WNIO81_DESIG <- "W(NI)O-Sch8_part1"
#' WNIO82_DESIG <- "W(NI)O-Sch8_part2"
#' WNIO5_DESIG <- "W(NI)O-Sch5"
#' WALES_DESIG <- "Wales_NERC_S.42"

#' config_values <- list(FEP_DESIG,WNIO1_DESIG,WNIO2_DESIG,WNIO81_DESIG,WNIO82_DESIG,WNIO5_DESIG,WALES_DESIG)

#' config_pairs <- list()                     # Create empty list
#' for(i in 1:length(config_keys)) {              # Add key/value pairs in for-loop
#' config_pairs[config_keys[i]] <- config_values[i]
#' }

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

#'df <- data.frame(a,b,c,d,e,f,a,g,h,h,i,j,k,l,m,n,o,p,q,r,s,b)


#'names(df) <- c('All.Design', 'Wildlife..', 'Wildlife_1', 'Taxon.grou', 'Taxon.Lati', 'Taxon.Comm', 'Obs.Commen', 'Sample.Rec', 'Sample.Loc', 'Sample.L_1', 'Sample.Dat', 'Sample.Spa', 'Survey.Run', 'Survey.Nam', 'Obs.Abunda', 'Determinat', 'Central_Ea', 'Central_No', 'Buffer', 'layer', 'path','Info')
#'
#' data <- remove_manual_desigs(df,config_pairs)
remove_manual_desigs <- function(pandn_data,config_vals){

  FEP_DESIG <- config_vals["FEP_DESIG"]
  WNIO1_DESIG <- config_vals["WNIO1_DESIG"]
  WNIO2_DESIG <- config_vals["WNIO2_DESIG"]
  WNIO81_DESIG <- config_vals["WNIO81_DESIG"]
  WNIO82_DESIG <- config_vals["WNIO82_DESIG"]
  WNIO5_DESIG <- config_vals["WNIO5_DESIG"]
  WALES_DESIG <- config_vals["WALES_DESIG"]

  filtered_data <- dplyr::filter(pandn_data,!(pandn_data$All.Design== "" | pandn_data$All.Design== unlist(FEP_DESIG) | pandn_data$All.Design== unlist(WNIO1_DESIG) | pandn_data$All.Design== unlist(WNIO2_DESIG) | pandn_data$All.Design== unlist(WNIO81_DESIG) | pandn_data$All.Design== unlist(WNIO82_DESIG) | pandn_data$All.Design== unlist(WNIO5_DESIG) | pandn_data$All.Design== unlist(WALES_DESIG)))
  return(filtered_data)
}
