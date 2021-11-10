#' Remove the optional designations selected by the user
#'
#' @param pandn_data data to process
#' @param optional_desigs Designations selected by the user
#' @param config_vals List of config values
#'
#' @return
#' @export
#'
#' @examples
#'
#' config_keys <- c("NORTH_DESIGS","NP_DESIGS","SCOT_DESIGS","TV_DESIGS","DURHAM_DESIGS")

#' NORTH_DESIGS <- 'pandn_data$All.Design== "Northumberland BAP" |  pandn_data$All.Design=="FEP-001, Northumberland NP BAP"'
#' NP_DESIGS <- 'pandn_data$All.Design=="FEP-001, Northumberland NP BAP"'
#' SCOT_DESIGS <- 'pandn_data$All.Design== "Scottish_Biodiversity_List" | pandn_data$All.Design== "Scottish_Biodiversity_List, W(NI)O-Sch1_part1" |pandn_data$All.Design== "Scottish_Biodiversity_List, Wales_NERC_S.42" |pandn_data$All.Design== "FEP-001, Scottish_Biodiversity_List"'
#' TV_DESIGS <- 'pandn_data$All.Design=="Tees Valley BAP" | pandn_data$All.Design=="Tees Valley BAP, " | pandn_data$All.Design=="Tees Valley BAP, FEP-001" | pandn_data$All.Design=="Tees Valley BAP, Tees Valley BAP" | pandn_data$All.Design=="Tees Valley BAP, Tees Valley BAP, W(NI)O-Sch8_part1, Wales_NERC_S.42" | pandn_data$All.Design=="Tees Valley BAP, W(NI)O-Sch8_part1, Wales_NERC_S.42"'
#' DURHAM_DESIGS <- 'pandn_data$All.Design=="Durham BAP"'

#' config_values <- list(NORTH_DESIGS,NP_DESIGS,SCOT_DESIGS,TV_DESIGS,DURHAM_DESIGS)

#' config_pairs <- list()                     # Create empty list
#' for(i in 1:length(config_keys)) {              # Add key/value pairs in for-loop
#'   config_pairs[config_keys[i]] <- config_values[i]
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
#' options <- c("DURHAM_DESIGS","NP_DESIGS","TV_DESIGS"  )
#' data <- remove_optional_desigs(df,options,config_pairs)
remove_optional_desigs <- function(pandn_data,optional_desigs,config_vals) {

  DURHAM_DESIGS <- config_vals["DURHAM_DESIGS"]
  NORTH_DESIGS <- config_vals["NORTH_DESIGS"]
  NP_DESIGS <- config_vals["NP_DESIGS"]
  SCOT_DESIGS <- config_vals["SCOT_DESIGS"]
  TV_DESIGS <- config_vals["TV_DESIGS"]

  # And the optional ones
  for (i in optional_desigs)
  {

    pandn_data <- dplyr::filter(pandn_data,!(eval(str2expression(unlist(eval(str2expression(i)))))))
  }

  filtered_data <- pandn_data

  return(filtered_data)
}
