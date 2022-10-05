#' Perform various fixes to data
#'
#' @param raw_data data frame of raw data
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
#'names(df) <- c('All.Design', 'Wildlife..', 'Wildlife_1', 'Taxon.grou', 'Taxon.Lati', 'Taxon.Comm', 'Obs.Commen', 'Sample.Rec', 'Sample.Loc', 'Sample.L_1', 'Sample.Dat', 'Sample.Spa', 'Survey.Run', 'Survey.Nam', 'Obs.Abunda', 'Determinat', 'Central_Ea', 'Central_No', 'Buffer', 'layer', 'path')
#'data <- do_data_fixes(df)
do_data_fixes <- function(raw_data){

  config_vals <- setup_survey_config_values()

  #Fish - relabel the taxon group
  fish_labels <- setup_fish_labels()
  raw_data$Taxon.grou <- stringr::str_replace_all(raw_data$Taxon.grou,fish_labels)

  #Zero moths - clear the abundance field for 0 counts in Tom Tans survey
  raw_data$Obs.Abunda <- ifelse((raw_data$Survey.Run=='Tom Tams' & raw_data$Obs.Abunda =='0 Count'),'',raw_data$Obs.Abunda)

  raw_data$Info <- ""

  #EA surveys & specific species - blank out location, obscure grid ref and add note about contacting EA
  RIVER_JELLY <- config_vals["RIVER_JELLY"]
  WCC_2005 <- config_vals["WCC_2005"]
  BRISTLE_MOSS <- config_vals["BRISTLE_MOSS"]
  ELM_LICHEN <- config_vals["ELM_LICHEN"]
  raw_data$Sample.Loc <- ifelse((raw_data$Survey.Nam == RIVER_JELLY & raw_data$Taxon.Lati == "Collema dichotomum") | (raw_data$Survey.Nam == WCC_2005 & raw_data$Taxon.Lati == "Austropotamobius pallipes") | (raw_data$Survey.Nam == BRISTLE_MOSS & raw_data$Taxon.Lati == "|thotrichum pallens") | (raw_data$Survey.Nam == BRISTLE_MOSS & raw_data$Taxon.Lati == "|thotrichum sprucei") | (raw_data$Survey.Nam == ELM_LICHEN & raw_data$Taxon.Lati == "Caloplaca luteoalba"),'',raw_data$Sample.Loc)
  raw_data$Info <- ifelse((raw_data$Survey.Nam == RIVER_JELLY & raw_data$Taxon.Lati == "Collema dichotomum") | (raw_data$Survey.Nam == WCC_2005 & raw_data$Taxon.Lati == "Austropotamobius pallipes") | (raw_data$Survey.Nam == BRISTLE_MOSS & raw_data$Taxon.Lati == "|thotrichum pallens") | (raw_data$Survey.Nam == BRISTLE_MOSS & raw_data$Taxon.Lati == "|thotrichum sprucei") | (raw_data$Survey.Nam == ELM_LICHEN & raw_data$Taxon.Lati == "Caloplaca luteoalba"),paste("The data search area contains ", raw_data$Taxon.Lati, " data.  Please contact the Environment Agency\'s North East Office for advice and more information."),'')
  raw_data$Sample.Spa <- ifelse(((raw_data$Survey.Nam == RIVER_JELLY & raw_data$Taxon.Lati == "Collema dichotomum") | (raw_data$Survey.Nam == WCC_2005 & raw_data$Taxon.Lati == "Austropotamobius pallipes") | (raw_data$Survey.Nam == BRISTLE_MOSS & raw_data$Taxon.Lati == "|thotrichum pallens") | (raw_data$Survey.Nam == BRISTLE_MOSS & raw_data$Taxon.Lati == "|thotrichum sprucei") | (raw_data$Survey.Nam == ELM_LICHEN & raw_data$Taxon.Lati == "Caloplaca luteoalba") & stringr::str_length(raw_data$Sample.Spa!=5)),paste0(stringr::str_sub(raw_data$Sample.Spa,1,3),stringr::str_sub(raw_data$Sample.Spa,(stringr::str_length(raw_data$Sample.Spa)+4)/2,(stringr::str_length(raw_data$Sample.Spa)+4)/2)),raw_data$Sample.Spa)


  #Planning surveys blank survey runn by & T. Coult blank survey name
  raw_data$Survey.Nam <- ifelse(stringr::str_detect(tolower(raw_data$Survey.Nam),'terry coult surveys'),'',raw_data$Survey.Nam)
  raw_data$Survey.Run <- ifelse(stringr::str_detect(tolower(raw_data$Survey.Nam),'planning application data & occasional records'),'',raw_data$Survey.Run)

  # Remove Terry Coult's name.
  raw_data$Sample.Rec <- ifelse(stringr::str_detect(tolower(raw_data$Sample.Rec),'t coult|t. coult|terry coult'),'',raw_data$Sample.Rec)
  raw_data$Survey.Run <- ifelse(stringr::str_detect(tolower(raw_data$Survey.Run),'t coult|t. coult|terry coult'),'',raw_data$Survey.Run)

  #RSNE - set recorder name
  raw_data$Sample.Rec <- ifelse(raw_data$Survey.Nam=='Red Squirrels Northern England','Red Squirrels Northern England',raw_data$Sample.Rec)

  #AONB Water voles - remove references to farms/farmers
  raw_data$Sample.Loc <- ifelse((raw_data$Survey.Nam=='North Pennines AONB - Water Vole Database') & stringr::str_detect(tolower(raw_data$Sample.Loc),'farm'),'',raw_data$Sample.Loc)
  raw_data$Sample.Rec <- ifelse((raw_data$Survey.Nam=='North Pennines AONB - Water Vole Database') & stringr::str_detect(tolower(raw_data$Sample.Rec),'(farmer)'),'',raw_data$Sample.Rec)

  #Butterfly conservation - set recorder name, for specific species (Grayling, Large Heath, Small Pearl-bordered Fritillary or Northern Brown Argus) blank the location and add a note about contacting BC
  butterfly_species <- setup_bfly_species()
  raw_data$Sample.Rec <- ifelse(raw_data$Survey.Nam=='Butterfly Conservation','Butterfly Conservation',raw_data$Sample.Rec)
  raw_data$Sample.Loc <-  ifelse(( raw_data$Survey.Nam=='Butterfly Conservation'   &  stringr::str_detect(raw_data$Taxon.Comm,paste(butterfly_species,collapse = '|'))),'',raw_data$Sample.Loc)
  raw_data$Info <-  ifelse(( raw_data$Survey.Nam=='Butterfly Conservation'   &  stringr::str_detect(raw_data$Taxon.Comm,paste(butterfly_species,collapse = '|'))),'For more information on this species please contact the North East branch of Butterfly Conservation',raw_data$Info)

  #NBMP - add a note linking to metadata
  raw_data$Info <- ifelse(raw_data$Survey.Nam=='National Bat Monitoring Programme - Durham','Please consult the BCT Metadata document www.ericnortheast.org.uk/wp-content/uploads/2019/08/NBMP-metadata_2011.pdf',raw_data$Info)

  #Bat worker records - add copyright statement
  # \U00A9 = copyright symbol
  raw_data$Info <-  ifelse((raw_data$Survey.Nam=='Batsites Inventory for England 1949 - 2011' | raw_data$Survey.Nam=='Natural England Bat Worker Records'),paste0('\U00A9 Natural England copyright. Contains Ordnance Survey data \U00A9 Crown copyright and database right ',format(Sys.Date(),'%Y')),raw_data$Info)

  fixed_data <- raw_data
  return(fixed_data)
}
