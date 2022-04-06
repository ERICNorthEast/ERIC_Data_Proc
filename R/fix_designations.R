#' Fix designations as they're incorrect in Recorder - not sure if this is still needed
#'
#' @param raw_data data frame of raw data
#' @param SLA is this an SLA dataset
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
#' data <- fix_designations(df,FALSE)
fix_designations <- function(raw_data,SLA){

  desig_config <- setup_desig_config_values()


  #Designations for corrections
  BIRD_AMBER <-  desig_config["BIRD_AMBER"]
  BIRD_RED <-  desig_config["BIRD_RED"]
  DURHAM_BAP <-  desig_config["DURHAM_BAP"]
  NLAND_BAP <-  desig_config["NLAND_BAP"]
  TV_BAP <-  desig_config["TV_BAP"]

  #Species lists
  amber_sp <- desig_config["amber_sp"]
  red_sp <- desig_config["red_sp"]
  red_from_amber_sp <- desig_config["red_from_amber_sp"]
  amber_from_red_sp <- desig_config["amber_from_red_sp"]
  durham_bap_sp <- desig_config["durham_bap_sp"]
  nland_bap_sp <- desig_config["nland_bap_sp"]
  tv_bap_sp <- desig_config["tv_bap_sp"]


  #Sort out designations (EA doesn't need this)
  #Need to finish looking at this
   raw_data$All.Design <- ifelse(stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(amber_sp),collapse = '|')),paste(BIRD_AMBER,raw_data$All.Design),raw_data$All.Design)
   raw_data$All.Design <- ifelse(stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(red_sp),collapse = '|')),paste(BIRD_RED,raw_data$All.Design),raw_data$All.Design)
  #raw_data$All.Design<-ifelse(stringr::str_detect(raw_data$Taxon.Lati, paste(unlist(amber_sp), collapse = "|")) && !stringr::str_detect(raw_data$All.Design,BIRD_AMBER), paste(BIRD_AMBER, raw_data$All.Design), raw_data$All.Design)
  #raw_data$All.Design<-ifelse(stringr::str_detect(raw_data$Taxon.Lati, paste(unlist(red_sp), collapse = "|")) && !stringr::str_detect(raw_data$All.Design,BIRD_RED), paste(BIRD_RED, raw_data$All.Design), raw_data$All.Design)


  #Red from amber
  raw_data$All.Design <- ifelse((stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(red_from_amber_sp),collapse = '|')) & stringr::str_detect(raw_data$All.Design,unlist(BIRD_AMBER))),stringr::str_replace_all(raw_data$All.Design,BIRD_AMBER,BIRD_RED),raw_data$All.Design)
  raw_data$All.Design <- ifelse((stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(red_from_amber_sp),collapse = '|')) & !stringr::str_detect(raw_data$All.Design,unlist(BIRD_AMBER))),paste(BIRD_RED,raw_data$All.Design),raw_data$All.Design)

  #Amber from red
  raw_data$All.Design <- ifelse((stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(amber_from_red_sp),collapse = '|')) & stringr::str_detect(raw_data$All.Design,unlist(BIRD_RED))),stringr::str_replace_all(raw_data$All.Design,BIRD_RED,BIRD_AMBER),raw_data$All.Design)
  raw_data$All.Design <- ifelse((stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(amber_from_red_sp),collapse = '|')) & !stringr::str_detect(raw_data$All.Design,unlist(BIRD_RED))),paste(BIRD_AMBER,raw_data$All.Design),raw_data$All.Design)

  #Local BAPs (NELBAP column is SLA only)
  raw_data$All.Design <- ifelse(stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(durham_bap_sp),collapse = '|')),paste(DURHAM_BAP,raw_data$All.Design),raw_data$All.Design)
  raw_data$All.Design <- ifelse(stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(nland_bap_sp),collapse = '|')), paste(NLAND_BAP,raw_data$All.Design),raw_data$All.Design)
  raw_data$All.Design <- ifelse(stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(tv_bap_sp),collapse = '|')),    paste(TV_BAP,raw_data$All.Design),raw_data$All.Design)

  raw_data$NELBAP <- ifelse(stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(durham_bap_sp),collapse = '|')),paste(DURHAM_BAP,raw_data$NELBAP),raw_data$NELBAP)
  raw_data$NELBAP <- ifelse(stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(nland_bap_sp),collapse = '|')), paste(NLAND_BAP,raw_data$NELBAP),raw_data$NELBAP)
  raw_data$NELBAP <- ifelse(stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(tv_bap_sp),collapse = '|')),    paste(TV_BAP,raw_data$NELBAP),raw_data$NELBAP)

  #The following are only needed for non-SLA datasets
  if (SLA == FALSE) {
    WACA1 <- desig_config["WACA1"]
    WACA2 <- desig_config["WACA2"]

    #WACA - don't do this for SLAs
    raw_data$All.Design <-ifelse((raw_data$Wildlife_1=='Yes' &!is.na(raw_data$Wildlife_1)),paste0(WACA1,raw_data$All.Design),raw_data$All.Design)
    raw_data$All.Design <-ifelse((raw_data$Wildlife..=='Yes' &!is.na(raw_data$Wildlife..)) ,paste0(WACA2,raw_data$All.Design),raw_data$All.Design)

    #Protected species - don't do this for SLAs
    NO_DESIG <- desig_config["NO_DESIG"]
    prot_config <- setup_prot_config()
    prot_species <- prot_config["prot_species"]
    prot_groups <- prot_config["prot_groups"]

    raw_data$All.Design <- ifelse((raw_data$All.Design=='' & (raw_data$Taxon.grou=='amphibian' & raw_data$Taxon.Lati != 'Mesotriton alpestris')),NO_DESIG,raw_data$All.Design)
    raw_data$All.Design <- ifelse((raw_data$All.Design=='' & (stringr::str_detect(raw_data$Taxon.grou,paste(unlist(prot_groups),collapse = '|')) | raw_data$Taxon.Lati == prot_species[1]| raw_data$Taxon.Lati == prot_species[2]| raw_data$Taxon.Lati == prot_species[3]| raw_data$Taxon.Lati == prot_species[4])),NO_DESIG,raw_data$All.Design)

    #Waxcaps - don't do this for SLAs
    WAXCAPS <- desig_config["WAXCAPS"]
    waxcap_sp <- desig_config["waxcap_sp"]

    raw_data$All.Design <- ifelse(stringr::str_detect(raw_data$Taxon.Lati,paste(unlist(waxcap_sp),collapse = '|')),paste(WAXCAPS,raw_data$All.Design),raw_data$All.Design)
    raw_data$All.Design <-  stringr::str_replace(stringr::str_replace_all(raw_data$All.Design,' ,',','),',$','')
  }

  fixed_data <- raw_data

  return(fixed_data)
}
