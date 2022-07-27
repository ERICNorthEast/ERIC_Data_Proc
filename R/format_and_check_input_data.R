#' Format the data for input and perform some checks
#'
#' 'Use this function to perform checks on incoming data
#' Flag whether iRecord, ERIC, standard data, NTBC, and
#'
#' @param raw_data Set of species data
#' @param locCheck whether to check for blank locations
#' @param inputFormat the source of the data, e.g. iRecord
#' @param recorderName name of recorder to insert into BirdTrack data
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

#'df <- data.frame(a,b,c,d,e,f,a,g,h,h,i,j,k,l,m,n,o,p,q,r,s,b)


#'names(df) <- c('All.Design', 'Wildlife..', 'Wildlife_1', 'Taxon.grou', 'Taxon.Lati', 'Taxon.Comm', 'Obs.Commen', 'Sample.Rec', 'Sample.Loc', 'Sample.L_1', 'Sample.Dat', 'Sample.Spa', 'Survey.Run', 'Survey.Nam', 'Obs.Abunda', 'Determinat', 'Central_Ea', 'Central_No', 'Buffer', 'layer', 'path','Info')
#'OutputCols <- c("All.Design","Taxon.grou", "Taxon.Lati","Taxon.Comm", "Obs.Abunda", "Determinat","Obs.Commen","Sample.Rec",  "Sample.Loc",  "Sample.Dat","Sample.Spa","Survey.Run","Survey.Nam","Info")
#'newColNames <- c("Designations","Taxon group","Latin Name","Common Name","Abundances","Determination Type","Comments","Recorder","Location Name","Date","Grid Reference","Survey Run By","Survey Name","Additional Information")
#'data <- format_and_check_data(df,OutputCols,newColNames, 0)

format_and_check_input_data <- function(raw_data,locCheck,inputFormat,recorderName) {

  locsToReplace <- setup_locs_to_replace()
  locsToIgnore <- setup_locs_to_ignore()
  swearWords <- setup_profanity_config()
  speciesToFlag <- setup_species_config()
  recordersToIgnore <- setup_recorders_to_ignore()
  speciesToIgnore <- setup_speciesnames_to_ignore()
  scientific <- setup_scientific_config()

  #FORMATTING

  #If dataSource is iRecord
  if (inputFormat == "irecord") {
  # Add verifier to observer
    raw_data["Verifier"][is.na(raw_data["Verifier"])] <- ""
    raw_data$Recorder<-paste(raw_data$Recorder,raw_data$Verifier,sep=", ")


    #Replace "present" in Abundances
    raw_data["Abundances"][(raw_data["Abundances"])=="Present"] <- ""

    #Remove NA throughout
    raw_data[is.na(raw_data)]<-""

    # Build comment field

    raw_data["Sample method"][(raw_data["Sample method"])=="Unknown"] <- ""

    raw_data["Sex"][(raw_data["Sex"])=="not recorded"] <- ""

    raw_data["Stage"][(raw_data["Stage"])=="not recorded"] <- ""


    raw_data$Biotope <- ifelse(raw_data$Biotope=="","",paste("Biotope:",raw_data$Biotope))

    raw_data$Comments<-str_trim(paste(raw_data$Comments, raw_data$`Sample comment`,raw_data$Biotope,raw_data$`Sample method`,raw_data$Sex,raw_data$Stage))

  }
  #If dataSource is ERIC website
  if (inputFormat == "eric") {
    #Replace "present" in Abundances
    raw_data["Abundances"][(raw_data["Abundances"])=="Present"] <- ""


    #Remove NA throughout
    raw_data[is.na(raw_data)]<-""

    #Build comment field
    raw_data["Sample method"][(raw_data["Sample method"])=="Unknown"] <- ""
    raw_data["Sex"][(raw_data["Sex"])=="not recorded"] <- ""
    raw_data["Stage"][(raw_data["Stage"])=="not recorded"] <- ""
    raw_data["Biotope"][is.na(raw_data["Biotope"])] <- ""
    raw_data$Biotope <- ifelse(raw_data$Biotope=="","",paste("Biotope:",raw_data$Biotope))


    raw_data$Comments<-str_trim(paste(raw_data$Comments, raw_data$`Sample comment`,raw_data$Biotope,raw_data$`Sample method`,raw_data$Sex,raw_data$Stage))
  }

  #If dataSource is BirdTrack
  if (inputFormat == "bt") {

    #Get breeding codes
    breeding_codes <- setup_bto_breeding_codes()
    raw_data <- dplyr::left_join(raw_data,breeding_codes)

    #Setup a column for the observer name
    raw_data$Recorder <- recorderName

    #Remove any "present" values in abundance column
    raw_data["Abundances"][(raw_data["Abundances"])=="Present"] <- ""

    #Remove NA throughout
    raw_data[is.na(raw_data)]<-""

    #Build comment field
    raw_data$Comments<-str_trim(paste(raw_data$Comments, raw_data$Plumage,ifelse(raw_data$`Breeding details`=="",ifelse(raw_data$`Status`=="","",paste("Breeding status:",raw_data$`Status`)),raw_data$`Breeding details`)))

    #Get the grid ref from one of two columns
    raw_data$`Grid Reference`<- ifelse(raw_data$Pinpoint=="",raw_data$`Grid Reference`,raw_data$Pinpoint)

    }

  #Remove NA throughout
  raw_data[is.na(raw_data)]<-""

  #Add row number column
  rowNo <- seq_len(nrow(raw_data))
  outputData<-cbind(raw_data,rowNo)
  names(outputData)[names(outputData) == 'rowNo'] <- 'Row No'


  #Strip commas and full stops from the end of the Recorder name
  outputData$Recorder <- str_replace(outputData$Recorder,"[.,]$","")


  #Check recorder for blanks, email addresses, ampersands or "Mr and mrs" type but ignore allowed values
  outputData$flagRec <- is.na(outputData$Recorder == "") | is.na(outputData$Recorder) | !str_detect(outputData$Recorder," ") | stringr::str_detect(outputData$Recorder,"@") | stringr::str_detect(tolower(outputData$Recorder)," and ") | stringr::str_detect(outputData$Recorder,"&") & is.na(match(outputData$`Recorder`,table = recordersToIgnore$`Recorder`))

  #Check species
  #outputData$flagSpecies <- is.na(outputData$`Common Name`== "" & outputData$`Species Name` == "") | (!stringr::str_detect(tolower(outputData$`Species Name`)," ") & (is.na(match(tolower(outputData$`Species Name`),speciesToIgnore$Species)))) | stringr::str_detect(tolower(outputData$'Common Name'),paste(c(tolower(species$species)),collapse = "|")) | stringr::str_detect(tolower(outputData$'Species Name'),paste(c(tolower(species$species)),collapse = "|")) | stringr::str_detect(tolower(outputData$'Species Name'),paste(c(tolower(scientific$term)),collapse = "|"))
  #outputData$flagSpecies <- is.na(outputData$`Common Name`== "" & outputData$`Species Name` == "") |  stringr::str_detect(tolower(outputData$'Common Name'),paste(c(tolower(species$species)),collapse = "|")) | stringr::str_detect(tolower(outputData$'Species Name'),paste(c(tolower(species$species)),collapse = "|")) | stringr::str_detect(tolower(outputData$'Species Name'),paste(c(tolower(scientific$term)),collapse = "|"))
  outputData$flagSpecies <- is.na(outputData$`Common Name`== "" & outputData$`Species Name` == "") |  stringr::str_detect(tolower(outputData$'Species Name'),paste(c(tolower(scientific$term)),collapse = "|"))
  speciesToFlag$match <- "yes"
  cnMatch <- dplyr::left_join(outputData, speciesToFlag, by=c("Common Name"="species"))
  snMatch <- dplyr::left_join(outputData, speciesToFlag, by=c("Species Name"="species"))

  outputData$flagSpecies <- outputData$flagSpecies | (snMatch$match == "yes")

  outputData$flagCommon <- (cnMatch$match == "yes")


  #Check abundance length & zero counts
  outputData$flagAbun <- str_length(outputData$Abundances) >=10 | outputData$Abundances == "0" | str_detect(outputData$Abundances,"-/")

  #Check comment length, for swearwords and e-mail addresses
  outputData$flagCom <- str_length(outputData$Comments) >= 150 | stringr::str_detect(tolower(outputData$Comments),paste(c(swearWords$word),collapse = "|")) | stringr::str_detect(outputData$Comments,"@")

  #Code to check house nums
  outputData <- check_house_numbers(outputData,locsToReplace,locsToIgnore)
  #Check for blank location name
  if (locCheck) {
    outputData$flagLoc <- is.na(outputData$`Location Name` == "") | outputData$flag1
  } else {

    outputData$flagLoc <- outputData$flag1
  }

  #Check grid ref
  outputData$flagGR <- is.na(outputData$`Grid Reference` =="") | is.na(str_length(outputData$`Grid Reference`) <=4) | str_length(outputData$`Grid Reference`) %% 2 | str_detect(outputData$`Grid Reference`, "[ .,-]") | !apply(sapply(c("NT","NU","NY","NZ"),grepl,str_sub(outputData$`Grid Reference`,1,2)),1,any)


  #Add dup check column


  #Return only the columns we want including the flags so we can highlight issues later
  OutputCols <- c("Recorder","Common Name","Species Name","Date","Grid Reference","Location Name","Abundances","Comments","Row No","flagRec","flagSpecies","flagAbun","flagCom","flagLoc","flagGR", "flagCommon")
  data_subset <- dplyr::select(outputData,dplyr::all_of(unlist(OutputCols)))
  return(data_subset)


}
