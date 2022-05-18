#' Format the data for input and perform some checks
#'
#' 'Use this function to perform checks on incoming data
#' Flag whether iRecord, ERIC, standard data, NTBC, and
#'
#' @param raw_data Set of species data
#' @param locCheck whether to check for blank locations
#' @param inputformat the source of the data, e.g. iRecord
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

format_and_check_input_data <- function(raw_data,locCheck,inputformat) {

  locsToReplace <- setup_locs_to_replace()
  locsToIgnore <- setup_locs_to_ignore()
  swearWords <- setup_profanity_config()
  species <- setup_species_config()
  recordersToIgnore <- setup_recorders_to_ignore()

  #FORMATTING

  #If dataSource is iRecord
  if (inputformat == "irecord") {
  # Add verifier to observer
    raw_data["Verifier"][is.na(raw_data["Verifier"])] <- ""
    raw_data$Recorder<-paste(raw_data$Recorder,raw_data$Verifier,sep=", ")

  #Replace "present" in Abundances
    raw_data["Abundances"][(raw_data["Abundances"])=="Present"] <- ""

  # Build comment field

    raw_data["Sample method"][(raw_data["Sample method"])=="Unknown"] <- ""
    raw_data["Sample method"][is.na(raw_data["Sample method"])] <- ""
    raw_data["Sex"][(raw_data["Sex"])=="not recorded"] <- ""
    raw_data["Sex"][is.na(raw_data["Sex"])] <- ""
    raw_data["Stage"][(raw_data["Sex"])=="not recorded"] <- ""
    raw_data["Stage"][is.na(raw_data["Sex"])] <- ""
    raw_data["Biotope"][is.na(raw_data["Biotope"])] <- ""
    raw_data$Biotope <- ifelse(raw_data$Biotope=="","",paste("Biotope:",raw_data$Biotope))

    raw_data$Comments<-paste(raw_data$Comments, raw_data$`Sample comment`,raw_data$Biotope,raw_data$`Sample method`,raw_data$Sex,raw_data$Stage)

  }
  #If dataSource is ERIC website
  if (inputformat == "eric") {
    #Replace "present" in Abundances
    raw_data["Abundances"][(raw_data["Abundances"])=="Present"] <- ""

    #Build comment field
    raw_data["Sample method"][(raw_data["Sample method"])=="Unknown"] <- ""
    raw_data["Sample method"][is.na(raw_data["Sample method"])] <- ""
    raw_data["Sex"][(raw_data["Sex"])=="not recorded"] <- ""
    raw_data["Sex"][is.na(raw_data["Sex"])] <- ""
    raw_data["Stage"][(raw_data["Sex"])=="not recorded"] <- ""
    raw_data["Stage"][is.na(raw_data["Sex"])] <- ""
    raw_data["Biotope"][is.na(raw_data["Biotope"])] <- ""
    raw_data$Biotope <- ifelse(raw_data$Biotope=="","",paste("Biotope:",raw_data$Biotope))

  }

  #If dataSource is BirdTrack
  #Setup a column for the observer name - ask for it
  #Remove any "present" values in abundance column
  #Build comment field
  #Get the grid ref from one of two columns

  #If datasource is standard no special formatting




  #Add row number column
  rowNo <- seq_len(nrow(raw_data))
  outputData<-cbind(raw_data,rowNo)
  names(outputData)[names(outputData) == 'rowNo'] <- 'Row No'


  #Check recorder for blanks, email addresses but ignore allowed values
  outputData$flagRec <- is.na(outputData$Recorder == "") | is.na(outputData$Recorder) | !str_detect(outputData$Recorder," ") | stringr::str_detect(outputData$Recorder,"@") & is.na(match(outputData$`Recorder`,table = recordersToIgnore$`Recorder`))

  #Check species
  outputData$flagSpecies <- is.na(outputData$`Common Name`== "" & outputData$`Species Name` == "")

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


  #Return only the columns we want
  OutputCols <- c("Recorder","Common Name","Species Name","Date","Grid Reference","Location Name","Abundances","Comments","Row No")
  data_subset <- dplyr::select(outputData,dplyr::all_of(unlist(OutputCols)))
  return(data_subset)


}
