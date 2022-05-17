#' Format the data for input and perform some checks
#'
#' 'Use this function to perform checks on incoming data
#' Flag whether iRecord, ERIC, standard data, NTBC, and
#'
#' @param raw_data Set of species data
#' @param OutputCols List of the columns to be output
#' @param newColNames Labels for output columns
#' @param dataSource Where has the data originated
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
#format_and_check_input_data <- function(raw_data,OutputCols,newColNames, dataSource)
format_and_check_input_data <- function(raw_data) {

  locsToReplace <- setup_locs_to_replace()
  locsToIgnore <- setup_locs_to_ignore()
  swearWords <- setup_profanity_config()
  species <- setup_species_config()
  recordersToIgnore <- setup_recorders_to_ignore()

  #Get the columns we're going to output  sort by taxon group & latin name & discard duplicates
  # data_subset <- dplyr::select(raw_data,dplyr::all_of(unlist(OutputCols)))
  #
  newColNames <- c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments")
  outputData <- raw_data
  colnames(outputData) <- unlist(newColNames)

  #FORMATTING

  #If dataSource is iRecord
  # Add verifier to observer
  # Build comment field

  #If dataSource is ERIC website
  #Build comment field

  #If dataSource is BirdTrack
  #Setup a column for the observer name - ask for it
  #Remove any "present" values in abundance column
  #Build comment field
  #Get the grid ref from one of two columns

  #If datasource is standard no special formatting


  #outputData <- dplyr::distinct(data_subset[with(data_subset,order(data_subset$`Taxon group`,data_subset$`Latin Name`)),])


  #Check grid refs

  #Check recorder
  outputData$flag7 <- is.na(outputData$Recorder == "") | is.na(outputData$Recorder) | !str_detect(outputData$Recorder," ")

  #Check species
  outputData$flag6 <- is.na(outputData$`Common Name`== "" & outputData$`Species Name` == "")

  #Check for blank location name
  outputData$flag1 <- is.na(outputData$`Location Name` == "")

  #Check abundance length
  outputData$flag2 <- is.na(str_length(outputData$Abundances) >=10)

  #Check comment length
  outputData$flag4 <- is.na(str_length(outputData$Comments) >= 150)

  #Code to check house nums
  outputData <- check_house_numbers(outputData,locsToReplace,locsToIgnore)

  #Check for 0 counts that we haven't already sorted
  outputData$flag2 <- outputData$Abundances =="0"


  #Check for swear words
  outputData$flag3 <- stringr::str_detect(tolower(outputData$Comments),paste(c(swearWords$word),collapse = "|"))


  #Check for e-mail addresses
  outputData$flag4 <- stringr::str_detect(outputData$Comments,"@")
  outputData$flag5 <- stringr::str_detect(outputData$Recorder,"@") & is.na(match(outputData$`Recorder`,table = recordersToIgnore$`Recorder`))


  #Add dup check column

  return(outputData)
}
