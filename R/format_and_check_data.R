#' Format the data for output and perform some checks
#'
#' @param raw_data Set of species data
#' @param OutputCols List of the columns to be output
#' @param newColNames Labels for output columns
#' @param sensitivecheck Highlight sensitive species or not
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
format_and_check_data <- function(raw_data,OutputCols,newColNames, sensitivecheck) {

  locsToReplace <- setup_locs_to_replace()
  locsToIgnore <- setup_locs_to_ignore()
  swearWords <- setup_profanity_config()
  recordersToIgnore <- setup_recorders_to_ignore()

  #Get the columns we're going to output  sort by taxon group & latin name & discard duplicates
  data_subset <- dplyr::select(raw_data,dplyr::all_of(unlist(OutputCols)))
  outputdata <- dplyr::distinct(data_subset[with(data_subset,order(data_subset$Taxon.grou,data_subset$Taxon.Lati)),] )

  # outputdata <- dplyr::distinct(raw_data[with(raw_data,order(raw_data$Taxon.grou,raw_data$Taxon.Lati)),] )
  #
  # outputdata <- dplyr::select(outputdata,dplyr::all_of(unlist(OutputCols)))
  colnames(outputdata) <- unlist(newColNames)


  #Code to check house nums


  outputdata <- check_house_numbers(outputdata,locsToReplace,locsToIgnore)

  #Check for 0 counts that we haven't already sorted
  outputdata$flag2 <- tolower(outputdata$Abundances) =="0 count"


  #Check for swear words
  outputdata$flag3 <- stringr::str_detect(tolower(outputdata$Comments),paste(c(swearWords$word),collapse = "|"))


  #Check for e-mail addresses
  outputdata$flag4 <- stringr::str_detect(outputdata$Comments,"@")
  outputdata$flag5 <- stringr::str_detect(outputdata$Recorder,"@") & is.na(match(outputdata$`Recorder`,table = recordersToIgnore$`Recorder`))

  #Sensitive species check
  if (sensitivecheck) {

    sens_config <- setup_sensitive_config()
    sensitive_species <- sens_config["sensitive_species"]
    sensitive_desigs <- sens_config["sensitive_desigs"]
    outputdata$flag6 <-stringr::str_detect(outputdata$'Latin Name',paste(unlist(sensitive_species),collapse = '|')) | stringr::str_detect(outputdata$Designations, paste(unlist(sensitive_desigs),collapse = '|'))
  }

  return(outputdata)
}
