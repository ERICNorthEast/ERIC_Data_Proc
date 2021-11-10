#' Format data for output for an SLA and perform some checks
#'
#' @param raw_data  data to output
#' @param OutputCols  columns to output
#' @param newColNames lables of output columns
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


#' names(df) <- c('All.Design', 'Wildlife..', 'Wildlife_1', 'Taxon.grou', 'Taxon.Lati', 'Taxon.Comm', 'Obs.Commen', 'Sample.Rec', 'Sample.Loc', 'Sample.L_1', 'Sample.Dat', 'Sample.Spa', 'Survey.Run', 'Survey.Nam', 'Obs.Abunda', 'Determinat', 'Central_Ea', 'Central_No', 'Buffer', 'layer', 'path')
#' OutputCols <- c("All.Design","AllUKLeg", "Wildlife..","Wildlife_1","broadgroup", "Taxon.grou", "Taxon.Lati","Obs.Abunda", "Determinat",              "Obs.Commen","Sample.Rec","Sample.Loc",  "Sample.Dat","Sample.Spa",    "Central_eastings","Central_northings","Buffer","Precision","Survey.Run","Survey.Nam","NELBAP","Taxon.Comm","ImportDate","Info")
#' newColNames <- c("Designations","All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated","Wildlife & Countryside Act Sch 9 Part 2","Wildlife & Countryside Act Sch 9 Part 1","Broad Group","Taxon group","Latin Name","Abundances","Determination Type","Comments","Recorder","Location Name","Date",  "Grid Reference","Cent_East","Cent_North","Buffer","Precision",             "Survey Run By","Survey Name",                     "North East LBAP - Short Names","Common Name","Obs Entry Date","Additional Information") # Three empty cols
#'data <- format_and_check_SLA_data(df,OutputCols,newColNames)
format_and_check_SLA_data <- function(raw_data,OutputCols,newColNames) {

  # locsToReplace <- setup_locs_to_replace()
  # locsToIgnore <- setup_locs_to_ignore()
  # swearWords <- setup_profanity_config()
  # recordersToIgnore <- setup_recorders_to_ignore()


  #AED Not sure on ordering the data

  outputdata <- dplyr::distinct(raw_data[with(raw_data,order(raw_data$Taxon.grou,raw_data$Taxon.Lati)),] )

  outputdata <- dplyr::select(outputdata,dplyr::all_of(unlist(OutputCols)))
  #Rename cols
  colnames(outputdata) <- unlist(newColNames)
  outputdata <- check_house_numbers(outputdata,locsToReplace,locsToIgnore)

  #Check for 0 counts that we haven't already sorted
  outputdata$flag2 <- tolower(outputdata$Abundances) =="0 count"


  #Check for swear words
  outputdata$flag3 <- stringr::str_detect(tolower(outputdata$Comments),paste(c(swearWords$word),collapse = "|"))


  #Check for e-mail addresses
  outputdata$flag4 <- stringr::str_detect(outputdata$Comments,"@")
  outputdata$flag5 <- stringr::str_detect(outputdata$Recorder,"@") & is.na(match(outputdata$`Recorder`,table = recordersToIgnore$`Recorder`))

  return (outputdata)
}
