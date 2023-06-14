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
#'
#'df <- data.frame(a,a,b,c,d,d,e,f,a,g,h,h,i,j,k,l,m,n,o,p,q,q,r,s,b,b,b)

#'names(df) <- c('All.Design','AllUKLeg', 'Wildlife..', 'Wildlife_1', 'broad', 'Taxon.grou', 'Taxon.Lati', 'Taxon.Comm', 'Obs.Commen','Sample.Rec', 'Sample.Loc', 'Sample.L_1', 'Sample.Dat', 'Sample.Spa', 'Survey.Run', 'Survey.Nam', 'Obs.Abunda', 'Determinat', 'Central_eastings', 'Central_northings', 'Buffer','Precision', 'layer', 'path','NELBAP', 'ImportDate', 'Info')
#'OutputCols <- c("All.Design","AllUKLeg", "Wildlife..","Wildlife_1", "broad", "Taxon.grou", "Taxon.Lati","Obs.Abunda", "Determinat", "Obs.Commen","Sample.Rec","Sample.Loc",  "Sample.Dat","Sample.Spa",    "Central_eastings","Central_northings","Buffer","Precision","Survey.Run","Survey.Nam","NELBAP","Taxon.Comm","ImportDate","Info")
#'newColNames <- c("Designations","All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated","Wildlife & Countryside Act Sch 9 Part 2","Wildlife & Countryside Act Sch 9 Part 1","Broad Group","Taxon group","Latin Name","Abundances","Determination Type","Comments","Recorder","Location Name","Date",  "Grid Reference","Cent_East","Cent_North","Buffer","Precision",             "Survey Run By","Survey Name",                     "North East LBAP - Short Names","Common Name","Obs Entry Date","Additional Information") # Three empty cols
#'data <- format_and_check_SLA_data(df,OutputCols,newColNames)
format_and_check_EA_SLA_data <- function(raw_data,EAOutputCols,newColNames) {

  locsToReplace <- setup_locs_to_replace()
  locsToIgnore <- setup_locs_to_ignore()
  #swearWords <- setup_profanity_config()

  outputdata <- dplyr::select(raw_data,dplyr::all_of(unlist(EAOutputCols)))

  #Rename columns so we can reuse some functions
  colnames(outputdata) <- unlist(newColNames)

  #Add Vitality column - hard-coded for 2023
  outputdata$vitality <-"not recorded"

  outputdata <- check_house_numbers(outputdata,locsToReplace,locsToIgnore)

  #Check for 0 counts that we haven't already sorted
  #outputdata$flag2 <- tolower(outputdata$Abundances) =="0 count"


  #Check for swear words
  #outputdata$flag3 <- stringr::str_detect(tolower(outputdata$Comments),paste(c(swearWords$word),collapse = "|"))


  #Check for e-mail addresses
  outputdata$flag4 <- stringr::str_detect(outputdata$recordedBy,"@")
  outputdata$flag5 <- stringr::str_detect(outputdata$identifiedBy,"@")


  #Rename cols back to EA format

  #newEACols <- append(unlist(EAOutputCols),c("Replacement","flag1","flag2","flag3","flag4"))
  newEACols <- append(unlist(EAOutputCols),c("vitality","Replacement","flag1","flag4","flag5"))
  colnames(outputdata) <- unlist(newEACols)

  return (outputdata)
}
