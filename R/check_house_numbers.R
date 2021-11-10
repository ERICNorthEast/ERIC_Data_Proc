#' Checks for house numbers in location name column. Deals with them as far as possible
#'
#' @param inputdata data to process
#' @param locsToReplace - tibble of locations to replace
#' @param locsToIgnore - tibble locations to ignore
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


#'names(df) <- c('All.Design', 'Wildlife..', 'Wildlife_1', 'Taxon.grou', 'Taxon.Lati', 'Taxon.Comm', 'Obs.Commen', 'Sample.Rec', 'Location Name', 'Sample.L_1', 'Sample.Dat', 'Sample.Spa', 'Survey.Run', 'Survey.Nam', 'Obs.Abunda', 'Determinat', 'Central_Ea', 'Central_No', 'Buffer', 'layer', 'path')
#' locsToIgnore <- tibble::tribble (~"Location name", "A189", "Sunderland 3")
#' locsToReplace <- tibble::tribble (~"Location Name", ~"Replacement", "- 37 Pretoria Avenue, Morpeth", "Pretoria Avenue, Morpeth", "1 Waynriggs Terrace","Waynriggs Terrace")
#' data <- check_house_numbers (df, locsToReplace, locsToIgnore)
#'
check_house_numbers <- function(inputdata, locsToReplace, locsToIgnore) {


  #Replace locations on our replace list
  inputdata <- dplyr::left_join(inputdata,locsToReplace)
  inputdata$`Location Name` <- ifelse(is.na(inputdata$Replacement),inputdata$`Location Name`,inputdata$Replacement)

  #Remove house numbers at start of string
  inputdata$`Location Name` <- stringr::str_remove(inputdata$`Location Name`,"^[0-9]{1,3}[,\\s]")

  #Check is what's left after removing numbers we're happy with
  check <- inputdata$`Location Name`
  for (r in ERICDataProc:::allowed_numbers) {
    check <- stringr::str_remove_all(check,r)
  }


  #Are there any numbers left to worry about?
  inputdata$flag1 <- stringr::str_detect(check,"[0-9]+") & is.na(match(inputdata$`Location Name`,table = locsToIgnore$`Location name`))
  return(inputdata)

}
