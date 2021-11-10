#' Format output data for a data request and output to an Excel spreadsheet
#'
#' @param XL_wb Excel workbook
#' @param sheet_name Label for Excel sheet
#' @param outputdata data set to be output
#' @param sensitivecheck Should sensitive species be highlighted?
#' @param del_dist Should the output be filtered by distance
#' @param DR_config list of config values
#' @param sheet_number Excel sheet number
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
#'t <- c(1567,890)

#'df <- data.frame(a,b,c,d,e,f,a,g,h,h,i,j,k,l,m,n,o,p,q,r,s,t)


#'names(df) <- c('All.Design', 'Wildlife..', 'Wildlife_1', 'Taxon.grou', 'Taxon.Lati', 'Taxon.Comm', 'Obs.Commen', 'Sample.Rec', 'Sample.Loc', 'Sample.L_1', 'Sample.Dat', 'Sample.Spa', 'Survey.Run', 'Survey.Nam', 'Obs.Abunda', 'Determinat', 'Central_Ea', 'Central_No', 'Buffer', 'layer', 'path','distance')
#' XL_wb <- openxlsx::createWorkbook()
#' config_keys <- c("locationCol","abundanceCol","commentCol","recorderCol","speciesCol","lastCol","dateCol","distanceCol","colCount")
#' locationCol <- 9
#' abundanceCol <- 5
#' commentCol <- 7
#' recorderCol <- 8
#' speciesCol <- 3
#' lastCol <- 14
#' dateCol <- 10
#' distanceCol <- 12
#' colCount <- 15
#'
#' config_values <- list(locationCol,abundanceCol,commentCol,recorderCol,speciesCol,lastCol,dateCol,distanceCol,colCount)

#'config_pairs <- list()                     # Create empty list
#'for(i in 1:length(config_keys)) {              # Add key/value pairs in for-loop
#'  config_pairs[config_keys[i]] <- config_values[i]
#'}


#'  XL_wb <- format_DR_Excel_output(XL_wb,"Data sheet",df, 1,1,config_pairs,1)
format_DR_Excel_output <- function(XL_wb,sheet_name, outputdata,sensitivecheck,del_dist,DR_config,sheet_number) {


  bold_style <- openxlsx::createStyle(textDecoration = "Bold")
  highlightStyle <- openxlsx::createStyle(fgFill="#FFFF00")
  highlightBoldStyle <- openxlsx::createStyle(fgFill="#FFFF00",textDecoration = "Bold")
  sensitiveStyle <- openxlsx::createStyle(fgFill="#CCFFFF")

  dateStyle <- openxlsx::createStyle(numFmt="DATE")
  #numberStyle <- openxlsx::createStyle(numFmt="NUMBER")
  numberStyle <- openxlsx::createStyle(numFmt="0")

  locationCol <- unlist(DR_config["locationCol"])
  abundanceCol<-unlist(DR_config["abundanceCol"])
  commentCol<-unlist(DR_config["commentCol"])
  speciesCol<-unlist(DR_config["speciesCol"])
  recorderCol<-unlist(DR_config["recorderCol"])
  dateCol<-unlist(DR_config["dateCol"])
  distanceCol<-unlist(DR_config["distanceCol"])
  lastCol<-unlist(DR_config["lastCol"])

  openxlsx::addWorksheet(XL_wb,sheet_name)
  openxlsx::writeData(XL_wb,sheet_name,outputdata,headerStyle = bold_style)

  #Highlight the cells we've flagged

  openxlsx::addStyle(XL_wb,sheet_name,cols=locationCol,rows=which(outputdata$flag1 == TRUE)+1,style = highlightStyle)
  openxlsx::addStyle(XL_wb,sheet_name,cols=abundanceCol,rows=which(outputdata$flag2 == TRUE)+1,style = highlightStyle)
  openxlsx::addStyle(XL_wb,sheet_name,cols=commentCol,rows=which(outputdata$flag3 == TRUE)+1,style = highlightStyle)
  openxlsx::addStyle(XL_wb,sheet_name,cols=commentCol,rows=which(outputdata$flag4 == TRUE)+1,style = highlightStyle)
  openxlsx::addStyle(XL_wb,sheet_name,cols=recorderCol,rows=which(outputdata$flag5 == TRUE)+1,style = highlightStyle)
  if (sensitivecheck) {
    openxlsx::addStyle(XL_wb,sheet_name,cols=speciesCol,rows=which(outputdata$flag6 == TRUE)+1,style = sensitiveStyle)
  }

  if (sum(outputdata$flag1,na.rm=TRUE)>0) {openxlsx::addStyle(XL_wb,sheet_name,cols=locationCol,rows=1,style = highlightBoldStyle)}
  if (sum(outputdata$flag2,na.rm=TRUE)>0) {openxlsx::addStyle(XL_wb,sheet_name,cols=abundanceCol,rows=1,style = highlightBoldStyle)}
  if (sum(outputdata$flag3,na.rm=TRUE)+sum(outputdata$flag4,na.rm=TRUE)>0 ) {openxlsx::addStyle(XL_wb,sheet_name,cols=commentCol,rows=1,style = highlightBoldStyle)}
  if (sum(outputdata$flag5,na.rm=TRUE)>0) {openxlsx::addStyle(XL_wb,sheet_name,cols=recorderCol,rows=1,style = highlightBoldStyle)}

  #Format date and distance columns
  openxlsx::addStyle(XL_wb,sheet_name,cols=dateCol,rows=2:nrow(outputdata), style=dateStyle)
  if (del_dist == 1) {
    openxlsx::addStyle(XL_wb,sheet_name,cols=distanceCol,rows=2:nrow(outputdata), style=numberStyle)
  }

  openxlsx::setColWidths(XL_wb,sheet_name,cols=1:lastCol,widths = 13.4)

  openxlsx::addFilter(XL_wb,sheet_name, rows= 1, cols=1:lastCol)

  if (del_dist) {lastCol <- lastCol+1}
  delete_temp_XL_cols(XL_wb,sheet_number,lastCol+1,nrow(outputdata)+1)

  return (XL_wb)
}
