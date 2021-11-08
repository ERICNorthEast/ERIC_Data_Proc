#' format_SLA_Excel_output
#'
#' @param XL_wb Excel workbook
#' @param sheet_name name to give the worksheet
#' @param outputdata data to output
#' @param SLA_config list of config values
#' @param SLA_type whether it's a local authority SLA or NNPA/AONB
#'
#' @return
#' @export
#'
#' @examples
format_SLA_Excel_output <- function(XL_wb, sheet_name,outputdata,SLA_config,SLA_type) {

  bold_style <- openxlsx::createStyle(textDecoration = "Bold")
  highlightStyle <- openxlsx::createStyle(fgFill="#FFFF00")
  highlightBoldStyle <- openxlsx::createStyle(fgFill="#FFFF00",textDecoration = "Bold")

  dateStyle <- openxlsx::createStyle(numFmt="DATE")
  numberStyle <- openxlsx::createStyle(numFmt="0")

  if (SLA_type==1) {
    locationCol <- unlist(SLA_config["LAlocationCol"])
    abundanceCol <- unlist(SLA_config["LAabundanceCol"])
    commentCol <- unlist(SLA_config["LAcommentCol"])
    recorderCol <- unlist(SLA_config["LArecorderCol"])
    dateCol <- unlist(SLA_config["LAdateCol"])
    lastCol <- unlist(SLA_config["LAlastCol"])
  } else
  {
  locationCol <- unlist(SLA_config["locationCol"])
  abundanceCol <- unlist(SLA_config["abundanceCol"])
  commentCol <- unlist(SLA_config["commentCol"])
  recorderCol <- unlist(SLA_config["recorderCol"])
  dateCol <- unlist(SLA_config["dateCol"])
  lastCol <- unlist(SLA_config["lastCol"])
}

  openxlsx::addWorksheet(XL_wb,sheet_name)
  openxlsx::writeData(XL_wb,sheet_name,outputdata,headerStyle = bold_style)

  #Highlight the cells we've flagged
  openxlsx::addStyle(XL_wb,sheet_name,cols=locationCol,rows=which(outputdata$flag1 == TRUE)+1,style = highlightStyle)
  openxlsx::addStyle(XL_wb,sheet_name,cols=abundanceCol,rows=which(outputdata$flag2 == TRUE)+1,style = highlightStyle)
  openxlsx::addStyle(XL_wb,sheet_name,cols=commentCol,rows=which(outputdata$flag3 == TRUE)+1,style = highlightStyle)
  openxlsx::addStyle(XL_wb,sheet_name,cols=commentCol,rows=which(outputdata$flag4 == TRUE)+1,style = highlightStyle)
  openxlsx::addStyle(XL_wb,sheet_name,cols=recorderCol,rows=which(outputdata$flag5 == TRUE)+1,style = highlightStyle)

  if (sum(outputdata$flag1,na.rm=TRUE)>0) {openxlsx::addStyle(XL_wb,sheet_name,cols=locationCol,rows=1,style = highlightStyle)}
  if (sum(outputdata$flag2,na.rm=TRUE)>0) {openxlsx::addStyle(XL_wb,sheet_name,cols=abundanceCol,rows=1,style = highlightStyle)}
  if (sum(outputdata$flag3,na.rm=TRUE)+sum(outputdata$flag4,na.rm=TRUE)>0 ) {openxlsx::addStyle(XL_wb,sheet_name,cols=commentCol,rows=1,style = highlightStyle)}
  if (sum(outputdata$flag5,na.rm=TRUE)>0) {openxlsx::addStyle(XL_wb,sheet_name,cols=recorderCol,rows=1,style = highlightStyle)}

  #Format the date column
  openxlsx::addStyle(XL_wb,sheet_name,cols=dateCol,rows=2:nrow(outputdata), style=dateStyle)
  openxlsx::setColWidths(XL_wb,1,cols=1:lastCol,widths=13.4)

  openxlsx::addFilter(XL_wb,1,rows=1, cols = 1:lastCol)

  #Delete the temp data
  delete_temp_XL_cols(XL_wb,sheet_number = 1,lastCol+1,nrow(outputdata)+1)

  return (XL_wb)

}
