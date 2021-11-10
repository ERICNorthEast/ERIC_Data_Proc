#' Split data to sheets within Excel workbook depending on the config specified
#'
#' @param SLA_split  config for the split rules
#' @param XL_wb  Excel workbook
#' @param SLA_data   data
#' @param allDataSheet name of the sheet to copy all data to
#' @param outputCols columns to output
#'
#' @return
#' @export
#'
#' @examples  split_data_to_sheets(SLA_split,XL_wb,SLA_data,allDataSheet,outputCols)
split_data_to_sheets <- function(SLA_split,XL_wb,SLA_data,allDataSheet,outputCols) {
  rowCount <- 0

  #AED where to put this?
  bold_style <- openxlsx::createStyle(textDecoration = "Bold")

  firstWrite <- TRUE
  for (j in 1:nrow(SLA_split))
  {

    if (dplyr::tally(SLA_data,eval(str2expression(unlist(SLA_split$FilterString[j]))))>0) {
      openxlsx::addWorksheet(XL_wb,SLA_split$SheetLabel[j])
      outputData<-dplyr::filter(SLA_data,eval(str2expression(unlist(SLA_split$FilterString[j]))))
      outputData <- dplyr::select(outputData,unlist(outputCols))
      openxlsx::writeData(XL_wb,SLA_split$SheetLabel[j],outputData,headerStyle = bold_style)

      if (allDataSheet != "") {
        outputData<-dplyr::filter(SLA_data,eval(str2expression(unlist(SLA_split$FilterString[j]))))
        outputData <- dplyr::select(outputData,unlist(outputCols))
        openxlsx::writeData(XL_wb,allDataSheet,outputData,headerStyle = bold_style, startRow = rowCount+1,colNames = firstWrite)

      }
      rowCount <- rowCount + dplyr::tally(SLA_data,eval(str2expression(unlist(SLA_split$FilterString[j]))))
      if (firstWrite) {rowCount=rowCount+1}
      firstWrite <- FALSE
    }

  }

}
