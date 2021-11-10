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

#'df <- data.frame(a,b,c,d,e,f,a,g,h,h,i,j,k,l,m,n,o,p,q,r,s,s,s,s,s,s)
#' names(df) <- c('Designations', 'All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated', 'Wildlife & Countryside Act Sch 9 Part 1', 'Wildlife & Countryside Act Sch 9 Part 2', 'Taxon group', 'Latin Name', 'Abundances', ',Determination Type','Comments', 'Recorder', 'Location Name', 'Date', 'Grid Reference','Cent_East', 'Cent_North',' Buffer',' Precision','Survey Run By', 'Survey Name', 'North East LBAP - Short Names', 'Common Name', 'England NERC S.41', 'Obs Changed Date', 'Obs Entry  Date', 'Obs Key', 'Additional Information'  )

#' outputCols <- c("Designations" , "Taxon group" , "Latin Name" , "Abundances" , "Determination Type", "Comments" , "Recorder" , "Location Name", "Date" , "Grid Reference", "Cent_East" , "Cent_North" , "Buffer" , "Precision" , "Survey Run By", "Survey Name" , "Common Name" , "Obs Changed Date", "Obs Entry Date", "Obs Key" , "Additional Information")
#'
#' SLA_split <- tibble::tribble(~SheetLabel, ~FilterString,'Reptiles_data','tolower(SLA_data$`Taxon group`)=="reptile"')
#' XL_wb <- openxlsx::createWorkbook()
#'
#' split_data_to_sheets(SLA_split,XL_wb,df,"All_data",outputCols)
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
