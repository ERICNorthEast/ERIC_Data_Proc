#' Delete ten columns from the given column in the Excel worksheet
#'
#' @param XL_wb  Excel workbook
#' @param sheet_number Number of worksheet
#' @param delCol  start column for deletion
#' @param delRow  last row to delete
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
#' XL_wb <- openxlsx::createWorkbook()
#' openxlsx::addWorksheet(XL_wb,'Test data')
#'openxlsx::writeData(XL_wb,'Test data',df)
#' delete_temp_XL_cols(XL_wb,1,10, 3)
delete_temp_XL_cols <- function(XL_wb,sheet_number,delCol, delRow) {
  startDel <- delCol
  endDel <- startDel+10

  openxlsx::deleteData(XL_wb, sheet = sheet_number,cols=startDel:endDel, rows=0:delRow, gridExpand = TRUE)


}
