#' Delete ten columns from the given column in the Excel worksheet
#'
#' @param XL_wb
#' @param sheet_number
#' @param delCol  start column for deletion
#' @param delRow  last row to delete
#'
#' @return
#' @export
#'
#' @examples
delete_temp_XL_cols <- function(XL_wb,sheet_number,delCol, delRow) {
  startDel <- delCol
  endDel <- startDel+10

  openxlsx::deleteData(XL_wb, sheet = sheet_number,cols=startDel:endDel, rows=0:delRow, gridExpand = TRUE)


}
