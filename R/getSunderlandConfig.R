#' Setup config to split data for the Sunderland SLA
#'
#' @param bat_sp
#' @param plant_sp
#'
#' @return
#' @export
#'
#' @examples
getSunderlandConfig <- function(bat_sp,plant_sp){
  #Sunderland and South Tyneside are identical apart from sheet name

  Sun_ST_split <- setup_Sun_ST_split(bat_sp,plant_sp)
  Sun_ST_Cols <- setup_Sun_ST_cols()
  return (list("Source","Sunderland_All_data",Sun_ST_split,Sun_ST_Cols))

}
