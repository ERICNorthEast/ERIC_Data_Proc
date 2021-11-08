#' Setup config to split data for the South Tyneside SLA (same as Sunderland)
#'
#' @param bat_sp
#' @param plant_sp
#'
#' @return
#' @export
#'
#' @examples
getSouthTynesideConfig <- function(bat_sp,plant_sp){

  Sun_ST_split <- setup_Sun_ST_split(bat_sp,plant_sp)
  Sun_ST_Cols <- setup_Sun_ST_cols()
  return (list("Source","South_Tyneside_All_data",Sun_ST_split,Sun_ST_Cols))

}
