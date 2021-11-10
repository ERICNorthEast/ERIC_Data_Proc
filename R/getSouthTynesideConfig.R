#' Setup config to split data for the South Tyneside SLA (same as Sunderland)
#'
#' @param bat_sp List of bat species
#' @param plant_sp List of plant species
#'
#' @return
#' @export
#'
#' @examples list <- getSouthTynesideConfig(bat_sp,plant_sp)
getSouthTynesideConfig <- function(bat_sp,plant_sp){

  Sun_ST_split <- setup_Sun_ST_split(bat_sp,plant_sp)
  Sun_ST_Cols <- setup_Sun_ST_cols()
  return (list("Source","South_Tyneside_All_data",Sun_ST_split,Sun_ST_Cols))

}
