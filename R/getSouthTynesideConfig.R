#' Setup config to split data for the South Tyneside SLA (same as Sunderland)
#'
#' @param bat_sp List of bat species
#' @param plant_sp List of plant species
#'
#' @return
#' @export
#'
#' @examples
#' bat_sp <- c("Chiroptera","Rhinolophidae","Vespertilionidae")
#' plant_sp <- c("flowering plant" ,"clubmoss" ,"conifer")
#' Other_LA_Cols <- c("Taxon group","Latin Name","Abundances","Determination Type")
#' list <- getSouthTynesideConfig(bat_sp,plant_sp)
getSouthTynesideConfig <- function(bat_sp,plant_sp){

  Sun_ST_split <- setup_Sun_ST_split(bat_sp,plant_sp)
  Sun_ST_Cols <- setup_Sun_ST_cols()
  return (list("Source","South_Tyneside_All_data",Sun_ST_split,Sun_ST_Cols))

}
