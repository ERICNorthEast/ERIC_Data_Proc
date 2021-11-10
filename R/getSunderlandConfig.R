#' Setup config to split data for the Sunderland SLA
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
#' list <- getSunderlandConfig(bat_sp,plant_sp)
getSunderlandConfig <- function(bat_sp,plant_sp){
  #Sunderland and South Tyneside are identical apart from sheet name

  Sun_ST_split <- setup_Sun_ST_split(bat_sp,plant_sp)
  Sun_ST_Cols <- setup_Sun_ST_cols()
  return (list("Source","Sunderland_All_data",Sun_ST_split,Sun_ST_Cols))

}
