#' Setup config to split data for the Tees Valley SLA
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
#' list <- getTeesValleyConfig(bat_sp,plant_sp)
getTeesValleyConfig <- function(bat_sp,plant_sp){
  #Sunderland, South Tyneside and Tees Valley are identical apart from sheet name

  Sun_TV_split <- setup_TV_split(bat_sp,plant_sp)
  Sun_TV_Cols <- setup_Sun_ST_cols()
  return (list("Source","Teesvalley_All_data",Sun_TV_split,Sun_TV_Cols))

}
