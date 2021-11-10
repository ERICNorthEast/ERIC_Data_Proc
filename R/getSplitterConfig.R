#' Setup the config to split data depending on the local authority
#'
#' @param LA which local authority
#'
#' @return
#' @export
#'
#' @examples getSplitterConfig(1)
getSplitterConfig <- function(LA,SLA_Split_config) {
  bat_sp <- SLA_Split_config["bat_sp"]
  plant_sp <- SLA_Split_config["plant_sp"]

  Other_LA_Cols <- c("Taxon group","Latin Name","Abundances","Determination Type","Comments","Recorder","Location Name","Date",
                     "Grid Reference","Cent_East","Cent_North","Buffer","Precision","Survey Run By","Survey Name","Common Name",
                     "Additional Information")
  switch(LA,getDurhamConfig(bat_sp,plant_sp,Other_LA_Cols),getGatesheadConfig(bat_sp,plant_sp,Other_LA_Cols),getNorthumberlandConfig(plant_sp,Other_LA_Cols,SLA_split_config),getSouthTynesideConfig(bat_sp,plant_sp),getSunderlandConfig(bat_sp,plant_sp))

}
