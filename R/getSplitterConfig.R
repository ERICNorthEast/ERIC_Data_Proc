#' Setup the config to split data depending on the local authority
#'
#' @param LA which local authority
#' @param SLA_Split_config config values
#'
#' @return
#' @export
#'
#' @examples
#'  config_keys <- c("bat_sp","plant_sp")
#'  bat_sp <- c("Chiroptera","Rhinolophidae","Vespertilionidae","Myotis","Barbastella","Eptesicus","Nyctalus","Pipistrellus","Plecotus","Vespertilio","Lasiurus","Tadarida","Hypsugo")
#'  plant_sp <- c("flowering plant" ,"clubmoss" ,"conifer" ,"fern" ,"fungus" ,"ginkgo" ,"horsetail" ,"lichen" ,"liverwort" ,"moss" ,"slime mould" ,"stonewort" ,"alga" ,"hornwort" ,"quillwort")
#'  config_values <- list(bat_sp,plant_sp)
#'  config_pairs <- list()                     # Create empty list
#'  for(i in 1:length(config_keys)) {              # Add key/value pairs in for-loop
#'    config_pairs[config_keys[i]] <- config_values[i]
#'  }
#' config <- getSplitterConfig(1,config_pairs)
getSplitterConfig <- function(LA,SLA_Split_config) {

  #Get species lists from config
  bat_sp <- SLA_Split_config["bat_sp"]
  plant_sp <- SLA_Split_config["plant_sp"]

  #Northumberland only
  if (LA==3) {
    pearl_mussel_select <- SLA_Split_config["pearl_mussel_select"]
    protected_mammals <- SLA_Split_config["protected_mammals"]
    non_inverts <- SLA_Split_config["non_inverts"]
    s41_mammals <- SLA_Split_config["s41_mammals"]
  }

  #Output columns for LAs apart from Sunderland and South Tyneside
  Other_LA_Cols <- c("Taxon group","Latin Name","Abundances","Determination Type","Comments","Recorder","Location Name","Date",
                     "Grid Reference","Cent_East","Cent_North","Buffer","Precision","Survey Run By","Survey Name","Common Name",
                     "Additional Information")

  #Setup the worksheet/criteria mapping depending on the LA
  switch(LA,getDurhamConfig(bat_sp,plant_sp,Other_LA_Cols),getGatesheadConfig(bat_sp,plant_sp,Other_LA_Cols),getNorthumberlandConfig(plant_sp,Other_LA_Cols,pearl_mussel_select,protected_mammals,non_inverts,s41_mammals),getSouthTynesideConfig(bat_sp,plant_sp),getSunderlandConfig(bat_sp,plant_sp))

}
