#' Setup confid values for EA SLAs
#'
#' @return
#' @export
#'
#' @examples config <- setup_EA_SLA_config_values()
setup_EA_SLA_config_values <- function(){
  config_keys <- c("locationCol", "abundanceCol", "commentCol", "lastCol", "dateCol", "EAOutputCols", "TempColNames")


  locationCol <- 4
  abundanceCol <- 3
  commentCol <- 16
  lastCol <- 18
  dateCol <- 5



  EAOutputCols <- c("Common_nam","Scientific","Survey_obs","Location","Survey_sta","Survey_end","Survey_typ","File_code","Data_provi","Data_owner","Supplier","Source","Capture_GR","Data_restr","Status","Other","OSGB_E","OSGB_N")
  TempColNames <- c("Common_nam","Scientific","Abundances","Location Name","Survey_sta","Survey_end","Survey_typ","File_code","Data_provi","Data_owner","Supplier","Source","Capture_GR","Data_restr","Status","Comments","OSGB_E","OSGB_N")


  config_values <- list(locationCol, abundanceCol, commentCol, lastCol, dateCol,   EAOutputCols, TempColNames)

  config_pairs <- list()                     # Create empty list
  for(i in 1:length(config_keys)) {              # Add key/value pairs in for-loop
    config_pairs[config_keys[i]] <- config_values[i]
  }


  return (config_pairs)
}
