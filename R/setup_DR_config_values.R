#' Setup a list of config keys and values for data requests
#'
#' @return list
#' @export
#'
#' @examples DRconfig <- setup_DR_config_values()
setup_DR_config_values <- function(){

  config_keys <- c("locationCol","abundanceCol","commentCol","recorderCol","speciesCol","lastCol","dateCol","distanceCol","colCount","DROutputCols","DROutputColsWithDist","DRColNames","DRColNamesWithDist","FEP_DESIG","WNIO1_DESIG","WNIO2_DESIG","WNIO81_DESIG","WNIO82_DESIG","WNIO5_DESIG","WALES_DESIG","NORTH_DESIGS","NP_DESIGS","SCOT_DESIGS","TV_DESIGS","DURHAM_DESIGS")

  locationCol <- 9
  abundanceCol <- 5
  commentCol <- 7
  recorderCol <- 8
  speciesCol <- 3
  lastCol <- 14
  dateCol <- 10
  distanceCol <- 12
  colCount <- 15

  DROutputCols <- c("All.Design","Taxon.grou", "Taxon.Lati","Taxon.Comm", "Obs.Abunda", "Determinat","Obs.Commen","Sample.Rec",  "Sample.Loc",  "Sample.Dat","Sample.Spa","Survey.Run","Survey.Nam","Info")
  DROutputColsWithDist <- c("All.Design","Taxon.grou", "Taxon.Lati","Taxon.Comm", "Obs.Abunda", "Determinat","Obs.Commen","Sample.Rec",  "Sample.Loc",  "Sample.Dat","Sample.Spa","distance","Survey.Run","Survey.Nam","Info")
  DRColNames <- c("Designations","Taxon group","Latin Name","Common Name","Abundances","Determination Type","Comments","Recorder","Location Name","Date","Grid Reference","Survey Run By","Survey Name","Additional Information")
  DRColNamesWithDist <- c("Designations","Taxon group","Latin Name","Common Name","Abundances","Determination Type","Comments","Recorder","Location Name","Date","Grid Reference","Approx. Distance(m)","Survey Run By","Survey Name","Additional Information")


  FEP_DESIG <- "FEP-001"
  WNIO1_DESIG <- "W(NI)O-Sch1_part1"
  WNIO2_DESIG <- "W(NI)O-Sch1_part2"
  WNIO81_DESIG <- "W(NI)O-Sch8_part1"
  WNIO82_DESIG <- "W(NI)O-Sch8_part2"
  WNIO5_DESIG <- "W(NI)O-Sch5"
  WALES_DESIG <- "Wales_NERC_S.42"

  NORTH_DESIGS <- 'pandn_data$All.Design== "Northumberland BAP" |  pandn_data$All.Design=="FEP-001, Northumberland NP BAP"'
  NP_DESIGS <- 'pandn_data$All.Design=="FEP-001, Northumberland NP BAP"'
  SCOT_DESIGS <- 'pandn_data$All.Design== "Scottish_Biodiversity_List" | pandn_data$All.Design== "Scottish_Biodiversity_List, W(NI)O-Sch1_part1" |pandn_data$All.Design== "Scottish_Biodiversity_List, Wales_NERC_S.42" |pandn_data$All.Design== "FEP-001, Scottish_Biodiversity_List"'
  TV_DESIGS <- 'pandn_data$All.Design=="Tees Valley BAP" | pandn_data$All.Design=="Tees Valley BAP, " | pandn_data$All.Design=="Tees Valley BAP, FEP-001" | pandn_data$All.Design=="Tees Valley BAP, Tees Valley BAP" | pandn_data$All.Design=="Tees Valley BAP, Tees Valley BAP, W(NI)O-Sch8_part1, Wales_NERC_S.42" | pandn_data$All.Design=="Tees Valley BAP, W(NI)O-Sch8_part1, Wales_NERC_S.42"'
  DURHAM_DESIGS <- 'pandn_data$All.Design=="Durham BAP"'

  config_values <- list(locationCol,abundanceCol,commentCol,recorderCol,speciesCol,lastCol,dateCol,distanceCol,colCount, DROutputCols,DROutputColsWithDist,DRColNames,DRColNamesWithDist,FEP_DESIG,WNIO1_DESIG,WNIO2_DESIG,WNIO81_DESIG,WNIO82_DESIG,WNIO5_DESIG,WALES_DESIG,    NORTH_DESIGS,NP_DESIGS,SCOT_DESIGS,TV_DESIGS,DURHAM_DESIGS)

  config_pairs <- list()                     # Create empty list
  for(i in 1:length(config_keys)) {              # Add key/value pairs in for-loop
    config_pairs[config_keys[i]] <- config_values[i]
  }


  return (config_pairs)
}
