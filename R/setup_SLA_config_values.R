#' Setup confid values for SLAs
#'
#' @return
#' @export
#'
#' @examples config <- setup_SLA_config_values()
setup_SLA_config_values <- function(){
  config_keys <- c("locationCol", "abundanceCol", "commentCol", "recorderCol", "lastCol", "dateCol", "LAlocationCol", "LAabundanceCol", "LAcommentCol", "LArecorderCol", "LAlastCol", "SLAOutputCols", "SLAColNames", "LAOutputCols", "LAColNames")


  #AD do we need all of these?
  locationCol <- 12
  abundanceCol <- 8
  commentCol <- 10
  recorderCol <- 11
  lastCol <- 24
  dateCol <- 13

  #LA cols
  LAlocationCol <- 11
  LAabundanceCol <- 7
  LAcommentCol <- 9
  LArecorderCol <- 10
  LAlastCol <- 26

  SLAOutputCols <- c("All.Design","AllUKLeg", "Wildlife..","Wildlife_1","broadgroup", "Taxon.grou", "Taxon.Lati","Obs.Abunda", "Determinat",              "Obs.Commen","Sample.Rec","Sample.Loc",  "Sample.Dat","Sample.Spa",    "Central_eastings","Central_northings","Buffer","Precision","Survey.Run","Survey.Nam","NELBAP","Taxon.Comm","ImportDate","Info")
  SLAColNames <- c("Designations","All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated","Wildlife & Countryside Act Sch 9 Part 2","Wildlife & Countryside Act Sch 9 Part 1","Broad Group","Taxon group","Latin Name","Abundances","Determination Type","Comments","Recorder","Location Name","Date",  "Grid Reference","Cent_East","Cent_North","Buffer","Precision",             "Survey Run By","Survey Name",                     "North East LBAP - Short Names","Common Name","Obs Entry Date","Additional Information") # Three empty cols
  LAOutputCols <- c("All.Design","AllUKLeg", "Wildlife..","Wildlife_1", "Taxon.grou", "Taxon.Lati","Obs.Abunda", "Determinat",   "Obs.Commen","Sample.Rec","Sample.Loc",  "Sample.Dat","Sample.Spa",    "Central_eastings","Central_northings","Buffer","Precision","Survey.Run","Survey.Nam","NELBAP","Taxon.Comm","Empty1","Empty2","ImportDate","Empty3","Info")
  LAColNames <- c("Designations","All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated","Wildlife & Countryside Act Sch 9 Part 2","Wildlife & Countryside Act Sch 9 Part 1","Taxon group","Latin Name","Abundances","Determination Type","Comments","Recorder","Location Name","Date",  "Grid Reference","Cent_East","Cent_North","Buffer","Precision",  "Survey Run By","Survey Name",          "North East LBAP - Short Names","Common Name","England NERC S.41","Obs Changed Date","Obs Entry Date","Obs Key","Additional Information") # Three empty cols

  config_values <- list(locationCol, abundanceCol, commentCol, recorderCol, lastCol, dateCol,  LAlocationCol, LAabundanceCol, LAcommentCol, LArecorderCol, LAlastCol,  SLAOutputCols, SLAColNames, LAOutputCols, LAColNames)

  config_pairs <- list()                     # Create empty list
  for(i in 1:length(config_keys)) {              # Add key/value pairs in for-loop
    config_pairs[config_keys[i]] <- config_values[i]
  }


  return (config_pairs)
}
