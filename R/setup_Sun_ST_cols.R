#' Setup column names for Sunderland and South Tyneside SLAs
#'
#' @return
#' @export
#'
#' @examples  cols <- setup_Sun_ST_cols()
setup_Sun_ST_cols <- function(){
  Sun_ST_Cols <- c("Designations","Taxon group","Latin Name","Abundances","Determination Type","Comments","Recorder","Location Name","Date",
                   "Grid Reference","Cent_East","Cent_North","Buffer","Precision","Survey Run By","Survey Name","Common Name","Obs Changed Date","Obs Entry Date",
                   "Obs Key","Additional Information")

  return(Sun_ST_Cols)
}
