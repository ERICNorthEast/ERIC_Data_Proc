#' Setup a list of config keys and values for checking input data
#'
#' @return list
#' @export
#'
#' @examples input_config <- setup_input_config_values()
setup_input_config_values <- function(){

  config_keys <- c("locationCol","abundanceCol","commentCol","recorderCol","commonCol","speciesCol","lastCol","dateCol","grCol","keyCol")

  locationCol <- 6
  abundanceCol <- 7
  commentCol <- 8
  recorderCol <- 1
  commonCol <- 2
  speciesCol <- 3
  lastCol <- 10
  dateCol <- 4
  colCount <- 15
  grCol <- 5
  keyCol <-10


  config_values <- list(locationCol,abundanceCol,commentCol,recorderCol,commonCol,speciesCol,lastCol,dateCol,grCol,keyCol)

  config_pairs <- list()                     # Create empty list
  for(i in 1:length(config_keys)) {              # Add key/value pairs in for-loop
    config_pairs[config_keys[i]] <- config_values[i]
  }


  return (config_pairs)
}
