#' Setup list of config keys and values for specific surveys
#'
#' @return list
#' @export
#'
#' @examples config <- setup_survey_config_values()
setup_survey_config_values <- function(){

  config_keys <- c("RIVER_JELLY","WCC_2005","BRISTLE_MOSS","ELM_LICHEN")

  RIVER_JELLY <- "River Jelly Lichen 2005"
  WCC_2005 <- "White-Clawed Crayfish 2005"
  BRISTLE_MOSS <- "Pale Bristle-Moss & Spruce\\'s Bristle-Moss 2006"
  ELM_LICHEN <- "Orange-Fruited Elm-Lichen 2005"


  config_values <- list(RIVER_JELLY,WCC_2005,BRISTLE_MOSS,ELM_LICHEN)

  config_pairs <- list()                     # Create empty list
  for(i in 1:length(config_keys)) {              # Add key/value pairs in for-loop
    config_pairs[config_keys[i]] <- config_values[i]
  }


  return (config_pairs)
}
