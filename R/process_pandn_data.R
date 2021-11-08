#' Filter data to just protected & notable species
#'
#' @param pandn_data
#' @param DR_config list of config values
#' @param optional_desigs optional designations selectd by the user
#'
#' @return
#' @export
#'
#' @examples
process_pandn_data <- function(pandn_data,DR_config,optional_desigs){

  # #For the P&N data remove the designations we'd delete manually
  filtered_data <- remove_manual_desigs(pandn_data,DR_config)

  # And the optional ones
  filtered_data <- remove_optional_desigs(filtered_data,optional_desigs,DR_config)

  return(filtered_data)
}
