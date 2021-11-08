#' Filter out designations we used to remove manually
#'
#' @param pandn_data
#' @param config_vals list of config values
#'
#' @return
#' @export
#'
#' @examples
remove_manual_desigs <- function(pandn_data,config_vals){

  FEP_DESIG <- config_vals["FEP_DESIG"]
  WNIO1_DESIG <- config_vals["WNIO1_DESIG"]
  WNIO2_DESIG <- config_vals["WNIO2_DESIG"]
  WNIO81_DESIG <- config_vals["WNIO81_DESIG"]
  WNIO82_DESIG <- config_vals["WNIO82_DESIG"]
  WNIO5_DESIG <- config_vals["WNIO5_DESIG"]
  WALES_DESIG <- config_vals["WALES_DESIG"]

  filtered_data <- dplyr::filter(pandn_data,!(pandn_data$All.Design== "" | pandn_data$All.Design== unlist(FEP_DESIG) | pandn_data$All.Design== unlist(WNIO1_DESIG) | pandn_data$All.Design== unlist(WNIO2_DESIG) | pandn_data$All.Design== unlist(WNIO81_DESIG) | pandn_data$All.Design== unlist(WNIO82_DESIG) | pandn_data$All.Design== unlist(WNIO5_DESIG) | pandn_data$All.Design== unlist(WALES_DESIG)))
  return(filtered_data)
}
