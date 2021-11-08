#' Remove the optional designations selected by the user
#'
#' @param pandn_data
#' @param optional_desigs Designations selected by the user
#' @param config_vals List of config values
#'
#' @return
#' @export
#'
#' @examples
remove_optional_desigs <- function(pandn_data,optional_desigs,config_vals) {

  DURHAM_DESIGS <- config_vals["DURHAM_DESIGS"]
  NORTH_DESIGS <- config_vals["NORTH_DESIGS"]
  NP_DESIGS <- config_vals["NP_DESIGS"]
  SCOT_DESIGS <- config_vals["SCOT_DESIGS"]
  TV_DESIGS <- config_vals["TV_DESIGS"]

  # And the optional ones
  for (i in optional_desigs)
  {

    pandn_data <- dplyr::filter(pandn_data,!(eval(str2expression(unlist(eval(str2expression(i)))))))
  }

  filtered_data <- pandn_data

  return(filtered_data)
}
