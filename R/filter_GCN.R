#' Remove zero abundance Great crested newt records
#'
#' @param raw_data
#'
#' @return
#' @export
#'
#' @examples
filter_GCN <- function(raw_data) {

  filtered_data <- dplyr::filter(raw_data,!(raw_data$Survey.Nam=='Great Crested Newt License Returns' & raw_data$Obs.Abunda == '0 Count'))
  return (filtered_data)

}
