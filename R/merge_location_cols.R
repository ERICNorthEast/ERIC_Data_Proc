#' Merge the two locations columns into one
#'
#' @param raw_data
#'
#' @return
#' @export
#'
#' @examples
merge_location_cols <- function(raw_data) {
  raw_data$Sample.Loc <- ifelse(raw_data$Sample.Loc=='',stringr::str_replace_na(raw_data$Sample.L_1,''),raw_data$Sample.Loc)
  return(raw_data)
}
