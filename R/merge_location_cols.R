#' Merge the two locations columns into one
#'
#' @param raw_data data frame of raw data
#'
#' @return
#' @export
#'
#' @examples

#'h <- c('Morpeth Town','Morpeth Town')
#'df <- data.frame(h,h)
#'names(df) <- c('Sample.Loc', 'Sample.L_1')
#'data <- merge_location_cols(df)
merge_location_cols <- function(raw_data) {
  raw_data$Sample.Loc <- ifelse(raw_data$Sample.Loc=='',stringr::str_replace_na(raw_data$Sample.L_1,''),raw_data$Sample.Loc)
  return(raw_data)
}
