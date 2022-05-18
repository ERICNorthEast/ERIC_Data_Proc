#' check_required_columns
#'
#' @param data_set
#'
#' @return
#' @export
#'
#' @examples
check_required_columns <- function(data_set) {

  return (all(c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments") %in% colnames(data_set)))
}
