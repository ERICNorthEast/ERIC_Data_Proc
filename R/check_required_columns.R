#' check_required_columns
#'
#' @param data_set
#'
#' @return
#' @export
#'
#' @examples
check_required_columns <- function(data_set,data_format) {

  # if (data_format == "std") {
  #   return (all(c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments") %in% colnames(data_set)))}
  #
  switch (data_format,
    "std" = return (all(c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments") %in% colnames(data_set))),
    "irecord" = return (all(c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments","Sample comment","Biotope","Sample method","Sex","Stage","Verifier") %in% colnames(data_set))),
    "eric" = return (all(c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments","Sample comment","Biotope","Sample method","Sex","Stage","Verifier") %in% colnames(data_set))),
    "bt" = return (all(c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments", "Breeding status","Breeding details","Pinpoint","Plumage") %in% colnames(data_set)))

  )
}
