#' check_required_columns
#'
#' @param data_set
#'
#' @return
#' @export
#'
#' @examples


#'sn <- c('Pieris rapae','Turdus merula')
#'cn <- c('Small White','Blackbird')
#'rec <- c('Mike Jeffries','Alnwick Wildlife Group')
#'loc <- c('Morpeth Town','Morpeth Town')
#'dt <- c('04/09/2010','04/09/2010')
#'gr <- c('NZ18X','NZ18X')
#'com <- c('Butterfly Conservation','Alnwick Wildlife Group')

#'ab <- c('2 Count','1 Count')

#'data <- data.frame(rec,cn,sn,dt,gr,loc,ab,com)
#'colmames(data) <- c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments")
#'   result <- check_required_columns(data,'std')
check_required_columns <- function(data_set,data_format) {

  # if (data_format == "std") {
  #   return (all(c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments") %in% colnames(data_set)))}
  #
  switch (data_format,
    "std" = return (all(c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments") %in% colnames(data_set))),
    "irecord" = return (all(c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments","Sample comment","Biotope","Sample method","Sex","Stage","Verifier") %in% colnames(data_set))),
    "eric" = return (all(c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments","Sample comment","Biotope","Sample method","Sex","Stage","Verifier") %in% colnames(data_set))),
    "bt" = return (all(c("Recorder","Common Name","Species Name", "Date", "Grid Reference", "Location Name", "Abundances","Comments") %in% colnames(data_set)))

  )
}
