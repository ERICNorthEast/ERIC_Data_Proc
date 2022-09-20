
#' Real list of email addresses to ignore from a csv file
#'
#' @return
#' @export
#'
#' @examples emls <- setup_emails_to_ignore()
setup_emails_to_ignore <- function(){
  file_name <-  "EmailsToIgnore.csv"
  file_path <- system.file("extdata",file_name,package = "ERICDataProc")

  emailsToIgnore <- read.csv(file_path)

  return(emailsToIgnore)
}
