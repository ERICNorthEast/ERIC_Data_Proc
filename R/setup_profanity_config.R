#' Read list of words for profanity check from a CSV file
#'
#' @return
#' @export
#'
#' @examples words <- setup_profanity_config()
setup_profanity_config <- function(){
  file_name <-  "SwearWords.csv"
  file_path <- system.file("extdata",file_name,package = "ERICDataProc")
  swearWords <- readr::read_csv(file_path)

  return(swearWords)
}
