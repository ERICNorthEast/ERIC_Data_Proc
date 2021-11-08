#' Setup config for protected species
#'
#' @return
#' @export
#'
#' @examples config <- setup_prot_config
setup_prot_config <- function() {


  prot_keys <- c("prot_species","prot_groups")
  prot_species <- c("Arvicola","Lutra","Meles","Sciurus")
  prot_groups <- c("fish, jawless \\(Agnatha\\)" , "marine mammal" , "reptile")
  prot_values <- list(prot_species,prot_groups)

  prot_pairs <- list()                     # Create empty list
  for(i in 1:length(prot_keys)) {              # Add key/value pairs in for-loop
    prot_pairs[prot_keys[i]] <- prot_values[i]
  }


  return (prot_pairs)
}


