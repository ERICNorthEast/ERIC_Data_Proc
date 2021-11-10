#' Set the protected flag for appropriate species
#'
#' @param raw_data
#'
#' @return
#' @export
#'
#' @examples
#'a <- c('','')
#'d <- c('insect - butterfly','bird')
#'e <- c('Pieris rapae','Turdus merula')

#'df <- data.frame(d,e,a)

#'names(df) <- c('Taxon.grou', 'Taxon.Lati','AllUKLeg')
#'
#' data <- set_protected_flag(df)
set_protected_flag <- function(raw_data) {
  #Not sure prot species is correct - = not "in"
  raw_data$AllUKLeg <- ifelse((raw_data$Taxon.grou=='amphibian' & raw_data$Taxon.Lati != 'Mesotriton alpestris'),'Yes',raw_data$AllUKLeg)
  #AED this needs sorting - getting this twice
  prot_config <- setup_prot_config()
  prot_species <- prot_config["prot_species"]
  prot_groups <- prot_config["prot_groups"]
  raw_data$AllUKLeg <- ifelse((stringr::str_detect(raw_data$Taxon.grou,paste(prot_groups,collapse = '|')) | stringr::str_detect(raw_data$Taxon.Lati,paste(prot_species,collapse = '|'))),'Yes',raw_data$AllUKLeg)

  return(raw_data)
}
