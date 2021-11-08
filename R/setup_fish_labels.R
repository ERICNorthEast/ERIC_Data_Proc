#' Setup list of labels to use for fish taxon
#'
#' @return
#' @export
#'
#' @examples fish <- setup_fish_labels()
setup_fish_labels <- function() {

  fish_labels <- c('jawless fish \\(Agnatha\\)'='fish, jawless \\(Agnatha\\)','bony fish \\(Actinopterygii\\)' = 'fish, bony \\(Actinopterygii\\)','cartilagenous fish \\(Chondrichthyes\\)' = 'fish, cartilagenous \\(Chondrichthyes\\)')
  return(fish_labels)
}
