#' Setup config for senstive species and taxon groups
#'
#' @return
#' @export
#'
#' @examples config <- setup_sensitive_config()
setup_sensitive_config <- function(){

  sens_config <- list()

  sens_config$sensitive_species <- c("Falco peregrinus" ,"Accipiter gentilis" ," Circus cyaneus" ," Circus aeruginosus" ," Aquila chrysaetos","Balaenoptera acutorostrata" ,"Lagenorhynchus albirostris" ,"Lutra lutra" ,"Martes martes" ,"Meles meles" ,"Pipistrellus" ,"Vespertilionidae", "Corallorhiza trifida" ,"Epipactis dunensis" ,"Epipactis phyllanthes" ,"Epipactis sancta" ,"Epipactis youngiana","Margaritifera margaritifera" ,"Hyles gallii" ,"Austropotamobius pallipes")
  sens_config$sensitive_desigs <- c("WACA-Sch5_sect9.1\\(kill/injuring\\)", "WACA-Sch1_part1", "WACA-Sch8")

  return(sens_config)
}
