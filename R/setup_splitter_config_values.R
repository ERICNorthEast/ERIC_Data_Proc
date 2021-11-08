#' Setup config for SLA splitter
#'
#' @return
#' @export
#'
#' @examples config <- setup_splitter_config_values ()
setup_splitter_config_values <- function(){
  config_keys <- c("short_desigs","bat_sp","plant_sp","pearl_mussel_select","protected_mammals","non_inverts","s41_mammals")

  short_desigs <- c('Durham BAP' = 'DuBAP','Newcastle BAP' = 'NeBAP','North Tyneside BAP' = 'NTBAP','Northumberland BAP' = 'NoBAP','Northumberland NP BAP' = 'NNPBAP','Tees Valley BAP' = 'TVBAP','England NERC' = 'EngNERC','England_NERC' = 'EngNERC','Wales NERC' = 'WalNERC','Wales_NERC' = 'WalNERC','Scottish Biodiversity List' = 'ScotBio','Scottish_Biodiversity_List' = 'ScotBio','RedList_Global' = 'RL_Glob','RedList_GB' = 'RL_GB','WACA-Sch5_sect9.1(kill/injuring)' = 'WACA5_9.1(kill/inj)','WACA-Sch5_sect9.1(taking)' = 'WACA5_9.1(tak)','WACA-Sch5_sect9.2' = 'WACA5_9.2','WACA-Sch5Sect9.4A*' = 'WACA5_9.4A*','WACA-Sch5_sect9.4.a' = 'WACA5_9.4.a','WACA-Sch5_sect9.4b' = 'WACA5_9.4b','WACA-Sch5Sect9.4c' = 'WACA5_9.4c','WACA-Sch5_sect9.5a' = 'WACA5_9.5a','WACA-Sch5_sect9.5b' = 'WACA5_9.5b')

  bat_sp <- c("Chiroptera","Rhinolophidae","Vespertilionidae","Myotis","Barbastella","Eptesicus","Nyctalus","Pipistrellus","Plecotus","Vespertilio","Lasiurus","Tadarida","Hypsugo")
  plant_sp <- c("flowering plant" ,"clubmoss" ,"conifer" ,"fern" ,"fungus" ,"ginkgo" ,"horsetail" ,"lichen" ,"liverwort" ,"moss" ,"slime mould" ,"stonewort" ,"alga" ,"hornwort" ,"quillwort")
  pearl_mussel_select <- "SLA_data$`Latin Name`=='Margaritifera (Margaritifera) margaritifera' | SLA_data$`Latin Name`=='Margaritifera margaritifera'"
  protected_mammals <- c("Arvicola amphibius", "Arvicola", "Lutra", "Lutra lutra", "Meles", "Meles meles", "Sciurus vulgaris", "Martes martes", "Martes", "Muscardinus avellanarius","Chiroptera","Rhinolophidae","Vespertilionidae","Myotis","Barbastella","Eptesicus","Nyctalus","Pipistrellus","Plecotus","Vespertilio","Lasiurus","Tadarida","Hypsugo")
  non_inverts <- c("alga", "amphibian", "bird", "chromist", "clubmoss", "conifer", "diatom", "fern", "flowering plant", "fungus", "ginkgo", "hornwort", "horsetail", "lancelet (Cephalochordata)", "lichen", "liverwort", "marine mammal", "moss", "quillwort", "reptile", "slime mould", "stonewort", "terrestrial mammal","fish,")
  s41_mammals <- c("Erinaceus europaeus", "Lepus europaeus", "Mustela putorius", "Micromys minutus")

  config_values <- list(short_desigs,bat_sp,plant_sp,pearl_mussel_select,protected_mammals,non_inverts,s41_mammals)

  config_pairs <- list()                     # Create empty list
  for(i in 1:length(config_keys)) {              # Add key/value pairs in for-loop
    config_pairs[config_keys[i]] <- config_values[i]
  }


  return (config_pairs)
}
