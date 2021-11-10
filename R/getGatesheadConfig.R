#' Setup config to split data for the Gateshead SLA
#'
#' @param bat_sp List of bat species
#' @param plant_sp List of plant species
#' @param Other_LA_Cols List of columns
#'
#' @return
#' @export
#'
#' @examples
#' bat_sp <- c("Chiroptera","Rhinolophidae","Vespertilionidae")
#' plant_sp <- c("flowering plant" ,"clubmoss" ,"conifer")
#' Other_LA_Cols <- c("Taxon group","Latin Name","Abundances","Determination Type")
#' list <- getGatesheadConfig(bat_sp,plant_sp,Other_LA_Cols)
getGatesheadConfig <- function(bat_sp,plant_sp,Other_LA_Cols){
  Gateshead_split <- tibble::tribble(~SheetLabel,~FilterString,
                             "Reptiles_data","tolower(SLA_data$`Taxon group`)=='reptile'",
                             "Water_Voles_data","SLA_data$`Latin Name`=='Arvicola amphibius'  & SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes'",
                             "Fish_Protected_data","stringr::str_detect(tolower(SLA_data$`Taxon group`),'^fish,') & SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes'",
                             "Red_Squirrel_data","SLA_data$`Latin Name`=='Sciurus vulgaris'  & SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes'",
                             "Plant_LowerPlantProtected_data",paste0("SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & stringr::str_detect(tolower(SLA_data$`Taxon group`),'",paste(plant_sp,collapse = '|'),"')"),
                             "Otters_data","(SLA_data$`Latin Name`=='Lutra lutra' | SLA_data$`Latin Name`=='Lutra')  & SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes'",
                             "Marine_Mammals_data","tolower(SLA_data$`Taxon group`)=='marine mammal' & SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes'",
                             "Butterflies_Protected_data","tolower(SLA_data$`Taxon group`)=='insect - butterfly' & SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes'",
                             "Birds_Protected_data","tolower(SLA_data$`Taxon group`)=='bird' & SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes'",
                             "Bats_data", paste0("SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & stringr::str_detect(SLA_data$`Latin Name`,'",paste(bat_sp,collapse = '|'),"')"),
                             "Badgers_data","(SLA_data$`Latin Name`=='Meles meles' | SLA_data$`Latin Name`=='Meles')  & SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes'",
                             "Amphibians_data", "tolower(SLA_data$`Taxon group`)=='amphibian' & SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes'",
                             "Polecat_data","stringr::str_detect(SLA_data$`Latin Name`,'Mustela putorius') & SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes'",
                             "DBAP_Birds_data", "SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP') & tolower(SLA_data$`Taxon group`)=='bird'",
                             "DBAP_Fish_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP') & stringr::str_detect(tolower(SLA_data$`Taxon group`),'fish')",
                             "DBAP_Insects_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP') & stringr::str_detect(tolower(SLA_data$`Taxon group`),'insect')",
                             "DBAP_Mammals_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP') & tolower(SLA_data$`Taxon group`)=='terrestrial mammal'",
                             "DBAP_Plants_LowerPlants_data",paste0("SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP') & stringr::str_detect(tolower(SLA_data$`Taxon group`),'",paste(plant_sp,collapse = '|'),"')"),
                             "WACA_Sch9_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & (stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP',negate=TRUE) | is.na(SLA_data$`North East LBAP - Short Names`)) & (SLA_data$`Wildlife & Countryside Act Sch 9 Part 1`=='Yes' | SLA_data$`Wildlife & Countryside Act Sch 9 Part 2`=='Yes')",
                             "Other_Birds_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & (stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP',negate=TRUE) | is.na(SLA_data$`North East LBAP - Short Names`))  & tolower(SLA_data$`Taxon group`)=='bird'  & is.na(SLA_data$`Wildlife & Countryside Act Sch 9 Part 1`) & is.na(SLA_data$`Wildlife & Countryside Act Sch 9 Part 2`)",
                             "Other_Mammals_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & (stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP',negate=TRUE) | is.na(SLA_data$`North East LBAP - Short Names`)) & tolower(SLA_data$`Taxon group`)=='terrestrial mammal' & is.na(SLA_data$`Wildlife & Countryside Act Sch 9 Part 1`) & is.na(SLA_data$`Wildlife & Countryside Act Sch 9 Part 2`)"


  )

  return (list("Source","",Gateshead_split,Other_LA_Cols))



}
