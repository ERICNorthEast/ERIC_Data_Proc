#' Setup config for the Sunderland and South Tyneside SLA splitter
#'
#' @param bat_sp list of bat species
#' @param plant_sp list of plant species
#'
#' @return
#' @export
#'
#' @examples
setup_Sun_ST_split <- function(bat_sp,plant_sp) {
  Sun_ST_split <- tibble::tribble(~SheetLabel,~FilterString,
                        "Reptiles_data","tolower(SLA_data$`Taxon group`)=='reptile'",
                        "Water_Voles_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & tolower(SLA_data$`Taxon group`)=='terrestrial mammal'& (SLA_data$`Latin Name`=='Arvicola'| SLA_data$`Latin Name`=='Arvicola amphibius')",
                        "Plants_Lowerplants_Protd_data",  paste0("SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & stringr::str_detect(tolower(SLA_data$`Taxon group`),'",paste(plant_sp,collapse = '|'),"')"),
                        "Fish_Protected_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & stringr::str_detect(tolower(SLA_data$`Taxon group`),'^fish,')",
                        "Red_Squirrels_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & tolower(SLA_data$`Taxon group`)=='terrestrial mammal' & (SLA_data$`Latin Name`=='Sciurus vulgaris')",
                        "Otters_data" ,"SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & tolower(SLA_data$`Taxon group`)=='terrestrial mammal' & (SLA_data$`Latin Name`=='Lutra' |  SLA_data$`Latin Name`=='Lutra lutra')",
                        "Marine_Mammals_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & tolower(SLA_data$`Taxon group`)=='marine mammal'",
                        "Butterflies_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & tolower(SLA_data$`Taxon group`)=='insect - butterfly'",
                        "Birds_Protected_data", "SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & tolower(SLA_data$`Taxon group`)=='bird'",
                        "Bats_data", paste0("SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & stringr::str_detect(SLA_data$`Latin Name`,'",paste(bat_sp,collapse = '|'),"')"),
                        "Badgers_data", "SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & tolower(SLA_data$`Taxon group`)=='terrestrial mammal' & (SLA_data$`Latin Name`=='Meles'| SLA_data$`Latin Name`=='Meles meles')",
                        "Amphibians_data", "SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & tolower(SLA_data$`Taxon group`)=='amphibian'",
                        "Birds_DBAP_data", "SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP') & tolower(SLA_data$`Taxon group`)=='bird'",
                        "Fish_DBAP_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP') & stringr::str_detect(tolower(SLA_data$`Taxon group`),'fish')",
                        "Insects_DBAP_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP') & stringr::str_detect(tolower(SLA_data$`Taxon group`),'insect')",
                        "Mammals_DBAP_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP') & tolower(SLA_data$`Taxon group`)=='terrestrial mammal'",
                        "Plants_DBAP_data",paste0("SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP') & stringr::str_detect(tolower(SLA_data$`Taxon group`),'",paste(plant_sp,collapse = '|'),"')"),
                        "WACA_sch9_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & (stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP',negate=TRUE) | is.na(SLA_data$`North East LBAP - Short Names`)) & (SLA_data$`Wildlife & Countryside Act Sch 9 Part 1`=='Yes' | SLA_data$`Wildlife & Countryside Act Sch 9 Part 2`=='Yes')",
                        "Other_Birds_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & (stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP',negate=TRUE) | is.na(SLA_data$`North East LBAP - Short Names`))  & tolower(SLA_data$`Taxon group`)=='bird'  & is.na(SLA_data$`Wildlife & Countryside Act Sch 9 Part 1`) & is.na(SLA_data$`Wildlife & Countryside Act Sch 9 Part 2`)",
                        "Other_Mammals_data","SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='No' & (stringr::str_detect(SLA_data$`North East LBAP - Short Names`,'Durham BAP',negate=TRUE) | is.na(SLA_data$`North East LBAP - Short Names`)) & tolower(SLA_data$`Taxon group`)=='terrestrial mammal' & is.na(SLA_data$`Wildlife & Countryside Act Sch 9 Part 1`) & is.na(SLA_data$`Wildlife & Countryside Act Sch 9 Part 2`)"
  )
  return(Sun_ST_split)
}
