#' Setup config to split data for the Northumberland SLA
#'
#' @param plant_sp List of plant species
#' @param Other_LA_Cols List of columns
#' @param SLA_Split_config config values
#'
#' @return
#' @export
#'
#' @examples
#' bat_sp <- c("Chiroptera","Rhinolophidae","Vespertilionidae")
#' plant_sp <- c("flowering plant" ,"clubmoss" ,"conifer")
#' Other_LA_Cols <- c("Taxon group","Latin Name","Abundances","Determination Type")
#
#' pearl_mussel_select <- "SLA_data$`Latin Name`=='Margaritifera (Margaritifera) margaritifera' | SLA_data$`Latin Name`=='Margaritifera margaritifera'"
#' protected_mammals <- c("Arvicola amphibius", "Arvicola", "Lutra", "Lutra lutra", "Meles", "Meles meles", "Sciurus vulgaris", "Martes martes", "Martes", "Muscardinus avellanarius","Chiroptera","Rhinolophidae","Vespertilionidae","Myotis","Barbastella","Eptesicus","Nyctalus","Pipistrellus","Plecotus","Vespertilio","Lasiurus","Tadarida","Hypsugo")
#' non_inverts <- c("alga", "amphibian", "bird", "chromist", "clubmoss", "conifer", "diatom", "fern", "flowering plant", "fungus", "ginkgo", "hornwort", "horsetail", "lancelet (Cephalochordata)", "lichen", "liverwort", "marine mammal", "moss", "quillwort", "reptile", "slime mould", "stonewort", "terrestrial mammal","fish,")
#' s41_mammals <- c("Erinaceus europaeus", "Lepus europaeus", "Mustela putorius", "Micromys minutus")
#
#' list <- getNorthumberlandConfig(plant_sp,Other_LA_Cols,pearl_mussel_select,protected_mammals,non_inverts,s41_mammals)
#'
getNorthumberlandConfig<- function(plant_sp,Other_LA_Cols,pearl_mussel_select,protected_mammals,non_inverts,s41_mammals){

  # pearl_mussel_select <- SLA_Split_config["pearl_mussel_select"]
  # protected_mammals <- SLA_Split_config["protected_mammals"]
  # non_inverts <- SLA_Split_config["non_inverts"]
  # s41_mammals <- SLA_Split_config["s41_mammals"]

  #Northumberland
  may_june_birds <- c("Recurvirostra avosetta", "Tyto alba", "Panurus biarmicus", "Tetrao tetrix", "Podiceps nigricollis", "Emberiza calandra",                     "Numenius arquata", "Calidris alpina", "Anas strepera",
                      "Pluvialis apricaria", "Bucephala clangula", "Accipiter gentilis",
                      "Locustella naevia", "Perdix perdix", "Coccothraustes coccothraustes",
                      "Circus cyaneus", "Falco subbuteo", "Alcedo atthis", "Charadrius dubius",
                      "Asio otus", "Circus aeruginosus", "Larus melanocephalus", "Falco columbarius",
                      "Caprimulgus europaeus", "Pandion haliaetus", "Falco peregrinus",
                      "Ficedula hypoleuca", "Aythya ferina", "Corvus corax", "Milvus milvus",
                      "Tringa totanus", "Acrocephalus scirpaceus", "Turdus torquatus",
                      "Charadrius hiaticula", "Calidris pugnax", "Asio flammeus", "Anas clypeata",
                      "Gallinago gallinago", "Anas crecca", "Passer montanus", "Rallus aquaticus",
                      "Anas penelope", "Phylloscopus sibilatrix", "Scolopax rusticola", "Motacilla flava")


  schedule1_birds <- c("Recurvirostra avosetta","Alcedo atthis", "Tyto alba", "Charadrius dubius","Panurus biarmicus")

  amphibian_sp <- c("Triturus cristatus", "Bufo bufo")

  marine_invasives <- c("Crassostrea gigas", "Codium fragile", "Caprella mutica",
                        "Eriocheir sinensis", "Sargassum muticum", "Elminius modestus",
                        "Corella eumyota", "Colpomenia peregrina", "Crepidula fornicata",
                        "Undaria pinnatifida", "Didemnum vexillum", "Asparagopsis armata",
                        "Haliplanella lineata", "Styela clava")

  Northumberland_split <- tibble::tribble(~SheetLabel,~FilterString,
                                  "Amphibians_data", paste0("stringr::str_detect(SLA_data$`Latin Name`,'",paste(unlist(amphibian_sp),collapse='|'),"')"),
                                  #"Freshwater_Pearl_Mussel_data",paste0("stringr::str_detect(SLA_data$`Latin Name`,'",paste0(pearl_mussel_sp,collapse='|'),"')"),
                                  "Freshwater_Pearl_Mussel_data",pearl_mussel_select,
                                  "May_June_Birds_data",paste0("stringr::str_detect(SLA_data$Date,'/06/|/05/|May|Jun') & stringr::str_detect(SLA_data$`Latin Name`,'",paste(unlist(may_june_birds),collapse='|'),"')"),
                                  "Non_Native_Invasives_data", "SLA_data$`Wildlife & Countryside Act Sch 9 Part 1` == 'Yes' | SLA_data$`Wildlife & Countryside Act Sch 9 Part 2`  == 'Yes'",
                                  "Protected_Mammals_data" ,paste0("tolower(SLA_data$`Taxon group`)=='terrestrial mammal' & stringr::str_detect(SLA_data$`Latin Name`,'",paste(unlist(protected_mammals),collapse='|'),"')"),
                                  "Protected_Plants_data",paste0("!stringr::str_detect(SLA_data$`Latin Name`,'^Hyacinthoides') & SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes' & stringr::str_detect(tolower(SLA_data$`Taxon group`),'",paste(unlist(plant_sp),collapse = '|'),"')"),
                                  "Protected_Inverts_data", paste0("!stringr::str_detect(SLA_data$Designations,'England_NERC_S.41') & SLA_data$`All UK Legally Protected (W&C Act, Badgers and Cons Regs) - Taxon Designated`=='Yes'  & !stringr::str_detect(tolower(SLA_data$`Taxon group`),'", paste(unlist(non_inverts),collapse = '|'),"')"),
                                  "Reptiles_data","tolower(SLA_data$`Taxon group`)=='reptile'",
                                  "S41_Invertebrates_data", paste0("stringr::str_detect(SLA_data$Designations,'England_NERC_S.41') & !stringr::str_detect(tolower(SLA_data$`Taxon group`),'", paste(unlist(non_inverts),collapse = '|'),"')"),
                                  "S41_Mammals_data", paste0("stringr::str_detect(SLA_data$`Latin Name`,'",paste(unlist(s41_mammals),collapse='|'),"')"),
                                  "S41_Plants_data", paste0("stringr::str_detect(SLA_data$Designations,'England_NERC_S.41') & stringr::str_detect(tolower(SLA_data$`Taxon group`),'",paste(unlist(plant_sp),collapse = '|'),"')"),
                                  "Sch1_Birds_data", paste0("stringr::str_detect(SLA_data$`Latin Name`,'",paste(unlist(schedule1_birds),collapse='|'),"')"),
                                  "WhiteClawed_Crayfish_data", "stringr::str_detect(SLA_data$`Latin Name`,'Austropotamobius pallipes')",
                                  "Marine_Mammals_data", "(tolower(SLA_data$`Taxon group`)=='marine mammal' & SLA_data$`Latin Name`!='Phoca vitulina')|SLA_data$`Latin Name`=='Anarhichas lupus' ",
                                  "Marine_invasives_data", paste0("stringr::str_detect(SLA_data$`Latin Name`,'",paste(unlist(marine_invasives),collapse='|'),"')")

  )

  return (list("Source","",Northumberland_split,Other_LA_Cols))
}
