#' Setup a list of config keys and values for designations that need correcting
#'
#' @return list
#' @export
#'
#' @examples config_pairs <- setup_desig_config_values()
setup_desig_config_values <- function(){

  config_keys <- c("WACA1","WACA2","NO_DESIG","WAXCAPS","waxcap_sp","DURHAM_BAP","NLAND_BAP","TV_BAP","durham_bap_sp","nland_bap_sp","tv_bap_sp")


  WACA1 <-"Wildlife & Countryside Act Sch 9 Part 1,"
  WACA2 <-"Wildlife & Countryside Act Sch 9 Part 2,"

  NO_DESIG  <-  "Designation not provided"

  WAXCAPS <- 'Waxcap grassland indicator, '

  #Waxcap grassland indicator species
  waxcap_sp <- c("Hygrocybe", "Entoloma", "Clavaria", "Microglossum", "Geoglossum", "Dermoloma", "Porpoloma", "Camarophyllopsis", "Clavulinopsis", "Ramariopsis", "Trichoglossum", "Glutinoglossum", "Cuphophyllus", "Porpolomopsis", "Neohygrocybe", "Gliophorus", "Gloioxanthomyces", "Hodophilus", "Pseudotricholoma" )

  #Local BAP designations
  DURHAM_BAP <-  "Durham BAP,"
  NLAND_BAP <-  "Northumberland BAP,"
  TV_BAP <-  "Tees Valley BAP,"

  #Local BAP species lists
  durham_bap_sp <- c("Recurvirostra avosetta", "Botaurus stellaris", "Podiceps caspicus", "Phalacrocorax carbo", "Cuculus canorus", "Calidris alpina", "Tringa alpina", "Fulmarus glacialis", "Pluvialis apricaria", "Locustella naevia", "Perdix perdix", "Motacilla cinerea", "Delichon urbicum", "Falco tinnunculus", "Rissa tridactyla", "Acanthis cabaret", "Carduelis cannabina subsp. autochthona/cannabina", "Carduelis cannabina", "Linaria cannabina", "Argialitis curonica", "Charadrius dubius", "Sterna albifrons", "Poecile palustris", "Turdus viscivorus", "Falco peregrinus", "Ficedula hypoleuca", "Aythya ferina", "Milvus ictinius", "Milvus milvus", "Milvus vulgaris", "Charadrius hiaticula", "Tadorna tadorna", "Hirundo rustica", "Apus apus", "Anthus trivialis", "Arenaria interpres", "Poecile montana", "Phylloscopus trochilus", "Scolopax rusticola", "Emberiza citrinella", "Micromys minutus", "Alca torda", "Carduelis cabaret", "Podiceps nigricollis", "Podiceps nigricollis subsp. nigricollis", "Poecile montanus")
  nland_bap_sp <- c("Recurvirostra avosetta", "Limosa lapponica", "Botaurus stellaris", "Podiceps caspicus", "Phalacrocorax carbo", "Cuculus canorus", "Calidris alpina", "Tringa alpina", "Anguilla anguilla", "Fulmarus glacialis", "Pluvialis apricaria", "Bucephala clangula", "Locustella naevia", "Uria aalge", "Circus cyaneus", "Delichon urbicum", "Passer domesticus", "Rissa tridactyla", "Acanthis cabaret", "Argialitis curonica", "Charadrius dubius", "Circus aeruginosus", "Poecile palustris", "Acrocephalus palustris", "Falco columbarius", "Falco aesalon", "Turdus viscivorus", "Falco peregrinus", "Ficedula hypoleuca", "Anser brachyrhynchus", "Aythya ferina", "Fratercula arctica", "Alca torda", "Milvus milvus", "Phalacrocorax aristotelis", "Tadorna tadorna", "Muscicapa striata", "Anthus trivialis", "Linaria flavirostris", "Poecile montana", "Phylloscopus trochilus")
  tv_bap_sp <- c("Botaurus stellaris", "Populus nigra ssp Betulifolia", "Lampetra planeri", "Salmo trutta", "Cottus gobio", "Neotinea ustulata", "Orchis ustulata", "Lacerta vivipara", "Vallonia excentrica", "Anguilla anguilla", "Arenostola phragmitidis", "Blysmus compressus", "Trollius europaeus", "Anacamptis morio", "Orchis morio", "Torilis nodosa", "Sterna albifrons", "Pupilla muscorum", "Silaum silaus", "Lampetra fluviatilis", "Salmo salar", "Hygrocybe punicea", "Petromyzon marinus", "Tadorna tadorna", "Anguis fragilis", "Tilia cordata", "Trifolium fragiferum", "Apus apus", "Carex elata", "Hottonia palustris", "Satyrium w-album", "Gagea lutea", "Motacilla flava")


  config_values <- list(WACA1,WACA2,NO_DESIG,WAXCAPS,waxcap_sp,DURHAM_BAP,NLAND_BAP,TV_BAP,durham_bap_sp,nland_bap_sp,tv_bap_sp)

  config_pairs <- list()                     # Create empty list
  for(i in 1:length(config_keys)) {              # Add key/value pairs in for-loop
    config_pairs[config_keys[i]] <- config_values[i]
  }


  return (config_pairs)
}
