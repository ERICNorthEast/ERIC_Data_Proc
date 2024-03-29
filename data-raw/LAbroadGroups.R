#Broad group lookup
group_LU <- tibble::tribble(~group,~broadgroup,
                            "alga", "Alga",
                            "amphibian", "Amphibian",
                            "bacterium", "Bacterium",
                            "bird", "Bird",
                            "Bird", "Bird",
                            "chromist", "Chromist",
                            "crustacean", "Crustacean",
                            "diatom", "Diatom",
                            "cartilagenous fish (Chondrichthyes)", "Fish",
                            "bony fish (Actinopterygii)", "Fish",
                            "jawless fish (Agnatha)", "Fish",
                            "fungus", "Fungus",
                            "acarine (Acari)", "Invertebrate",
                            "annelid", "Invertebrate",
                            "bryozoan", "Invertebrate",
                            "centipede", "Invertebrate",
                            "coelenterate (=cnidarian)", "Invertebrate",
                            "false scorpion (Pseudoscorpiones)", "Invertebrate",
                            "flatworm (Turbellaria)", "Invertebrate",
                            "hairworm (Nematomorpha)", "Invertebrate",
                            "harvestman (Opiliones)", "Invertebrate",
                            "insect - alderfly (Megaloptera)", "Invertebrate",
                            "insect - beetle (Coleoptera)", "Invertebrate",
                            "insect - booklouse (Psocoptera)", "Invertebrate",
                            "insect - butterfly", "Invertebrate",
                            "insect - caddis fly (Trichoptera)", "Invertebrate",
                            "insect - dragonfly (Odonata)", "Invertebrate",
                            "insect - earwig (Dermaptera)", "Invertebrate",
                            "insect - hymenopteran", "Invertebrate",
                            "insect - lacewing (Neuroptera)", "Invertebrate",
                            "insect - mayfly (Ephemeroptera)", "Invertebrate",
                            "insect - moth", "Invertebrate",
                            "insect - orthopteran", "Invertebrate",
                            "insect - scorpion fly (Mecoptera)", "Invertebrate",
                            "insect - stonefly (Plecoptera)", "Invertebrate",
                            "insect - true bug (Hemiptera)", "Invertebrate",
                            "insect - true fly (Diptera)", "Invertebrate",
                            "millipede", "Invertebrate",
                            "mollusc", "Invertebrate",
                            "spider (Araneae)", "Invertebrate",
                            "tapeworm (Cestoda)", "Invertebrate",
                            "clubmoss", "Lower Plant",
                            "hornwort", "Lower Plant",
                            "lichen", "Lower Plant",
                            "liverwort", "Lower Plant",
                            "moss", "Lower Plant",
                            "stonewort", "Lower Plant",
                            "terrestrial mammal", "Mammal",
                            "marine mammal", "Mammal",
                            "conifer", "Plant",
                            "fern", "Plant",
                            "flowering plant", "Plant",
                            "horsetail", "Plant",
                            "reptile", "Reptile",
                            "slime mould", "Slime Mould",
                            "sponge (Porifera)", "Sponge",
                            "comb jelly (Ctenophora)", "Invertebrate",
                            "echinoderm", "Invertebrate",
                            "ginkgo", "Plant",
                            "horseshoe worm (Phoronida)", "Invertebrate",
                            "insect - bristletail (Archaeognatha)", "Invertebrate",
                            "insect - cockroach (Dictyoptera)", "Invertebrate",
                            "insect - flea (Siphonaptera)", "Invertebrate",
                            "insect - silverfish (Thysanura)", "Invertebrate",
                            "insect - snakefly (Raphidioptera)", "Invertebrate",
                            "insect - thrips (Thysanoptera)", "Invertebrate",
                            "peanut worm (Sipuncula)", "Invertebrate",
                            "protozoan", "Protozoan",
                            "ribbon worm (Nemertinea)", "Invertebrate",
                            "roundworm (Nematoda)", "Invertebrate",
                            "scorpion", "Invertebrate",
                            "sea spider (Pycnogonida)", "Invertebrate",
                            "springtail (Collembola)", "Invertebrate",
                            "tunicate (Urochordata)", "Invertebrate")

sysdata_filenames <- load("R/sysdata.rda")
save(list = c(sysdata_filenames, "group_LU"),file = "R/sysdata.rda")
