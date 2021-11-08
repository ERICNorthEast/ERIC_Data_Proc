#Grid letters for grid refs
gridLetters <- tibble::tribble(~Letters,~East, ~North,
                       "NZ", "4","5",
                       "NY", "3","5",
                       "NU", "4","6",
                       "NT", "3","6",
                       "SE", "4","4",
                       "SD", "3","4"
)


sysdata_filenames <- load("R/sysdata.rda")
save(list = c(sysdata_filenames, "gridLetters"),file = "R/sysdata.rda")
