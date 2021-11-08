#House numbers
#Number patterns allowed in locations
allowed_numbers <- list("[a-zA-Z]{2}[0-9]{1,2}[a-zA-Z]?[\\s]?[0-9][a-zA-Z]{2}","[a-zA-Z]{2}[0-9]{10}", "[a-zA-Z]{2}[0-9]{8}","[a-zA-Z]{2}[0-9]{6}", "[a-zA-Z]{2}[0-9]{4}",
                        "[a-zA-Z]{2}[0-9]{2}[a-zA-Z]?", "\\s[0-9]+[\\s]*$", "[ABC][0-9]{1,4}" , "[0-9]{1,3}[\\s]?[Kk]?[Mm]\\s", "[0-9]{1,2}[\\.]?[0-9]*[\\s]?mile[s]?\\s", "[0-9]{1,2}[\\s]?x[\\s]?[0-9]{1,2}")

sysdata_filenames <- load("R/sysdata.rda")
save(list = c(sysdata_filenames, "allowed_numbers"),file = "R/sysdata.rda")

