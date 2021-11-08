# Prefix mappings for grid refs
prefixes <- tibble::tribble(~prefix,~pvalue,"NZ","45","NY",	"35","NT",	"36",  "NU",	"46","SD",	"34","SE",	"44")

sysdata_filenames <- load("R/sysdata.rda")
save(list = c(sysdata_filenames, "prefixes"),file = "R/sysdata.rda")

