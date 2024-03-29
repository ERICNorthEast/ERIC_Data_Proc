% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_pandn_data.R
\name{process_pandn_data}
\alias{process_pandn_data}
\title{Filter data to just protected & notable species}
\usage{
process_pandn_data(pandn_data, DR_config, optional_desigs)
}
\arguments{
\item{pandn_data}{data to process}

\item{DR_config}{list of config values}

\item{optional_desigs}{optional designations selected by the user}
}
\value{

}
\description{
Filter data to just protected & notable species
}
\examples{
options <- c("DURHAM_DESIGS","NP_DESIGS","TV_DESIGS"  )
config_keys <- c("locationCol","abundanceCol","commentCol","recorderCol","speciesCol","lastCol","dateCol","distanceCol","colCount","DROutputCols","DROutputColsWithDist","DRColNames","DRColNamesWithDist","FEP_DESIG","WNIO1_DESIG","WNIO2_DESIG","WNIO81_DESIG","WNIO82_DESIG","WNIO5_DESIG","WALES_DESIG","NORTH_DESIGS","NP_DESIGS","SCOT_DESIGS","TV_DESIGS","DURHAM_DESIGS")
locationCol <- 9
abundanceCol <- 5
commentCol <- 7
recorderCol <- 8
speciesCol <- 3
lastCol <- 14
dateCol <- 10
distanceCol <- 12
colCount <- 15
DROutputCols <- c("All.Design","Taxon.grou", "Taxon.Lati","Taxon.Comm", "Obs.Abunda", "Determinat","Obs.Commen","Sample.Rec",  "Sample.Loc",  "Sample.Dat","Sample.Spa","Survey.Run","Survey.Nam","Info")
DROutputColsWithDist <- c("All.Design","Taxon.grou", "Taxon.Lati","Taxon.Comm", "Obs.Abunda", "Determinat","Obs.Commen","Sample.Rec",  "Sample.Loc",  "Sample.Dat","Sample.Spa","distance","Survey.Run","Survey.Nam","Info")
DRColNames <- c("Designations","Taxon group","Latin Name","Common Name","Abundances","Determination Type","Comments","Recorder","Location Name","Date","Grid Reference","Survey Run By","Survey Name","Additional Information")
DRColNamesWithDist <- c("Designations","Taxon group","Latin Name","Common Name","Abundances","Determination Type","Comments","Recorder","Location Name","Date","Grid Reference","Approx. Distance(m)","Survey Run By","Survey Name","Additional Information")
FEP_DESIG <- "FEP-001"
WNIO1_DESIG <- "W(NI)O-Sch1_part1"
WNIO2_DESIG <- "W(NI)O-Sch1_part2"
WNIO81_DESIG <- "W(NI)O-Sch8_part1"
WNIO82_DESIG <- "W(NI)O-Sch8_part2"
WNIO5_DESIG <- "W(NI)O-Sch5"
WALES_DESIG <- "Wales_NERC_S.42"
NORTH_DESIGS <- 'pandn_data$All.Design== "Northumberland BAP" |  pandn_data$All.Design=="FEP-001, Northumberland NP BAP"'
NP_DESIGS <- 'pandn_data$All.Design=="FEP-001, Northumberland NP BAP"'
SCOT_DESIGS <- 'pandn_data$All.Design== "Scottish_Biodiversity_List" | pandn_data$All.Design== "Scottish_Biodiversity_List, W(NI)O-Sch1_part1" |pandn_data$All.Design== "Scottish_Biodiversity_List, Wales_NERC_S.42" |pandn_data$All.Design== "FEP-001, Scottish_Biodiversity_List"'
TV_DESIGS <- 'pandn_data$All.Design=="Tees Valley BAP" | pandn_data$All.Design=="Tees Valley BAP, " | pandn_data$All.Design=="Tees Valley BAP, FEP-001" | pandn_data$All.Design=="Tees Valley BAP, Tees Valley BAP" | pandn_data$All.Design=="Tees Valley BAP, Tees Valley BAP, W(NI)O-Sch8_part1, Wales_NERC_S.42" | pandn_data$All.Design=="Tees Valley BAP, W(NI)O-Sch8_part1, Wales_NERC_S.42"'
DURHAM_DESIGS <- 'pandn_data$All.Design=="Durham BAP"'
config_values <- list(locationCol,abundanceCol,commentCol,recorderCol,speciesCol,lastCol,dateCol,distanceCol,colCount, DROutputCols,DROutputColsWithDist,DRColNames,DRColNamesWithDist,FEP_DESIG,WNIO1_DESIG,WNIO2_DESIG,WNIO81_DESIG,WNIO82_DESIG,WNIO5_DESIG,WALES_DESIG,    NORTH_DESIGS,NP_DESIGS,SCOT_DESIGS,TV_DESIGS,DURHAM_DESIGS)
config_pairs <- list()                     # Create empty list
for(i in 1:length(config_keys)) {              # Add key/value pairs in for-loop
config_pairs[config_keys[i]] <- config_values[i]
}
a <- c('','BirdsDir-A2.2')
b <- c('','')
c <- c('','')
d <- c('insect - butterfly','bird')
e <- c('Pieris rapae','Turdus merula')
f <- c('Small White','Blackbird')
g <- c('Mike Jeffries','Alnwick Wildlife Group')
h <- c('Morpeth Town','Morpeth Town')
i <- c('04/09/2010','04/09/2010')
j <- c('NZ18X','NZ18X')
k <- c('Butterfly Conservation','Alnwick Wildlife Group')
l <- c('Butterfly Conservation','Alnwick Wildlife Group')
m <- c('2 Count','1 Count')
n <- c('Considered Correct','Considered Correct')
o <- c(419000,419000)
p <- c(585000,585000)
q <- c(1000,1000)
r <- c('layer','layer')
s <- c('path','path')
df <- data.frame(a,b,c,d,e,f,a,g,h,h,i,j,k,l,m,n,o,p,q,r,s)
names(df) <- c('All.Design', 'Wildlife..', 'Wildlife_1', 'Taxon.grou', 'Taxon.Lati', 'Taxon.Comm', 'Obs.Commen', 'Sample.Rec', 'Sample.Loc', 'Sample.L_1', 'Sample.Dat', 'Sample.Spa', 'Survey.Run', 'Survey.Nam', 'Obs.Abunda', 'Determinat', 'Central_Ea', 'Central_No', 'Buffer', 'layer', 'path')
data <- change_recorder_col_names(df)
pandn_data <- process_pandn_data(df,config_pairs,options)
}
