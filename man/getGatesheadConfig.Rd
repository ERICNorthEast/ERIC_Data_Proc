% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getGatesheadConfig.R
\name{getGatesheadConfig}
\alias{getGatesheadConfig}
\title{Setup config to split data for the Gateshead SLA}
\usage{
getGatesheadConfig(bat_sp, plant_sp, Other_LA_Cols)
}
\arguments{
\item{bat_sp}{List of bat species}

\item{plant_sp}{List of plant species}

\item{Other_LA_Cols}{List of columns}
}
\value{

}
\description{
Setup config to split data for the Gateshead SLA
}
\examples{
bat_sp <- c("Chiroptera","Rhinolophidae","Vespertilionidae")
plant_sp <- c("flowering plant" ,"clubmoss" ,"conifer")
Other_LA_Cols <- c("Taxon group","Latin Name","Abundances","Determination Type")
list <- getGatesheadConfig(bat_sp,plant_sp,Other_LA_Cols)
}
