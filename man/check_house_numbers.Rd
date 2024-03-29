% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check_house_numbers.R
\name{check_house_numbers}
\alias{check_house_numbers}
\title{Checks for house numbers in location name column. Deals with them as far as possible}
\usage{
check_house_numbers(inputdata, locsToReplace, locsToIgnore)
}
\arguments{
\item{inputdata}{data to process}

\item{locsToReplace}{\itemize{
\item tibble of locations to replace
}}

\item{locsToIgnore}{\itemize{
\item tibble locations to ignore
}}
}
\value{

}
\description{
Checks for house numbers in location name column. Deals with them as far as possible
}
\examples{
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
names(df) <- c('All.Design', 'Wildlife..', 'Wildlife_1', 'Taxon.grou', 'Taxon.Lati', 'Taxon.Comm', 'Obs.Commen', 'Sample.Rec', 'Location Name', 'Sample.L_1', 'Sample.Dat', 'Sample.Spa', 'Survey.Run', 'Survey.Nam', 'Obs.Abunda', 'Determinat', 'Central_Ea', 'Central_No', 'Buffer', 'layer', 'path')
locsToIgnore <- tibble::tribble (~"Location name", "A189", "Sunderland 3")
locsToReplace <- tibble::tribble (~"Location Name", ~"Replacement", "- 37 Pretoria Avenue, Morpeth", "Pretoria Avenue, Morpeth", "1 Waynriggs Terrace","Waynriggs Terrace")
data <- check_house_numbers (df, locsToReplace, locsToIgnore)

}
