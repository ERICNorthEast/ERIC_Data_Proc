#' Convert column names from those output by Recorder to match QGIS ones
#'
#' @param raw_data data frame of raw data
#'
#' @return
#' @export
#'
#' @examples
#'a <- c('','BirdsDir-A2.2')
#'b <- c('','')
#'c <- c('','')
#'d <- c('insect - butterfly','bird')
#'e <- c('Pieris rapae','Turdus merula')
#'f <- c('Small White','Blackbird')
#'g <- c('Mike Jeffries','Alnwick Wildlife Group')
#'h <- c('Morpeth Town','Morpeth Town')
#'i <- c('04/09/2010','04/09/2010')
#'j <- c('NZ18X','NZ18X')
#'k <- c('Butterfly Conservation','Alnwick Wildlife Group')
#'l <- c('Butterfly Conservation','Alnwick Wildlife Group')
#'m <- c('2 Count','1 Count')
#'n <- c('Considered Correct','Considered Correct')
#'o <- c(419000,419000)
#'p <- c(585000,585000)
#'q <- c(1000,1000)
#'r <- c('layer','layer')
#'s <- c('path','path')

#'df <- data.frame(a,b,c,d,e,f,a,g,h,h,i,j,k,l,m,n,o,p,q,r,s)


#'names(df) <- c('All.Design', 'Wildlife..', 'Wildlife_1', 'Taxon.grou', 'Taxon.Lati', 'Taxon.Comm', 'Obs.Commen', 'Sample.Rec', 'Sample.Loc', 'Sample.L_1', 'Sample.Dat', 'Sample.Spa', 'Survey.Run', 'Survey.Nam', 'Obs.Abunda', 'Determinat', 'Central_Ea', 'Central_No', 'Buffer', 'layer', 'path')
#' data <- change_recorder_col_names(df)
#'
#'
change_recorder_col_names <- function(raw_data) {
  raw_data$All.Design <- raw_data$All.Designations...Short.Names
  raw_data$AllUKLeg   <- raw_data$All.UK.Legally.Protected..W.C.Act..Badgers.and.Cons.Regs....Taxon.Designated
  raw_data$Wildlife_1 <- raw_data$Wildlife...Countryside.Act.Sch.9.Part.1
  raw_data$Wildlife.. <- raw_data$Wildlife...Countryside.Act.Sch.9.Part.2
  raw_data$Taxon.grou <- raw_data$Taxon.group
  raw_data$Taxon.Lati <- raw_data$Taxon.Latin.Name
  raw_data$Obs.Commen <- raw_data$Obs.Comment
  raw_data$Sample.Rec <- raw_data$Sample.Recorders
  raw_data$Sample.Loc <- raw_data$Sample.Location.Name
  raw_data$Sample.L_1 <- raw_data$Sample.Location
  raw_data$Sample.Dat <- raw_data$Sample.Date
  raw_data$Sample.Spa <- raw_data$Sample.Spatial.Reference
  raw_data$Survey.Run <- raw_data$Survey.Run.By
  raw_data$Survey.Nam <- raw_data$Survey.Name
  raw_data$NELBAP     <- raw_data$North.East.LBAP...Short.Names
  raw_data$Taxon.Comm <- raw_data$Recommended.Common.Name
  raw_data$Obs.Abunda <- raw_data$Obs.Abundances..LC.
  raw_data$Determinat <- raw_data$Determination.Type
  raw_data$NERC       <- raw_data$England.NERC.S.41
  raw_data$Ch.Date    <- raw_data$Obs.Changed.Date
  raw_data$ImportDate <- raw_data$Obs.Entry.Date
  raw_data$Obs.Key    <- raw_data$Obs.Key

  return(raw_data)
}
