#' Find duplicates
#'
#'  Find duplicates in data.tables for DateTime, Station, Parameter~ value
#' @param dt input data.table, From GetData()
#' @return dtdups: a data.table list conatining $dups $points $mean
#' @details For tables that include DateTime, Station, Parameter, and value.
#'          Returns list dtdups:
#'          $dups: Combinations of DateTime, Station, and Parameter that produced a duplicate
#'          $points: all points that are duplicates of DateTime, Station, and Parameter
#'          $mean: Fixed dt with duplicates removed by taking average of duplicate points. (NA's removed)
#'
#' @export


DupCheck <- function( dt ) {

  dt <- copy(dt) #bc by referance data.table works by referance

  dtdups <- list()

  dtdups$dups <- dt[,.(N=.N),by=.(DateTime,Station,Parameter)][N>1]


  setkey(dtdups$dups,DateTime,Station,Parameter)
  setkey(dt,DateTime,Station,Parameter)


  dtdups$points <- dt[dtdups$dups]

  dtdups$mean <- dt[, .(value=mean(value,na.rm=TRUE)), by=.(DateTime,Station,Parameter)]
  return(dtdups[])

}
