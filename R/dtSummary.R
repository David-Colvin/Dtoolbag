# Summarize data in a dt (as I like to construct them)
# 6/22/2022

#' Summarize a data.table as made by Dmelt(Dread)
#'
#' Summarize a data.table as made by Dmelt(Dread) to show Max's Min's Range and value count
#' @param dt a data.table
#' @return Returns data.table of summary
#' @details Files should be melted files ran through Dread
#' @import stringr
#' @export

dtSummary <- function(dt){

  #could make more interesting...
  dtsum <- dt[,.(Start=min(DateTime),End=max(DateTime),Max=max(value,na.rm=TRUE),Min=min(value,na.rm=TRUE),.N),by=.(Station,Parameter)][order(Station)]

  return(dtsum)
}

