#' Combine data.tables
#'
#' Combine data.tables that have come from GetData
#' @param ldt First data.table (or list of data.tables, in which case all tables will be combined)
#' @param dt2 Second data.table. Should not be a list.
#' @return Single data.table. All columns are considered Keys, so rows with all matching columns will be combined.
#' @details Merges two data tables on shared columns
#' @import stringr
#' @export

Dmerge <- function(ldt,dt2=NULL){

  if( !class(ldt) == "list"){
    ldt <- list(ldt)
  } #convert input to list if it isn't already


  if( !is.null(dt2) ) { ldt[[length(ldt)+1]] <- dt2} # add dt2 to list of data.tables

  ldt <- lapply( ldt, data.table::setkey) # set keys for each data.table in the list


  #DC20231130 return( Reduce( function(...) merge(..., all = TRUE), ldt))
  return( data.table::as.data.table(Reduce( function(...) merge(..., all = TRUE), ldt)))#DC20231130
}
