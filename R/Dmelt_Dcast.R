#' Melt standard WQES data.table
#'
#' Convert Wide table to long table. Converts from standardized [station].[parameter] column naming to Station, Parameter, value column names
#' @param dt input data.table
#' @param DateTime_col to specify either DateTime or Date, if blank assumes first column name is Time column to melt by
#' @param type if TRUE adds Type column based on input column names (ie PDC.Temp.Min)
#' @param importer if input file is from WQES_Importer use TRUE to remove duplicated rows the occure due to multi-lined comments. (comments will be removed)
#' @return a long format data.table with columns (Date/DateTime), Station, Parameter, variable, value
#' @details use this function to convert a data.table back into long format after being cast into wide format from Dcast, Dread (or other function within Dtoolbag)
#' @import stringr
#' @export
Dmelt=function(dt,DateTime_col=NULL,type=FALSE,importer=FALSE,CDEC=FALSE){
  #melts files that have gone through Dread.
  # converts a wide table format to a long table format assuming file columns are named StationCode.Parameter
  #
  #dt: data.table from Dread
  #type: if TRUE further breaks down the column name by parameter type (eg, PDC.Temp.Min or PDC.Temp.Mean   type= Min or Mean)
  #CDEC: if a dt came from CDEC, naming convetions added .CDEC to end of column name (or use to?) if this is the case, use CDEC = TRUE to keep the .CDEC as part of the Parameter value
  #importer: if input file is from WQES_Importer use TRUE to remove duplicated rows the occur due to multi-lined comments. (comments will be removed)

  if(is.null(DateTime_col[1])){DateTime_col<-names(dt)[1]} #assumes first column is a Date or DateTime

  #melt(dt,id.vars=DateTime_col,measure.vars=)->mdt   #i think this was an error?
  data.table::melt(dt,id.vars=DateTime_col)->mdt #assumes every other column is a measurement variable
  mdt[,Station:=str_match(variable,"^([A-Za-z]{2,16}[0-9]{0,1})[\\.|_]")[,-1]]
  mdt[,Parameter:=str_match(variable,"^[A-Za-z]{2,16}[0-9]{0,1}\\.([A-Za-z%]{2,9})\\.{0,1}|$")[,-1]]
  if(CDEC){mdt[,Parameter:=str_match(variable,"^[A-Za-z]{2,16}[0-9]{0,1}\\.([A-Za-z%\\.]{2,12})$")[,-1]]}
  #if(DELi){mdt[!str_detect(variable,"^i\\.")]->mdt}
  if(type){mdt[,Type:=str_match(variable,"\\.([A-Za-z]{1,7})$")[,-1]]}
  if(importer){mdt[,.(value=first(value),Station=first(Station),Parameter=first(Parameter)),by=.(DateTime,variable)]->mdt} #with importer files, rows are repeated (for additional comment rows) This removes the duplicate rows (note: comments will not be included)

if(FALSE){
  setkey(mdt,DateTime,Station,Parameter)
  qdt <- mdt[stringr::str_detect(variable,"Qual$")]
  mdt <- mdt[!stringr::str_detect(variable,"Qual$")]
  if(length(qdt$DateTime)>0){
    qdt[,Quality:=value]
    qdt[,value:=NULL]
    qdt[,variable:=NULL]
    setkey(qdt,DateTime,Station,Parameter)
    mdt <- mdt[qdt]
  }
}#trying to match the quality points but it causes duplicates, may want to revisit, but no one cares about quality codes
  mdt <- mdt[!stringr::str_detect(variable,"Qual$")]

  return(mdt[])
}#melts files that have gone through the Dread process

#' Cast standard WQES data.table
#'
#' Convert Long table to Wide table. Converts from standardized column naming of (Date/DateTime), Station, Parameter, value to columns named as [station].[parameter]
#' @param dt input data.table
#' @param value.var (default="value") column name to cast (ie populate columns of Station.Parameter)
#' @return a wide table format data.table
#' @details use this function to convert a data.table from GetData or Dmelt into a wide format.
#' @import stringr
#' @export

Dcast=function(dt,value.var="value"){
  #converts Standard Dmelt long format table to wide table similare to Dread
  if(any(str_detect(names(dt),"DateTime"))){
    data.table::dcast(dt,DateTime~Station+Parameter,value.var=value.var,sep=".")->dtw
  }else if(any(str_detect(names(dt),"Date"))){
    data.table::dcast(dt,Date~Station+Parameter,value.var=value.var,sep=".")->dtw
  }
  return(dtw[])
}
