#File to read Kor Calibration files
# created 20220310

#' Read KOR calibration data files
#'
#' Reads Kor calibration data files
#' @param file file name and path you want to read.
#' @param parameters parameters to filter
#' @param range date range to filter
#' @return list of two data.tables, Kor Cals and Cal points
#' @details I dont know
#' @import stringr
#' @export


DreadKorCal <- function(file, parameters = NULL, range = NULL){
  #creates two tables, calibrations, and cal points linked by DateTime_InstrumentID
  #
  ##testing
  #file <- "Z:/Water Quality Evaluation Section/Data Files/YSI FILES/WORKING FILES/Calibration File Export - 031022 113759.csv"

  #read lines and convert to data.table
  lines <- readLines(file)
  dt <- data.table::data.table(lines)

  #split by comma
  dt[, c("variable", "value") := tstrsplit(lines, "=,", fixed=TRUE)]

  #remove spaces
  dt[,variable:= stringr::str_replace_all(variable," ","")]

  #idintify calibrations
  dt[,CalLine:=0]
  dt[variable=="LastCalibrationTime", CalLine:=1]
  dt[,CalNum:=cumsum(CalLine)]

  #identify cal points by Cals
  dt[,CalLine:=0]
  dt[stringr::str_detect(variable, "\\[CalPoint*"), CalLine:=1]
  dt[,CalPoint:=cumsum(CalLine),by=CalNum]

  #clean up
  dt[,CalLine:=NULL]
  dt[,lines:=NULL]

  dt <- dt[!CalNum==0 & !is.na(variable) & !variable =="----------"]


  #split tables
  dtCals <- dt[CalPoint==0]
  dtPts <- dt[!CalPoint==0]

  #convert to wide format
  dtCals <- dcast(dtCals[!is.na(variable)], CalNum~variable, value.var="value")
  dtPts <- dcast(dtPts[!stringr::str_detect(variable, "\\[CalPoint*")], CalNum + CalPoint~variable, value.var="value")

  #CalID
  dtCals[,CalID:=paste0(format(MkDate(CalibrationStartTime),"%Y%m%d_%H%M_"),SensorSerialNumber,"_",InstrumentName)]

  dtID <- dtCals[,.(CalID,CalNum)]

  setkey(dtID,CalNum)
  setkey(dtPts,CalNum)

  dtPts <- dtID[dtPts]

 #convert numbers
 dtPts[,Units:= str_match(PostCalibrationValue," (.*)")[,2]]
 dtPts[,PostCalibrationValue:=stringr::str_match(PostCalibrationValue,"([^\\s]+)")[,2]]
 dtPts[,PreCalibrationValue:=stringr::str_match(PreCalibrationValue,"([^\\s]+)")[,2]]
 dtPts[,Standard:=stringr::str_match(Standard,"([^\\s]+)")[,2]]

 #i dont know what Raw value is   dtPts[,RawValue:=stringr::str_match(RawValue,"([^\\s]+)")[,2]]

 dtPts[,Temperature:=stringr::str_match(Temperature,"([^\\s]+)")[,2]]

 setkey(dtCals,CalID)
 setkey(dtPts,CalID)


 #set class types to numeric
 dtPts[,PreCalibrationValue:=as.numeric(PreCalibrationValue)]
 dtPts[,PostCalibrationValue:=as.numeric(PostCalibrationValue)]
 dtPts[,Standard:=as.numeric(Standard)]
 dtPts[,RawValue:=as.numeric(RawValue)]
 dtPts[,Temperature:=as.numeric(Temperature)]


 ldt <- list(dtCals,dtPts)
 names(ldt) <- c("Cals","CalPts")
 return(ldt)
}

