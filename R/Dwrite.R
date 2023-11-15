#' Write data.table to CSV file
#'
#' write data.tables to working directory (or Rstuff/data_files)
#' @param file file name (include path if different from working directory or data_files)
#' @param nRstuff Default is FALSE, and writes to working directory. if data=TRUE writes to Rstuff/data_files
#' @return absolute path to file
#' @details Times are easily read in excel and can be read back in using fread and date time column converted using MkDate()
#' @import stringr
#' @export


Dwrite = function(dt,file="",nRstuff=FALSE){
  #dt:  data.table to write
  #path: folders/subfolders within data_files in which to write
  #file: file name
  #nRstuff:(TRUE) default location is data_files
  #ExcelDate: will out put dates in an excel friendly format, not currentily used or necessary






  if(stringr::str_detect(file,":/")){nRstuff=FALSE} #means an absolute path was entered for File name
  if(nRstuff){
    Path2Rstuff<-GetPath2Rstuff()
    file <- paste0(Path2Rstuff,"/data_files/",file)
    }else {file <- paste0(getwd(),"/",file)
  }
  path=stringr::str_match(file,"(.*)/[A-Za-z0-9 -_\\+\\.]{0,50}$")[2] #should match everything to last /
  if(!file.exists(path)){dir.create(file.path(path),recursive=TRUE)}

  if(!stringr::str_detect(file,"(\\.csv|\\.CSV)$")){file=paste0(file,".csv")}


fwrite(dt,file=file,dateTimeAs="write.csv") #added "write.csv" because fwrite default forces timezone as UTC
  print(paste0("file: ",file))
  return(file)
}#writes data.tables in a consistant format
