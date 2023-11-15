# 1.1 create function Dreadfolder
# 1.2 trying to make more robust

#' Read all files in a folder
#'
#' Created so files could be downloaded from new WDL, placed in a folder, and combined in R. Dreadfolder() will read and combine all files in user folder Documents/Rstuff/data_files/Combine
#' @param folder folder name to read. (default is 'Combine' in Rstuff/data_files).
#' @param nRstuff is folder relative to Rstuff/data_files/? (use TRUE) Else, relative to working directory
#' @return dt with all data
#' @details Files must work with function Dread, which generally means file was downloaded from WDL or Hydstra.
#'  If file contains multiple stations or parameters, Quality codes will be erased (v1.1).
#'  Note: Dread default of time15min=TRUE is used.
#' @import stringr
#' @export

DreadFolder <- function(folder = "Combine", nRstuff = TRUE, ErrorCheck = FALSE){

  files <- Files4R(folder,"*.csv",nRstuff=nRstuff)

  if(length(files)==0) {
    if(nRstuff){
      path = paste0(GetPath2Rstuff(),"/data_files/",folder)
      print(paste("No files, or folder to combine not found.  ",path))
    }else {
      path = paste0(getwd(),"/",folder)
      print(paste("No files, or folder to combine not found.  ",path))
    }

    return(NULL)

  }else {



    DT=NULL
    for( i in 1:length(files)){
      print(paste(i,'/',length(files),"   ",files[i]))
      DT2 <- Dread(files[i],ErrorCheck=ErrorCheck)
      if(!is.null(DT2)){
        if(any(stringr::str_detect(names(DT2),"Qual"))){
          if(length(names(DT2))==3){
            #standard file from WDL (ie only one station/parameter with a quality code)
            station = stringr::str_match(names(DT2)[2],"^([A-Za-z0-9]{1,6})\\.")[2]
            parameter = stringr::str_match(names(DT2)[2],"\\.([A-Za-z0-9]{1,20}$)")[2]
            names(DT2)[c(2,3)]<-c("value","Quality")
            DT2[,Station:= station]
            DT2[,Parameter:= parameter]
          }else{
            rm(DT2) #probably I should just remove the relevant columns, but right now this is easier
            print(paste(paste(" Quality Codes will be removed from file: ",files[i])))
            DT2 = Dmelt(Dread(files[i],quality=FALSE))
          }
        }

        if(is.null(DT)){
          DT<-DT2
        }else {
          DT<-merge(DT,DT2,all=TRUE)
        } #merge not part of data.table?

        rm(DT2)
      }#!is.null(DT2)
    }

    DT<-DT[,c(1,5,4,2,3)]

    return(DT)
  }
}
