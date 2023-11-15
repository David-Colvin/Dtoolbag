# Functions for producing hydstra SQL statements and commands (because I'm bored)
#
# Started: 6/9/2022
# David Colvin
# 1.1 created
#

#' Replace HYCSV history file with new export criteria
#'
#' By replacing HYCSV history, you can open HYCSV in hydstra, select "Use previous export" and the new export criteria will populate the user form.
#' @param stations a vector of Station Codes that match StationCode in tbl_Stations
#' @param parameters a character vector of parameters that match ParamR in tbl_Parameters with corresponding HydstraCode (Temp, DO, DOsat, pH, SpCond, Turb, Chlor, Flow, Stage).
#' @param range date range.(use MkDate)
#' @param source hydstra file source. Default is "A" for archive
#' @param type hydstra export type. Default is "POINT"
#' @param quality default is "No" (or use"Yes")
#' @param interval default is 15. Or use Daily.
#' @return Returns dtGet, data.table of stations and parameters sent to HYCSV
#' @details Function re-writes the csv file located in "Q:/Users/DCOLVIN/TEMP/PRM/HYCSV.PRM" with new hydstra station codes, parameter codes and date range for quick population of HYCSV.
#' Each combo of station and parameter is set into HYCSV
#' Can only do 6 times series at a time. (ie stations * parameters <= 10)
#' @import stringr
#' @export



set_hycsv <- function(stations,parameters,range,source="A",type="POINT",quality="No",interval="15"){

  #set WQES tbls from function
  tbl<-Dtoolbag::WQES_tbls()

  #check and set path to HYCSV.PRM file
  if(stringr::str_detect(GetPath2Rstuff(),"Users/dcolvin")){
    PRMfile <- "//10.158.193.55/Hydstra/USERS/DCOLVIN/TEMP/PRM/HYCSV.PRM" #updated from Q: drive
  } else{
    print("You are not approved to use this function. END")
    return(NULL)
  }

  #check length: HYCSV can only do 10 data series (there may be a way to do more)
  if((length(stations)*length(parameters))>10) {
    print("stations * parameters must be less than 10. END")
  }


  #make list of stations and parameters
  dtGet=data.table(
    StationCode = rep(stations,each=length(parameters)),
    ParamR = rep(parameters,times=length(stations))
  )

  #set hydstra codes (and check which parameter)
  data.table::setkey(tbl$Stations,StationCode)
  data.table::setkey(dtGet,StationCode)
  dtGet[,ID_Hydstra:=tbl$Stations[dtGet]$ID_HydstraWQ]
  dtGet[ParamR=="Flow",ID_Hydstra:=tbl$Stations[dtGet[ParamR=="Flow"]]$ID_HydstraFlow] #because flow (and velocity) are stored under a different column name in tbl_Stations
  dtGet[ParamR=="Velocity",ID_Hydstra:=tbl$Stations[dtGet[ParamR=="Velocity"]]$ID_HydstraFlow]
  dtGet[ParamR=="Elevation",ID_Hydstra:=tbl$Stations[dtGet[ParamR=="Elevation"]]$ID_HydstraFlow]
  dtGet[ParamR=="Stage",ID_Hydstra:=tbl$Stations[dtGet[ParamR=="Stage"]]$ID_HydstraSurface]

  #set parameter hystra numbers
  data.table::setkey(tbl$Parameters,ParamR)
  data.table::setkey(dtGet,ParamR)
  dtGet[,HydParam:=tbl$Parameters[dtGet]$HydstraNum]
  dtGet[,HydParam:=as.character(paste0(HydParam,".00"))]


  #use 0 for no hydstra site ID
  dtGet[ID_Hydstra=="",ID_Hydstra:= "0"]

  #order by inverse stations(puts "0"s at end)
  data.table::setorder(dtGet,-ID_Hydstra,ParamR)

  #make lines of station parameters to extract to paste into HYCSV.PRM
  lines="0"
  for(i  in 1:length(dtGet[]$Station)){
    lines[i] <- paste(c("DATA",dtGet[i]$ID_Hydstra,source,dtGet[i]$HydParam,dtGet[i]$HydParam,type),collapse=" ")
  }
  #fill any remaining lines with '0' (may not be necessary)
  if(length(dtGet[]$Station)<10){
    for(i in (length(dtGet[]$Station)+1):10){
      lines[i] <- paste(c("DATA","0",source,"262.00","262.00",type),collapse=" ")
    }
  }

  #Make sure range is datetime
  range<-Dtoolbag::MkDate(range)

  #write final line
  if(interval=="day"|interval=="Day"|interval=="daily"|interval=="Daily"){INTERVAL='TIME DAY 1.0000 0.00'}
  if(interval=="15") {INTERVAL ='TIME MINUTE 15.0000 0.00'}

  lines[11] <- paste(c(INTERVAL,format(range[1],format="%H:%M_%m/%d/%Y"),format(range[2],format="%H:%M_%m/%d/%Y"),
                       'START X No ',quality,' NO "MM/DD/YYYY HH:II"'),collapse=" ")

  #open file connection, write lines and close close connection
  con <- file(PRMfile, open="r+")
  write(lines,file=con,sep="/t")
  close(con)

  #return dtGet so we see what was done
  return(dtGet)
}


#' Read recent output from HYCSV in Hydstra drive
#'
#' Read and import recent export from HYCSV
#' @param num Default is 1. Number of files to read, IDK, i'm making this up as I go
#' @return Returns latest num files in Q:Users/DCOLVIN/TEMP/TEMP
#' @details After running HYCSV, grabs the latest file. then Use Dread
#' @import stringr
#' @export

get_HYCSV <- function(num=1){

  if(stringr::str_detect(Dtoolbag::GetPath2Rstuff(),"Users/dcolvin")){
    path <- "//10.158.193.55/Hydstra/USERS/DCOLVIN/TEMP/TEMP" #"Q:/Users/DCOLVIN/TEMP/TEMP"
  } else{
    print("You are not approved to use this function. END")
    return(NULL)
  }

  file <- Sys.glob(paste0(path,"/HYCSV_ListTemp_*"))
  files <- data.table(file,file.info(file))
  files <- files[order(-mtime)]
  files <- files$file[1:num]

  return(files)

}
