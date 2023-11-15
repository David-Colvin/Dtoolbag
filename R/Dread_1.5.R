#V1.3 includes a check for reading in Discrete data
#v1.4 adding new WDL format
#V1.5 changed quality code variable type to character because new WDL spits out quality like "1: Good Data"

#' Read in common files for WQES
#'
#' Reads in common file types and converts them to a standard format data.table (column names: DateTime, StationCode.Parametercode)
#' @param file file name and path you want to read.
#' @param time15min TRUE replaces times to an even 15 minute interval (ie 10:16:18 -> 10:15:00)
#' @param quality (TRUE/FALSE) Should data quality column be kept? Default is TRUE
#' @param comments (TRUE/FALSE) Should comments column be kept? Default is TRUE
#' @param from (one of: "file","HYD","WDL",or "Importer") Automagically determined in function.
#' @param station Set station code for files not in the WQES referances.
#' @param nRstuff If TRUE, file path starts ar Rstuff/data_files/. IF FALSE file name should be absolute
#' @return data.table with column names in the form DateTime,(StationCode).(ParameterAbreviation)
#' @details If crashes try using fread()
#' @import stringr
#' @export

Dread = function(file,time15min=TRUE,quality=TRUE,comments=TRUE,station=NULL,from=NULL,nRstuff=FALSE,filter=NULL,ErrorCheck=FALSE){
  #function to read a file and return a data.table with column names as (StationCode).(ParamR)  see tbl_Stations and tbl_Parameters for values of StationCode and ParamR
  #file: file path and name you want to read. (use nRstuff=TRUE for just file name)
  #nRstuff: If nRstuff = TRUE, path starts at Documents/Rstuff/data_files/. nRstuff=FALSE file name is assumed to be absolute
  #time15min: TRUE replaces times to an even 15 minute interval (ie 10:16:18 -> 10:15:00)
  #
  #
  #     from:("file","HYD","WDL")   NO LONGER USED IN INPUT
  #     if file is a raw file (not yet run through Dwrite)  HYD=Hydstra or WDR, USGS


  #set tbl_ referances
  if(ErrorCheck){print("ER 1")}
  WQES_tbls()->tbl
  if(ErrorCheck){print("ER 2")}


 # if(is.null(from)){from="file"}

  if(nRstuff){file=paste0(GetPath2Rstuff(),"/data_files/",file)} #for file name relative to Documents/Rstuff/data_files

  #Determine file type based on input file structure. Default is "file"
  if(is.null(from)){
    if(ErrorCheck){print(c("ER 3: ",file))}
    #should go back and make this cleaner
    if(scan(file,skip=1,nline=1,what=character(),sep=",",quiet=TRUE)[1]=="and"){
      if( !is.na(scan(file,skip=6,nline=1,what=character(),sep=",",quiet=TRUE)[4]=="Variables:") &
        scan(file,skip=6,nline=1,what=character(),sep=",",quiet=TRUE)[4]=="Variables:"){
        from="WDL"
      }else {from="HYD"}
    }
    if(ErrorCheck){print("ER 4")}
    if(ErrorCheck){print("ER 4.0")}
    if(length(scan(file,skip=0,nline=1,what=character(),sep=",",quiet=TRUE))>=6){
      if(ErrorCheck){print("ER 4.1")}
      if(scan(file,skip=0,nline=1,what=character(),sep=",",quiet=TRUE)[6] == "Analyte"){from="Discrete"}
    }
    if(ErrorCheck){print("ER 4.2")}
    if(scan(file,nline=1,what=character(),sep=",",quiet=TRUE)[1]=="# ---------------------------------- WARNING ----------------------------------------"){
      from="USGS"

    }
    if(scan(file,nline=1,what=character(),sep=",",quiet=TRUE)[1]=="Sample Owner" | scan(file,nline=1,what=character(),sep=",",quiet=TRUE)[1]=="LongStationName"){
      from="Discrete" #note: there are different file structures that are coming from WDL, these options might not be all incompassing
    }
  }
  if(ErrorCheck){print("ER 4.3")}

  #v1.4 search and detect new WDL format v1.4
  if(is.null(from)){
    if( stringr::str_detect(scan(file,skip=1,nline=1,what=character(),sep=",",quiet=TRUE)[1], "^#Station Name:")){
      from="WDL_21"
    }
  }



  #set a default if still not set
  if(is.null(from)){from="file"}

  if(from=="file"){heads= scan(file,nline=1,what=character(),sep=",",quiet=TRUE)}

  if(from=="HYD"|from=="WDL"){
    heads=HYD_ColNames(file)
    station=scan(file,nline=1,what=character(),sep=",",quiet=TRUE)[2]
    station<-tbl$Stations[ID_HydstraWQ==station|ID_HydstraFlow==station]$StationCode
    if(length(station)>1){print("Warning! Error, Hydstra ID found matching multiple station Codes")}
  }

  if(from =="WDL_21"){

    #check if its just an empty file
  if(length(scan(file,nline=1,what=character(),skip=9,sep=",",quiet=TRUE))==0){
     print(paste0("Dread: Empty file ",file))
    return(NULL)
   }


    parameter <- tbl$Parameters[WDL_Param==
                                  stringr::str_match(scan(file,skip=3,nline=1,what=character(),sep=",",quiet=TRUE)[1], "^#Parameter:(.*)$")[2]
                                ]$ParamR

    if(parameter=="Flow"){
      station <- tbl$Stations[ID_HydstraFlow==
                                stringr::str_match(scan(file,skip=0,nline=1,what=character(),sep=",",quiet=TRUE)[1], "^#Station Number:(.*)$")[2]
                              ]$StationCode
    } else{
      station <- tbl$Stations[ID_HydstraWQ==
                                stringr::str_match(scan(file,skip=0,nline=1,what=character(),sep=",",quiet=TRUE)[1], "^#Station Number:(.*)$")[2]
                              ]$StationCode
    }

    if(!length(scan(file,skip=8,nline=1,what=character(),sep=",",quiet=TRUE))==3){print("Warning: file does not have three columns")}

    heads <- c("DateTime",paste0(station,".",parameter),paste0("Qual")) #note:changing name later
    types <- c("character","numeric","character")
    Xdt <- data.table::fread(file,sep=",",header=FALSE,col.names=heads,skip=9,colClasses=types,verbose=FALSE)
   # Xdt[ ,Qual:=stringr::str_match(Qual,"^([0-9]{1,3})")[-1]  ]
    names(Xdt)[3] <- paste0(station,".Qual")

  }

  if(ErrorCheck){print("ER 4.4")}
  if(from=="Importer"){heads= scan(file,nline=1,what=character(),sep=",",quiet=TRUE)
    if(!is.null(station)){heads[2:length(heads)]=paste0(station,".",heads[2:length(heads)])}
  }



  if(from=="Discrete"){
    Xdt <- DreadDiscrete(file=file, simple=TRUE, filter=filter)
    return(Xdt)
    }

  if(ErrorCheck){print("ER 5")}
  #setting column types to avoid warnings from fread (still has warnings, so I need to address that someday)
  types="numeric"
  types[1]="character"
  for(i in 2:length(heads)){
    if(str_detect(heads[i],"_Qual|\\.Qual")){types[i]="character"   #V1.5, changing type integer to character because of new WDL (can convert to integers later)
    }else if(str_detect(heads[i],"_Com|Comment|\\.$")){types[i]="character"
    }else {types[i]="numeric"}
  }
  if(ErrorCheck){ print( c( "ER 6 from: ", from )) }
  if(from=="HYD") {Xdt=data.table::fread(file=file,sep=",",header=FALSE,col.names=heads,skip=3,colClasses=types,verbose=FALSE)}
  if(from=="WDL") {
    if( ErrorCheck ){ print("ER 7")}
    Xdt=data.table::fread(file,sep=",",header=FALSE,col.names=heads,skip=3,colClasses=types,verbose=FALSE)
    if( ErrorCheck ){ print("ER 8") }
    Xdt=Xdt[,-4]   #deleting notes
    if( ErrorCheck ){ print("ER 9") }
  }
  if(from=="file") {Xdt=data.table::fread(file,sep=",",header=FALSE,col.names=heads,skip=1,colClasses=types,verbose=FALSE)}
  if(from=="Importer"){
    Xdt=data.table::fread(file,sep=",",header=FALSE,col.names=heads,skip=2,colClasses=types)
  }#not completed
  if( ErrorCheck ){ print("ER 10.0") }
  if(from=='USGS'){
    print("Note:Dread file from = USGS. Dread not configured for USGS yet. Useing fread(file)")
    Xdt <- suppressWarnings(fread(file)) #note: this relies on data.table internal format checking... may change
    names(Xdt) <- as.character(as.vector(Xdt[1]))
    Xdt<-Xdt[!1:2]
    return(Xdt)
  }
  if( ErrorCheck ){ print("ER 10.1") }
  #setting Date/DateTime
  heads[1]->DateCol #assuming first column is a date or date time column
  DateForm=DateFormat(Xdt[1,get(DateCol)])
  if(!str_detect(DateForm,"H")){
    names(Xdt)[1]<-"Date"
    DateCol="Date"
    #should be caught in MkDate    time15min=FALSE
  }#Checking if Date or DateTime column. causes crash if TimeFIx is used with a date column

  if(ErrorCheck){print("ER 11")}
  Xdt[,(DateCol):=MkDate(as.character(get(DateCol)),time15min=time15min)] #converting to POSIXct with timezone PST

  #removing Quality Column and/or Comment Column if quality=FALSE or comments=FALSE (Default is TRUE and both columns will remain)
  if(!quality){names(Xdt)[str_detect(names(Xdt),"_Qual|\\.Qual")]->Qualcol
    Xdt[,Qualcol[1:length(Qualcol)]:=NULL]}
  if(!comments){names(Xdt)[str_detect(names(Xdt),"_Com|\\.Comment")]->Comcol
    Xdt[,Comcol[1:length(Comcol)]:=NULL]}

  return(Xdt[])  #returns data.table

}#reads raw data files either from Hydstra, WDL, or pre-configured files




HYD_ColNames=function(file){
  #combines the three line headers from HYD files and WDL files into column names
  #create file name  format (WQES Code)_(starting)YYYYMMDD_(total days)(hydstra export type).csv   example PDC_20171001_365A.csv
  #name columns

  #set tbl_ referances
  WQES_tbls()->tbl

  header1 = scan(file,nlines=1,what =character(),sep=",",quiet=TRUE)
  header2 = scan(file,skip=1,nlines=1, what=character(),sep=",",quiet=TRUE)
  header3 = scan(file,skip=2,nlines=1,what=character(),sep=",",quiet=TRUE)

  #this is stupid. confusing script to check each line and create a station vector that has the StationCode for each column (paying attention to possibility of multiple stations)
  station="blank"
  for(i in 2:length(header1)){
    if(!stringr::str_detect(header1[i],"^[BG]{1}[0-9]{5,7}")){station[i-1]=station[i-2]
    }else {
      #station[i-1]=
      #  WQStations(header1[i])
      if(length(tbl$Stations[ID_HydstraWQ==header1[i]|ID_HydstraFlow==header1[i]]$StationCode)>0){
        station[i-1]=tbl$Stations[ID_HydstraWQ==header1[i]|ID_HydstraFlow==header1[i]]$StationCode
      }else{station[i-1]=header1[i]}}
  }

  heads="DateTime"

  #if(header3[length(header3)]=="Comments"){num=length(header3)   trying to deal with random end column in data from WDL
  #}else{num=length(header3)-1}
  num=length(header3)
  for(i in 2:num){
    heads[i]=paste0(station[i-1],".")
    if(stringr::str_detect(header2[i],"[0-9]{3,4}")){
      heads[i]=paste0(heads[i],tbl$Parameters[HydstraNum==as.numeric(header2[i]) & !ParamR==""]$ParamR) #previously there was a ,"Param" in the paste?
    }#change from Hydstra vairable codes to parameter names

    if(header3[i]=="Qual"){heads[i]=paste0(heads[i-1],"_Qual")}
    if(header3[i]=="Comments"){heads[i]=paste0(heads[i-2],"_Com")}

  }


  return(heads)
}#to decifer hydstra and WDL column naming conventions




