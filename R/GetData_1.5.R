# Version and Update Notes -----------------------------------------------------------------------------------
# 1.2 changing so no default parameters
# 1.4 updating to (hopefully) work with new WDL website
#     edited source=USGS to convert DateTime using MkDate and to not attempt downloading of missing USGS station numbers
# 1.4.1 adding change of hydstra station code for downloading stage data from WDL
#     Error? noticed that when entering range=range when calling function range (external to function) gets converted to the downloaded data? weird
# 1.4.2 Error checking and improving (noticed issue of returning a zero data.table when there should be some data)
# 1.5 rewriting WDL to work with new hystra web services (now gets downloaded as a json)

#' Download Water Quality Data
#'
#' Downloads water quality data for stations and parameters that have been set up in WQES_database tbl_Stations and tbl_Parameters. Sources include CDEC, WDL, and USGS
#' @param stations a vector of Station Codes that match StationCode in tbl_Stations (or see dtGet)
#' @param parameters a character vector of parameters that match ParamR in tbl_Parameters (Temp, DO, DOsat, pH, SpCond, Turb, Chlor, Flow, Stage(only avaiable from CDEC)) Default is "All".
#'                   For source="WDL", field HydstraNum in tbl_Stations must be present for specified ParamR row. For source="CDEC", CDECNum in tbl_Parameters must be present, and for source="USGS", USGSNum must be present.
#' @param range date range. For best results specify Start and End date with time and use MkDate() (default=NULL and if WDL will return current year, CDEC returns past 10 days, USGS returns past 365 days).
#' @param source Source to download data ("CDEC", "WDL", or "USGS") Default is "CDEC". WDL requires field ID_HydstraWQ in tbl_Stations for given StationCode (or ID_HydstraFlow for Flow data). USGS requires ID_USGS for given StationCode. (or see dtGet)
#' @param duration used to specify CDEC data range when range is not entered
#' @param POR (deafult FALSE) Download Period Of Record from WDL as daily average values. Note: any day with one or more 170 or 151 points will not be included
#' @param replace (default TRUE) For WDL and USGS, new download files will replace prious ones. If False, will first check for existing file and not replace it.
#' @param type (Default "15Minute" or "Raw" for WDLnew) For USGS other type is "Daily" but is untested.. For New WDL 15minute is Raw or use Daily_Mean.
#' @param time15min (unused)
#' @param ErrorCheck (Default is FALSE)
#' @param report (Default FALSE) Shows table of downloaded files and failed files at end.(dtGet)
#' @param dtGet to directly specify the Station, Parameter, and ID combinations and bypass setting stations,parameters.
#'              if source = WDL, dtGet must have columns (StationCode, ParamR, ID_Hydstra, Year)
#'              if source = CDEC, (dtGet cannot be entered)
#'              if source = USGS, dtGet must have columns (StationCode, ParamR, ID_USGS, Start, END)
#' @return Returns a data.table long format of downloaded data.
#' @details For WDL and USGS, raw files are downloaded to Documents/Rstuff/data_files. Post processing of data is required, use DupCheck() to find duplicates.
#' @import stringr
#' @import data.table
#' @import httr
#' @import jsonlite
#' @export

GetData=function(stations,parameters,range=NULL,source="WDL",duration=NULL,
                 stationAS="StationCode",POR=FALSE,replace=TRUE,type="15Minute",time15min=TRUE,
                 ErrorCheck=FALSE,report=FALSE,dtGet=NULL){
  #Returns a data.table
  #
  #source: WDL, CDEC, (future:USGS)
  #stations: list of stations to download data from (default is listed as StationCode in tbl_Stations)
  #parameters: Defines Paramaters to be downloaded (see must match ParamR's in tbl_Parameters)
  #range: Defines the range for data.    two value vector, c(Start,End) (use MkDate)
  #         Either Null (defaults to WDL, current year or CDEC most reacent 5 days. OR single (see duration)
  #duration: if range is single value, duration defines the end (or start) of data
  #
  #project: not used. Sets a project Name (used in Rstuff organization)
  #
  #stationAS: not used. Sets how input stations is defined. Default is StationID (ie CDEC code)
  #POR: Not used.  use for Period of Record from WDL, returns Daily average
  #type: For USGS only, Deafult is "15Minute", Other option is Daily, but will probably not work
  #replace: if true a file that has already been downloaded will be replaced (ie re-downloaded)
  #
  #report: True will use View(dtGet)
  #ErrorCheck: if True will use View(dtGEt)

  #notes
  #quality code 151 (missing) and 255(?) is automatically removed (see end of WDL section) because it causes a duplicate time stamp with time15min=TRUE

  #set tbl_ referances
  tbl <- WQES_tbls()

  # WDL ---------------------------------------
  if(source=="WDL"){  #because each source has different requirements and is set up in different ways
    print("WDL website now using hydstra web services. Visit: https://wdlhyd.water.ca.gov/")
    if(is.null(dtGet)){
      #default range is  10/1/ (previous year) to current date
      if(is.null(range)){
        if(POR){Years="POR"
                print("WArning! not working!")
        }else {
          if(length(range)==1){
            range[2]=Sys.Date()
          }else {
            #                                                                             testing because it switches to PDT
            range<-MkDate(c(paste0(as.character(as.numeric(format(Sys.time(),"%Y"))-1),"10010000"),paste0(as.character(format(Sys.time(),"%Y%m%d")),"0000")))
          }
        }
      }
      # Specify Parameters
      if(parameters[1]=="All"){
        print("Warning: parameters=All will take awhile") #probably should use the WDL_SiteTraces function...

          parameters<-tbl$Parameters[!WDL_Param==""|Parameter=="Flow_CFS",]$ParamR

      }

      if(ErrorCheck){print(c("ErrorCheck:",parameters," Stations: ",stations))}

      #Sets all files to download (every unique station, parameter combo)
      dtGet=data.table(
        ParamR = rep(parameters,times=length(stations)*(1)),
        StationCode = rep(stations,each=length(parameters)*(1))
      )

      #Specify Parameter names for WDL hystra web services (ie, traces)
      data.table::setkey(tbl$Parameters,ParamR)
      data.table::setkey(dtGet,ParamR)
      dtGet[,WDL_Param:=tbl$Parameters[dtGet]$WDL_Param]

      if(ErrorCheck){View(dtGet)}

      #sets ID_Hydstra and (ID_HydstraWQ for WQES data and ID_HydstraFlow for Flow data where available)
      data.table::setkey(tbl$Stations,StationCode)
      data.table::setkey(dtGet,StationCode)
      dtGet[,ID_Hydstra:=tbl$Stations[dtGet]$ID_HydstraWQ]
      dtGet[ParamR=="Flow",ID_Hydstra:=tbl$Stations[dtGet[ParamR=="Flow"]]$ID_HydstraFlow] #because flow (and velocity) are stored under a different column name in tbl_Stations
      dtGet[ParamR=="Velocity",ID_Hydstra:=tbl$Stations[dtGet[ParamR=="Velocity"]]$ID_HydstraFlow]
      dtGet[ParamR=="Stage",ID_Hydstra:=tbl$Stations[dtGet[ParamR=="Stage"]]$ID_HydstraSurface]

      dtGet[ID_Hydstra=="",ID_Hydstra:=NA]

      #correct for entering in Hydstra codes instead of StationCodes (probably needs some clean up)
      dtGet[is.na(ID_Hydstra)&stringr::str_detect(StationCode,"^[BG]{1}[0-9]{5,7}"),ID_Hydstra:=StationCode]

      #Another way in case using obscure hydstra codes
      if(stationAS=="ID_Hydstra"){
        dtGet[,ID_Hydstra:=stations]
      }

      #add range
      dtGet[,Start:=range[1]]
      dtGet[,End:=range[2]]

      dtGet[,Status:="Pending"]
      dtGet[,url:=url <- paste0('https://wdlhyd.water.ca.gov/hydstra/sites/',ID_Hydstra,'/traces/',paste0(WDL_Param,".Raw"),'/points?start-time=',Start,'&end-time=',End)
      ]



    } #bc I may want to enter my own dtGet at some point

    # Downloading-----------------------------------------------------------------------------------------------------
    print(paste0("Downloading files ( ",length(dtGet$ParamR), " )" ) )

    ldt<-list()
    #ldt[[length(dtGet$ParamR)+1]]="End" #bc if last element is NULL, list is resized to last valid element
    print(paste0("Downloading: ",length(dtGet$ParamR)))
    for(i in 1:length(dtGet$ParamR)){ #download all Variables station combos
      cat(paste0(i," "))

      #dtGet[i,File:= WDL_downlo_v3(dtGet[i]$ID_Hydstra,dtGet[i]$ParamR,dtGet[i]$Year,replace=replace,ErrorCheck=ErrorCheck)]

      ldt[[i]]<-WDL_SiteTraceData(hydstra_id=dtGet[i]$ID_Hydstra, param=dtGet[i]$WDL_Param, range=MkDate(c(dtGet[i]$Start,dtGet[i]$End),ErrorCheck=ErrorCheck),type="RAW",ErrorCheck=ErrorCheck)
      #if(length(ldt[[i]])>0){ldt[[i]]<-"No Data"}
      dtGet[i,Status:="Downloaded"]
      print(i)

    } #download all Variables station combos
    print("")
    if(ErrorCheck){print("Done downloading")}
    if(FALSE){ldt_save <- ldt}

    #initialize DT as first non error ldt
    #    suppressWarnings(Dread(dtGet[!str_detect(File,"Error")]$File[1],quality=TRUE,time15min=time15min))->DT
    #    names(DT)[c(2,3)]<-c("value","Quality")
    #    DT[,Station:=dtGet[!str_detect(File,"Error")]$StationCode[1]]
    #    DT[,Parameter:=dtGet[!str_detect(File,"Error")]$ParamR[1]]
    #    setkey(DT)



    # combine all files into DT------------------------------------------------

    print(paste0("Compiling files ( ",length(dtGet$ParamR), " )" ) )
    for(i in 1:length(dtGet$ParamR)){
      cat(paste0(i," "))

      if(length(ldt[[i]])>1){ #if the list is NULL length(ldt[[i]]) should be 0, if empty should be 1
        ldt[[i]]<-data.table::data.table(ldt[[i]])
        names(ldt[[i]])<-c("DateTime","value","QC","d")
        ldt[[i]][,DateTime:=MkDate(DateTime)]
        ldt[[i]][,Station:=dtGet[i]$StationCode]
        ldt[[i]][,Parameter:=dtGet[i]$ParamR]
        ldt[[i]][,Source:="WDL"]
        ldt[[i]]<-ldt[[i]][,.(DateTime,Station,Parameter,value,QC,d,Source)]
        dtGet[i,Status:="Proccessed"]
      }else {
        dtGet[i,Status:="No data"]
        #ldt[[i]]=NULL
        }
    }


    print("")

    #combine list of datatables
    #remove NULL elements of list
    #ldt <- ldt[!sapply(ldt, is.null)]
    #ldt <- ldt[!sapply(ldt,is.list)]

    # Logical condition to identify non-empty, non-NULL elements
    #condition <- !(sapply(ldt, is.null) | (sapply(ldt, is.list) & sapply(ldt, length) == 0))
    #condition <- !(sapply(ldt,stringr::str_detect(string= ldt,pattern="$No ")))
    condition <- suppressWarnings(stringr::str_detect(ldt,pattern="^No"))



    # Apply the condition
    ldt_data <- ldt[!condition]



    DT=NULL
    DT<-Dmerge(ldt_data)


    if(report|ErrorCheck){View(dtGet)}


      return(DT[])

      if(ErrorCheck){print("Completed GetData")}

  }#ends WDL source


  #CDEC ----------------------------------------------------------
  if(source=="CDEC"){
    if(ErrorCheck) { print("source: CDEC")}
    #if no range entered, default is current time and 5 days previous
    if(is.null(duration)){duration=4}
    if(is.null(range)){
      range=c(MkDate(Sys.time())-duration*(60*60*24),MkDate(Sys.time()))
    }else {
      if(ErrorCheck) {print("range not NULL")}
      if(class(range[1])[1]=="Date") {
        if(ErrorCheck){print(paste0("Date to Datetime: ",paste0(as.character(range)," 00:00") ))}
        range=MkDate(paste0(as.character(range)," 00:00"))#because entering date scews things up
        if(ErrorCheck){print(paste0("Post date conversion"))}
        }

    }
    if(length(range)==1){range=c(range,MkDate(range)-duration*(60*60*24))}
    range=sort(range)


    if(parameters[1]=="All"){
      print("Warning: parameters=All will take awhile")
      parameters<-tbl$Parameters[CDECNum>0&!ParamR=="",]$ParamR
    }

    #set download combinations
    dtGet=data.table(
      ParamR = rep(parameters,times=length(stations)),
      StationCode = rep(stations,each=length(parameters)))
    dtGet[,Start:=range[1]]
    dtGet[,End:=range[2]]


    ldt=list() #initialize list
    numi=length(dtGet$ParamR)
    for(i in 1:numi){
      print(paste0(i,"/",numi,"  ",dtGet[i]$StationCode,".",dtGet[i]$ParamR))
      ldt[[i]]<-CDEC_downlo_v2(dtGet[i]$StationCode,dtGet[i]$ParamR,range,ErrorCheck=ErrorCheck)
      if(is.na(ldt[[i]]$DateTime[1])){
        dtGet[i,Error:="Error"]
      }else{

        #ldt[[i]][is.na(Flag),Flag:=""]#testing because otherwise things get screwy

        if(dtGet[i]$ParamR=="Temp"){
          print("Converting Temperature F to C")
          ldt[[i]][,value:=round((value-32)*(5/9),2)] #note that I'm rounding. Two digits is probably wrong somehow
        }
      }
    }

    DT=NULL
    #combile DT from all non empty ldt's
    for(i in 1:length(dtGet$ParamR)){
      cat(paste0(i," "))
      if(!is.na(ldt[[i]]$DateTime[1])){
        if(is.null(DT)){#find first none NA data
          DT=ldt[[i]]
          data.table::setkey(DT)
        }else {
          DT2=ldt[[i]]
          data.table::setkey(DT2)
          DT <- merge(DT,DT2,all=TRUE)  #i believe merge is from data.table but there is a crash if i use data.table::merge
          rm(DT2)
        }

      }
    }

    if(!is.null(DT)){
      DT[,Source:="CDEC"]
      DT[Flag=="",Flag:=NA]
      DT <- DT[,.(DateTime,Station,Parameter,value,Flag,Source)]
      }

    if(report|ErrorCheck){View(dtGet)}
    #DT[]
    return(DT[])
  }#ends CDEC section

  #USGS -------------------------------------------------------------------------------------------------
  if(source=="USGS"){

    if(is.null(dtGet)){
      if(is.null(range)){#default range is current water year
        range=c(Sys.Date()-365,Sys.Date())
      }else {
        if(length(range)==1){range[2]=Sys.Date()}
      }


      if(parameters[1]=="All"){
        print("Warning: parameters=All does not work with USGS, downloading Flow only")
        parameters="Flow"}
      if(type=="Daily"&parameters[1]=="Flow"){print("Warning 2: Using type='Daily' with USGS Flow will probably not work. Use 'TidalFlow' instead")}
      #Sets all files to download (every unique station, parameter, year combo)
        dtGet=data.table(
          ParamR = rep(parameters,times=length(stations)),
          StationCode = rep(stations,each=length(parameters)))
        dtGet[,Start:=range[1]]
        dtGet[,End:=range[2]]

        #sets ID_USGS
        data.table::setkey(tbl$Stations,StationCode)
        data.table::setkey(dtGet,StationCode)
        dtGet[,ID_USGS:=tbl$Stations[dtGet]$ID_USGS]
        dtGet[ID_USGS=="",ID_USGS:=NA]  #note: this is because WQES_tbls() spits out blanks instead of NULL/NA
        dtGet[is.na(ID_USGS) & stringr::str_detect(StationCode,"^[0-9]+$"), ID_USGS:=StationCode]
       #dtGet[is.na(ID_USGS),ID_USGS:="No Station Code found"]
    }else {
      #dtGet specified by user

  }
    print(paste0("Downloading files ( ",length(dtGet$ParamR), " )" ) )
    for(i in 1:length(dtGet$ParamR)){ #download all Variables station combos for (inside the loop for each year)
      cat(paste0(i," "))
      dtGet[i,File:= USGS_downlo_v2(USGS_Stn= dtGet[i]$ID_USGS, Param= dtGet[i]$ParamR, range= range, type= type, replace= replace, ErrorCheck= ErrorCheck)]
    }#ends Variable Station combo loop

    print("")


    #combine all files into DT
    print(paste0("Compiling files ( ",length(dtGet$ParamR), " )" ) )
    DT=NULL
    for(i in 1:length(dtGet$ParamR)){
      cat(paste0(i," "))
      if(!stringr::str_detect(dtGet[i]$File,"Error")){

        #eventually this will turn to a USGS_read function or included in Dread
        suppressWarnings(fread(dtGet[i]$File))->DT2
        if(type=="15Minute"){
          DT2[!c(1,2),c(3,5)]->DT2
        }else if(type=="Daily") { DT2[!c(1,2),c(3,4)]->DT2}
        names(DT2)<-c("DateTime","value")

        DT2[,Station:=dtGet[i]$StationCode]
        DT2[,Parameter:=dtGet[i]$ParamR]
        data.table::setkey(DT2)
        if(is.null(DT)){
          DT<-DT2
          rm(DT2)
        }else {
          DT <- merge(DT,DT2,all=TRUE) #i thought merge was part of data.table, but appernetly not?
          rm(DT2)
        }
      }else {print(paste("No file: ",dtGet[i]$Station," ",dtGet[i]$ParamR, " ", dtGet[i]$Year ))}
    }
    print("")

    if(report|ErrorCheck){View(dtGet)}
    if(!is.null(DT)){
      DT[,Source:="USGS"]
      #DT[,DateTimeCheck:=MkDate(DateTime)]
      DT <- DT[,.(DateTime,Station,Parameter,value,Source)]

      #some reason value is being returned as a character
      DT[,value:=as.numeric(value)]
      DT[,DateTime:=Dtoolbag::MkDate(DateTime)]
    }
    return(DT[])





  }#ends USGS section
}#check




#Downlo functions
CDEC_downlo_v2= function(StationCode, Param,range=NULL, PDT=FALSE, ErrorCheck=FALSE){
  #_v2 changed to reflect wdl_downlo
  #added changes to reflect CDEC changing file download format
  #updated 20181105 more changes to file formate
  #old      pattern = "<tr><td nowrap>([^<]*)</td><td align=right nowrap>[ \t]*([0-9.]*)"
  #old seperate out date and time seperatily  pattern = "<tr><td nowrap>([0-9]{2}/[0-9]{2}/[0-9]{4}) ([0-9]{2}:[0-9]{2})</td><td align=right nowrap>[ \t]*([-0-9.]*)"
  #keep Date and time togethor

  #set tbl_ referances
  WQES_tbls()->tbl

  if(ErrorCheck){print(paste0("CDEC_downlo range: ",range[1],"  ",range[2],"  Class: ",class(range[1])))}

  if(is.null(range)){
    range=c(MkDate(Sys.time())-5*(60*60*24),MkDate(Sys.time())) #defaults to past 5 days
  }

  #Note:some turbidity is27 and some is 221

  range=MkDate(range,PDT=TRUE)#might not be necissary, but (since CDEC is in PST/PDT) avoids ever having tz wrong

  sensor_num=tbl$Parameters[ParamR==Param]$CDECNum #note:Temp (Farenhiet) will need to be converted to Celsius
  if(length(sensor_num)==0){warning(paste0("Parameter: ",Param," not found in tbl_Parameters$ParamR"))}
  if(is.na(sensor_num)){print("Error: Param to CDECNum")}

  file.loc=paste0(GetPath2Rstuff(),"/data_files/CDEC")
  #range_=as.character(range)
  #range_[!str_detect(range_,":")]=paste0(range_[!str_detect(range_,":")]," 00:00")
  range_=stringr::str_match(as.character(range),"([0-9]{4})-([0-9]{2})-([0-9]{2})( ([0-9]{2}):([0-9]{2}))?")[,-c(1,5)]

  #correcting for when times are not present
  if(is.na(range_[1,4])){range_[1,4:5]="00"}
  if(is.na(range_[2,4])){
    range_[2,4]="23"
    range_[2,5]="00"
  }

  #old way format   rangeCD=paste0(range_[,2],"/",range_[,3],"/",range_[,1],"+",range_[,4],":",range_[,5]) #note: I believe i could removed this for the new cdec file url
  rangeCD=paste0(range_[,1],"-",range_[,2],"-",range_[,3],"+",range_[,4],":",range_[,5])
  range_f=paste0(range_[,1],range_[,2],range_[,3])

  url =  paste0("http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=",StationCode,
                "&SensorNums=",sensor_num,
                "&dur_code=E&Start=",rangeCD[1],"&end_date=",rangeCD[2])

  #write file names
  #  destfile = paste0(file.loc,"/txt/",StationCode,"_",Param,".txt")
  #  csvfile = paste0(file.loc,"/csv/",StationCode,"_",Param,"_CDEC_",range_f[1],".csv")
  #print(url)
  if(ErrorCheck){print(paste0("CDEC_downlo url: ",url))}
  xdt=tryCatch(data.table::fread(url,showProgress=FALSE),error=function(cond){"Error: Download"})

  #because CDEC is stupid and I need to check other sensor_num possibilities
  if(!nrow(xdt)>0){
    if(Param=="Turb"){sensor_num=221
    url =  paste0("http://cdec.water.ca.gov/dynamicapp/req/CSVDataServlet?Stations=",StationCode,
                  "&SensorNums=",sensor_num,
                  "&dur_code=E&Start=",rangeCD[1],"&end_date=",rangeCD[2])
    xdt=tryCatch(data.table::fread(url,showProgress=FALSE),error=function(cond){"Error: Download"})
    }

  }
  #if statement to check if download contains data
  #if(str_detect(x[[1]][1],"STATION_ID")) {
  if(nrow(xdt)>0){

    xdt[,value:=as.numeric(VALUE)]
    xdt$DateTime=xdt$"DATE TIME"

    xdt[,DateTime:=MkDate(DateTime,PDT=TRUE)]
    if(!PDT){xdt[,DateTime:=MkDate(DateTime,PDT=FALSE)]} #if you wanted to keep Dates in PDT

    xdt[,Flag:=as.character(DATA_FLAG)] #formily   xdt$Flag=xdt$"DATA_FLAG"

    xdt<-xdt[,.(DateTime,value,Station=STATION_ID,Parameter=Param,Flag)]

    #if(Print){ names(Xdt)[1]<-c("DateTime_wPDT")
    #fwrite(Xdt,file=csvfile)
    #names(Xdt)[1]<-c("DateTime")}

  } else{
    #trying other sensor_nums because CDEC is stupid


    print(paste("No Data: ",StationCode, " ",Param))
    xdt=data.table(DateTime=NA,value=NA,Station=NA,Parameter=NA,Flag=NA)
  }
  return(xdt)
}#work in progress. look at function inputs

WDL_downlo_v3=function(HydCode, Param, Year= NULL, replace= TRUE, ErrorCheck= FALSE){
  #-v3 changed url to reflect new wdl wedbsite
  #assumes id is the hydstra id (set by user or calling function, adjusted for WQ or Flow parameters)
  #Param as matched to ParamR in \\pcdistfs1\share\Water Quality Evaluation Section\Tools\R_toolbag\db_tables\tbl_Parameters.txt
  #Year as integer or "POR" for period of record. (Note: period or record is daily averaged values, if one value is NA or 170'd no data for that Date is exported)

  #set tbl_ referances
  WQES_tbls()->tbl

  if(is.null(Year)){Year=data.table::year(Sys.Date())}


  WDL_name=tbl$Parameters[ParamR==Param]$WDL_name
  if(!length(WDL_name)==1){
    print("WDL_downlo_v3: WDL_name not length = 1")
    return("Error:Param to WDL_name")
  }

  if(WDL_name==""){
    print("WDL_downlo_v3: WDL_name blank")
    return("Error:Param to WDL_name")
  }

  #according to Ben Brezing I need to add wdl. before water.ca
  #_v2 url:  url=paste0("http://wdl.water.ca.gov/waterdatalibrary/docs/Hydstra/docs/",HydCode,"/",Year,"/",WDL_name,"_DATA.CSV")
  url=paste0("https://wdlstorageaccount.blob.core.windows.net/continuousdata/docs/",HydCode,"/",Year,"/",WDL_name,"_DATA.CSV")

  if(ErrorCheck){print(paste0("WDL url: ",url))}
  destfile=paste0(GetPath2Rstuff(),"/data_files/WDL_raw")
  if(!file.exists(destfile)){dir.create(file.path(destfile),recursive=TRUE)}
  destfile=paste0(destfile,"/",HydCode,"_",Param,"_",Year,"_WDL.CSV")

  #dir.create(paste0(Path2Rstuff,"/data_files/WDL_raw/"),showWarnings = FALSE)
  if(!file.exists(destfile)|replace){
    FileSuccess=tryCatch(suppressWarnings(download.file(url,destfile=destfile,mode='wb',quiet=TRUE)),error=function(cond){"Error"})
  }else {FileSuccess = "Already downloaded"}
  if(FileSuccess=="Error"){
    return("Error: trycatch download")
  }else {return(destfile)}



}

USGS_downlo_v2=function(USGS_Stn, Param, range, type= "15Minute", replace= TRUE, ErrorCheck= FALSE){
  #range as Date
  #ParamNum="00060"
  #example URL is a daily average flow

  #set tbl_ referances
  WQES_tbls()->tbl

  RefMod=TRUE  #adds "referred_module=sw" don't know what this does
  #https://waterdata.usgs.gov/ca/nwis/dv?cb_00060=on&format=rdb&site_no=10338500&referred_module=sw&period=&begin_date=1998-10-01&end_date=2018-05-01

  #15minute https://waterdata.usgs.gov/nwis/uv?cb_00060=on&format=rdb&site_no=11336930&period=&begin_date=2019-11-25&end_date=2019-12-02
  #15Minute https://waterdata.usgs.gov/nwis/uv?cb_00060=on&format=rdb&site_no=11336930&period=&begin_date=2019-11-25&end_date=2019-12-02
  #Daily    https://waterdata.usgs.gov/nwis/dv?cb_72137=on&format=rdb&site_no=11336930&referred_module=sw&period=&begin_date=2018-12-01&end_date=2019-12-01
  #daily    https://waterdata.usgs.gov/nwis/dv?cb_72137=on&format=rdb&site_no=11336930&referred_module=sw&period=&begin_date=2018-12-01&end_date=2019-12-01
  #Set ParamNum from tbl_Parameters. If Param is only a number, use number

  if(USGS_Stn == "" | is.na(USGS_Stn) | is.null(USGS_Stn)){
    print("No USGS station ID")
    return("Error: No Station ID")
  }

  if(!stringr::str_detect(Param,"^[0-9]+$")){
    ParamNum=tbl$Parameters[ParamR==Param]$USGSNum
  }else {ParamNum=Param}

  if(type=="Daily"){url=paste0("https://nwis.waterdata.usgs.gov/ca/nwis/dv?cb_",ParamNum,"=on&format=rdb&site_no=",USGS_Stn,
                               "&period=&begin_date=",as.character(range[1]),"&end_date=",
                               as.character(range[2]))
  }
  if(type=="15Minute"){
    url=paste0("https://nwis.waterdata.usgs.gov/ca/nwis/uv?cb_",ParamNum,"=on&format=rdb&site_no=",USGS_Stn,
               "&period=&begin_date=",as.character(range[1]),"&end_date=",
               as.character(range[2]))
  }

  if(RefMod){url=paste0(url,"&reffered_module=sw")}#don't know what this does
  if(ErrorCheck){print(url)}
  destfile=paste0(GetPath2Rstuff(),"/data_files/USGS_raw")
  if(!file.exists(destfile)){dir.create(file.path(destfile),recursive=TRUE)}
  destfile=paste0(destfile,"/USGS_id",USGS_Stn,"_",Param,"_",as.character(range[1]),
                  "_",as.character(range[2]),"_",type,".txt")


  #dir.create(paste0(Path2Rstuff,"/data_files/WDL_raw/"),showWarnings = FALSE)
  if(!file.exists(destfile)|replace){    #download.file(url,destfile=destfile,mode='wb')}
    FileSuccess=tryCatch(suppressWarnings(download.file(url,destfile=destfile,mode='wb',quiet=TRUE)),error=function(cond){"Error"})

  }else {FileSuccess = "Already downloaded"}
  if(ErrorCheck){print(paste("FileSuccess: ",FileSuccess))}
  if(FileSuccess=="Error"){
    return("Error: trycatch download")
  }else {
    line=scan(destfile,nline=1,what=character(),sep=",",quiet=TRUE)
    if(line=="# ---------------------------------- WARNING ----------------------------------------"){return(destfile)
    }else {

      return("Error: Not Found")}
  } #should be destfile

}#not tested yet
