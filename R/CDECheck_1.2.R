#' CDEC Check
#'
#' Download and plot all WQES data from CDEC. Saves plot to Water Quality Evaluation Section/Tools/Rstuff/CDECheck/ (or working driectory if not connected to //pcdistfs1/share)
#'
#' @param stations list of Station codes. Default is all WQES CDEC stations. Other options include station Region ("North Delta", "South Delta", "Central Delta", "Rock Slough")
#' @param parameters List of parameters. Default is "All" ("Flow","Temp","SpCond","DO","pH","Turb","Chlor")
#' @param range Default is current date time or specify an end date time (or a start and end date time) Use MkDate for specifying date time.
#' @param duration Specify number of days prior to end date to download data for. (Ignored if range includes two values, start and end dates) Problems may occure with start or end times at midnight 00:00
#' @param graphs TRUE generates a PDF plot containing all station and parameter combos. (can take several seconds to finish)
#' @param dt To skip the data download step, supply a data.table. If stations and parameters are unspecified, will graph all station/parameter combos in dt
#' @param ErrorCheck Use TRUE to print during error checking.
#' @return data.table of downloaded CDEC data. When graphs = TRUE also creates a PDF of plotted data.
#' @details Download and plot data for daily CDEC checks. data table is returned from function and PDF plot is saved to "Water Quality Evaluation Section/Tools/Rstuff/CDECheck/"
#'          StationOrder=c("SOI","SXS","MIR","BLP","FAL","FCT","TSL","MOK","OSJ","BET","HOL","ORQ","RSCC","DRB","WDC","HCHM","OBI","HLT","TRN","VCU","MRX","MHO","MRU","WCI","ORI","GLC","ODM","ORM","TWA","GLE","SGA","TPI","PDC","ORX","OH1","SJD")
#' @import ggplot2
#' @import stringr
#' @export

CDECheck <- function(stations=NULL,parameters="All",range=NULL,duration=4,graphs=TRUE,dt=NULL, RedTide=TRUE,ErrorCheck = FALSE){
  #dont use range as midnight (ie 0000)
  #set tbl_ referances
  WQES_tbls()->tbl

  if(is.null(dt)){
    #range = NULL will be passed to GetData where range will be current time and previous 5 days. Otherwise specify range using MkDate()
    if(is.null(stations)){
      stations <- tbl$Stations[WQESmanaged==1&Telemetry==1&Terminated==0]$StationCode
    }
    if(stations[1] %in% c("Central Delta","South Delta","North Delta","Rock Slough")) {
      stations <- tbl$Stations[WQESmanaged==1&Telemetry==1&Terminated==0&Region==stations]$StationCode

    }

    if(stringr::str_detect(stations[1],"David|david")) {
      if(ErrorCheck){print("Well, Howdy Pardner!)")}
      stations = c("GLC","GLE","ORX","OH1","SJD","ORM","TWA","OLD","PDC","PCO","SGA","SUR","TPS","TPI")
      }
    stations<-tbl$Stations[StationCode%in%stations,.(Station=StationCode,Region=Region,SubGroup=SubGroup)]
    Regions<-stations[,.N,by=Region]$Region

    if(is.null(duration)){duration=4}
    if(is.null(range)){range=c(MkDate(Sys.time())-duration*(60*60*24),MkDate(Sys.time()))}
    if(length(range)==1){range=c(range,MkDate(range)-duration*(60*60*24))}
    range=sort(range)

    if(parameters[1]=="All"){
      parameters<-tbl$Parameters[CDECNum>0&!ParamR=="",]$ParamR
    }

    dt<-GetData(stations$Station,parameters,range,"CDEC",report=ErrorCheck)
    print("Done Downloading")



    data.table::setkey(dt,Station)

    data.table::setkey(stations,Station)

    dt<-dt[stations] #might be times when this doesn't work?
    returnDT=TRUE
  }else {
    returnDT=FALSE
    parameters<-dt[,.N,by=Parameter]$Parameter
    stations<-dt[,.N,by=.(Region,SubGroup,Station)]
    Regions<-stations[,.N,by=Region]$Region
    range<-c(min(dt$DateTime),max(dt$DateTime))

  }

  StationOrder=c("SOI","SXS","MIR","BLP","FAL","FCT","TSL","MOK","OSJ","BET","HOL","ORQ","RSCC","DRB","WDC","HCHM","OBI","HLT","TRN","VCU","MRX","MHO",
                 "MRU","WCI","ORI","GLC","ODM","ORM","TWA","GLE","SGA","TPI","PDC","ORX","OH1","SJD")
  dt[,Station:=factor(Station,levels=StationOrder)]
  ParamOrder=c("Flow","Temp","DOconc","SpCond","pH","Turb","Chlor")
  dt[,Parameter:=factor(Parameter,levels=ParamOrder)]


  if(graphs){


    #insert NA's for missing parameters & stations
    dtleft<-dt[,.N,by=.(Station,Parameter)]
    dtright<-data.table(Station=rep(stations$Station,each=length(parameters)),Parameter=rep(parameters,times=length(stations$Station)))
    data.table::setkey(dtleft,Station,Parameter)
    data.table::setkey(dtright,Station,Parameter)
    dtmis<-dtright[!dtleft]

    dtmis[,DateTime:=range[1]]
    data.table::setkey(dtmis,DateTime,Station,Parameter)
    data.table::setkey(dt,DateTime,Station,Parameter)
    DT<-merge(dt,dtmis,all=TRUE)

    dtmis[,DateTime:=range[2]]  #trying to avoid the warning     geom_path: Each group consists of only one observation.
    DT<-merge(DT,dtmis,all=TRUE)
    DT[(Parameter=="Temp"&value>212),value:=NA]

    DT[,Station:=factor(Station,levels=StationOrder)]
    DT[,Parameter:=factor(Parameter,levels=ParamOrder)]


    if(TRUE){
      #plot idea 3, facet on big pdf
      path=paste0('//pcdistfs1/share/Water Quality Evaluation Section/Tools/Rstuff/CDECheck')
      if(!file.exists(path)){
        path=getwd()
        warning(paste0("Cannot find path: //pcdistfs1/share/Water Quality Evaluation Section/Tools/Rstuff/CDECheck     pdf saved to: ", path)) #will this work if not connected to share drive?
        print(paste0("Cannot find path: //pcdistfs1/share/Water Quality Evaluation Section/Tools/Rstuff/CDECheck     pdf saved to: ", path))
      }

      #When a station is down can cause NA's to appear in list of parameters
      DT <- DT[!is.na(Parameter)]

      nCol=length(DT[,.N,by=Parameter]$Parameter)

      if(ErrorCheck){print(DT[,.N,by=Parameter]$Parameter)}
      nRow=length(DT[,.N,by=Station]$Station)

      file=paste0(path,"/CDECheck_", MkDate(range[2],asDate=TRUE),"_", stringr::str_replace(str_match(as.character(range[2]),"^[0-9\\-]{10} ([0-9:]{5})")[2],":",""))

      #options(warn=-1)
      error= tryCatch( pdf(file=paste0(file,'.pdf'),w=(nCol*3),h=nRow*2),error=function(cond){TRUE})
      if(is.null(error)){
        print(paste0("plotting to: ",file,'.pdf'))
        p=ggplot2::ggplot(DT[],aes(x=DateTime,y=value,group=1))+geom_line(aes(group=1))+labs(title=paste0("CDEC Check: ",range[2]))+
          facet_wrap(Station~Parameter,nrow=nRow,ncol=nCol,scales="free_y")

        if(RedTide){
          dtrf<-DT[Parameter=="Flow"&value<0,.(DateTime,Station ,val_Up=-0.666666)]

          setkey(dtrf)
          setkey(DT,DateTime,Station)
          dtrf<-merge(DT,dtrf,all=TRUE)
          dtrf[val_Up==-0.666666,val_Up:=value]

          p<-p+geom_line(data=dtrf,aes(x=DateTime,y=val_Up),color="red",size=.25)
        }#add red line segments for upstream Flow

        print(p)
        dev.off()

        print("Done plotting")

      }else {
        print(paste0("File is open, cannot print: ",file,'.pdf'))
      }#if i wasn't able to open the pdf, stop plotting

      #options(warn=0)

    }

  }#ends if(graphs)


  if(returnDT){

    return(dt[])
  }else{  return(NULL)}

}
