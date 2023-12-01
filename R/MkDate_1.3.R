#1.3 Github'ing
#1.2 updating to avoid the UTC trap. if time is already POSIXct UTC, add 8 hours
# 20220310 Changed Quiet to ErrorCheck
# 20231201 more lines for ErrorCheck

#' Convert common timstamps to POSIXct
#'
#' convert common date and time formats to POSIXct, timezone "ETC/GMT+8"   (aka pst)
#' @param DateTime vector of dates, need to be an a format already in DateFormat
#'                 If typing numbers, must be characters (ie use "202010301015" for 10/30/2020 10:15)
#' @param asDate Converts to Date only (no time)
#' @param time15min Make times even intervals of 15 minutes  (:00 :15 :30 :45)
#' @param sequence Makes a sequence using BY
#' @param BY Time interval to be used by sequence
#' @param PDT Use TimeZone "America/Los_Angeles" (aka daylight savings)
#' @param OFFset I don't know
#' @param DateForm Specify the date formate (if blank DateForm is set by function DateFormat)
#' @return Converted POSIXct timestamps
#' @details If crashes try using fread(). (sometimes fread will assume values are in UTC, and cause a shift when using MkDate...)
#' @import stringr
#' @export

MkDate=function(DateTime,asDate=FALSE,time15min=FALSE,sequence=FALSE,BY="15 min",OFFset="",PDT=FALSE,DateForm="",ErrorCheck=FALSE, UTCcheck=TRUE){
  #consider making a new one for converting integer seconds to date
  #DateTime: vector of dates, need to be an a format already in DateFormat
  #         must be in character format
  #sequence: to return a sequence of dates/times
  #time15min: to make times an even 15 minutes
  #note: if dates do not have time DO NOT use time15min
  #always use tz="ETC/GMT+8" when using base function as.Date, because MkDate converts to POSIXct, which will convert to tz=UTC by default in as.Date
  #time zones are very confusing. To the best of my knowledge, Etc/GMT+8 will produce an -08 offset (seems backwards I know).

  #DC20191210 added DateTime1

  #convert numeric to character
  if(class(DateTime)[1] == "numeric") {
    DateTime = as.character( DateTime )
  }

  DateTime1=DateTime[!is.na(DateTime)] #to make sure I check stuff with the first non NA DateTime
  DateTime1=DateTime1[!stringr::str_detect(DateTime1,"^(?![\\s\\S])")][1] #then make sure the first value isn't ""

  if(ErrorCheck){print(paste0(c("MkDate - DateTime1: ",DateTime1)))}

  #convert numeric to character
  if(class(DateTime)[1] == "numeric") {
    DateTime = as.character( DateTime )
  }

  if(PDT){tzone="America/Los_Angeles"
  }else {tzone="ETC/GMT+8"}
  #from: https://stat.ethz.ch/R-manual/R-devel/library/base/html/timezones.html
  #Most platforms support time zones of the form Etc/GMT+n and Etc/GMT-n (possibly also without prefix Etc/), which assume a fixed offset from UTC (hence no DST). Contrary to some expectations (but consistent with names such as PST8PDT), negative offsets are times ahead of (east of) UTC, positive offsets are times behind (west of) UTC.

  #should check the entire vector


  if(class(DateTime)=="Date"){
    if(ErrorCheck){
      print("(1)MkDate: is Date")#problem is that sometimes Dates get converted to UTC, then corrected with timezone, so that they are an incorrect date. I fucking hate dates/times in R
    }
    return(DateTime)
  }

  #should check the entire vector
  if(class(DateTime)[1]=="POSIXct"){
    if(ErrorCheck){print("MkDate: is POSIXct")}
    if(!is.null(attr(DateTime,"tzone"))){
      if(attr(DateTime,"tzone")=="UTC"){
        print("UTC time, 8 hours added to TimeStamp")
        DateTime <- DateTime + 28800 #add 8 hours because when time is convert to ETC/GMT+8 8 hours will be lost when shown
    }
    }

  }


  if(DateForm==""){
    #print(DateTime[1])
    #browser()
    DateForm=DateFormat(DateTime1)#checking first none NA DateTime
    if(ErrorCheck){print(paste("DateForm: ",DateForm))}
  }
  if(DateForm=="%b %Y"){
    MonthDate = as.yearmon(DateTime,DateForm)
    return(MonthDate)
  }
  if(!class(DateTime)[1]=="POSIXct"&!stringr::str_detect(DateForm,"H|I")){ #edited _V3
    #if(!str_detect(DateForm,"H")){
    time15min=FALSE
    if(ErrorCheck){print("(2)MkDate says: Date")}
    asDate=TRUE}#implies DateTime does not have time, and there fore is a Date


  DateTime=as.POSIXct(as.POSIXlt(DateTime,format=DateForm,tz=tzone))
  if(time15min){DateTime=as.POSIXct(as.POSIXlt(round(as.double(DateTime)/(15*60))*(15*60),origin=(as.POSIXlt('1970-01-01')),tz=tzone))}

  if(sequence){DateTime=seq(DateTime[1],DateTime[2],by=BY)}
  if(asDate) {DateTime=as.Date(DateTime,tz=tzone)}

  return(DateTime)
} #makes input into POSIX dates


##' Determine DateFormat
##'
##' Sets expected Date/Time format from list of pre set common types
##' @param DateForm Singe Date Time value as character
##' @return Returns a Date Format to be used by POSIXct format parameter.
##' @details Somethng for later


DateFormat=function(DateForm){
  #print(DateForm)
  YMDHM =  "^[0-9]{12}"                                                    #201311010000  for speedy-ness #DC20190404 added $
  YMDHMS =  "^[0-9]{14}"                                                   #20131101000001  for speedy-ness #DC20210913 added option for seconds
  YMD_HMS1="^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$"       #2015-10-01 00:15:00   from fwrite using "write.csv" for date formate #DC20190404 added $
  YMD_HM  ="^[0-9]{4}-[0-9]{2}-[0-9]{2} [0-9]{2}:[0-9]{2}$"                #2015-10-01 00:15 #DC20190404 added $
  YMD_HMS3 = "^[0-9]{8}_[0-9]{6}$"                                          #20210927_110701
  YMD_HM2  = "^[0-9]{8} [0-9]{4}$"                                          #20151001 0015 #DC20190404 added $

  MDY_HM1= "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{2}:[0-9]{2}$" #08/15/2016 09:30:04   from WDL #DC20190404 added $   #DC20190723 add {1,2} to month and day and hour
  MDY_HM2= "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4} [0-9]{1,2}:[0-9]{2}$"          #8/5/2016 9:30      from excel #DC20190404 added $
  MDY    =  "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}$"                             #7/1/2017    no time
  MDY2   =  "^[0-9]{1,2}-[0-9]{1,2}-[0-9]{4}$"                             #09-01-2019  no time (From TROA.net)
  YMD2 =    "^[0-9]{8}$"                                                   #20181126  no time

  YMD    =  "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"                                 #2017-09-01  notime
  YMDtz  =  "^[0-9]{4}-[0-9]{2}-[0-9]{2} -08$"                             #2017-09-01 -08  no time w/ tz? did i add it?  #DC20190404 added $
  MDYAMPM = "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4} [0-9]{1,2}:[0-9]{2}:[0-9]{2} [AP]{1}M$"  #7/1/(20)17 01:12 PM    stupid excel/Hobo ware  #DC20190404 added $
  Mon_YYYY = "^Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sep|Oct|Nov|Dec [0-9]{4}$"  #Oct 1999
  #browser()
  if(stringr::str_detect(DateForm,YMD_HMS1)){     DForm="%Y-%m-%d %H:%M:%S"
  }else if(stringr::str_detect(DateForm,YMD_HM)){ DForm="%Y-%m-%d %H:%M"
  }else if(stringr::str_detect(DateForm,YMD_HMS3)){ DForm="%Y%m%d_%H%M%S"
  }else if(stringr::str_detect(DateForm,YMD_HM2)){ DForm="%Y%m%d %H%M"

  }else if(stringr::str_detect(DateForm,MDY_HM1)){DForm="%m/%d/%Y %H:%M:%S"
  }else if(stringr::str_detect(DateForm,MDY_HM2)){DForm="%m/%d/%Y %H:%M"

  }else if(stringr::str_detect(DateForm,MDY)){    DForm="%m/%d/%Y"
  }else if(stringr::str_detect(DateForm,MDY2)){    DForm="%m-%d-%Y"
  }else if(stringr::str_detect(DateForm,YMD)){    DForm="%Y-%m-%d"
  }else if(stringr::str_detect(DateForm,YMD2)){    DForm="%Y%m%d"
  }else if(stringr::str_detect(DateForm,YMDHMS)){  DForm="%Y%m%d%H%M%S"
  }else if(stringr::str_detect(DateForm,YMDHM)){  DForm="%Y%m%d%H%M"
  }else if(stringr::str_detect(DateForm,MDYAMPM)){DForm="%m/%d/%Y %I:%M:%S %p"
  }else if(stringr::str_detect(DateForm,Mon_YYYY)){DForm="%b %Y"
  }else {
    warning(paste(DateForm, " format not found; correct function DateFormat in DataManipulation"))

    #other example DateForms
    #YearMonth   "[0-9]{6}"    "%Y%m"

  }

  return(DForm)
}#to be used to find date format to convert input date to POSIX





