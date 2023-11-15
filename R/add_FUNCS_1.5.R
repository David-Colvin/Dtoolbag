# 1.3 added add_NAValues 'Date' option and changed code to use setDT and CJ functions
# 1.4 adding NALimit to GodinFilter
# 1.5 changing the GodinFilter

#' Add column Day of Water Year
#'
#' Add a column to data.table for day number of water year (1-365) based on Date column
#' @param dt data.table
#' @param time Include additional column for wy Day and time fraction
#' @return dt with new column (or two new columns)
#' @details If input data.table has column Date than creates a new column for the Water year day number (ie 10/01(WY-1) is 1, 9/30(WY) is 365/366). Uses yday()
#' @import stringr
#' @export

add_WYDay=function(dt,time=FALSE){
  dt <- copy( dt )

  if(!any(stringr::str_detect(names(dt),"^Date$"))) {dt[,Date := MkDate(DateTime,asDate=TRUE)]}
  DateN <- "Date"


  dt[ month( get( DateN)) >= 10 & as.integer( data.table::year( get( DateN))) %% 4 == 0,
     WYDay := as.integer( yday( get( DateN))) - 274]

  dt[ month( get( DateN)) >= 10 & as.integer( data.table::year( get( DateN))) %% 4 > 0,
     WYDay:=as.integer( data.table::yday( get( DateN))) - 273]
  dt[ month( get( DateN)) < 10,
     WYDay := as.integer( data.table::yday( get( DateN))) + 92]
  if(time){
    dt[,WYDayTime := WYDay + ( as.numeric( strftime( DateTime, format = "%H"))+( as.numeric( strftime( DateTime, format = "%M")) / 60)) / 24]
  }
  return(dt[])

}#adds a column that is the number of days since the start of the water year (ie, 10/01 -> 1)

#' Add column for Water Year
#'
#' adds column to data.table for Water Year based on Date column
#' @param dt data.table
#' @return dt with new column WY
#' @details If input data.table has column Date than creates a new column for the Water year (ie 10/01 to 9/30)
#' @import stringr
#' @export

add_WY=function(dt){
  dt <-copy(dt)
  if(any(stringr::str_detect(names(dt),"^WY$"))){dt[,WY:=NULL]}
  if(any(stringr::str_detect(names(dt),"^Date$"))){
    # print("fuckyou")
    # print(dt[month(Date)>=10,WY:=as.integer(year(Date)+1)])
    dt[month(Date)>=10,WY:=as.integer(data.table::year(Date)+1)]
    dt[month(Date)<10,WY:=as.integer(data.table::year(Date))]
  }

  if(any(stringr::str_detect(names(dt),"^DateTime$"))){
    dt[month(DateTime)>=10,WY:=as.integer(data.table::year(DateTime)+1)]
    dt[month(DateTime)<10,WY:=as.integer(data.table::year(DateTime))]
  }
  if(!any(stringr::str_detect(names(dt),"^Date$|^DateTime$"))){print("WY not added, no recognized Date Column")}
  return(dt[])
} #adds column for water year based on date


#' Add Month groups
#'
#' adds column to data.table for Month group based on Date column
#' @param dt data.table
#' @param monthgroups_of (2 or 3, default is 3) if 3 groups are ("Oct - Dec","Jan - Mar","Apr - Jun","Jul - Sep"). if 2, groups are ("Oct-Nov","Dec-Jan","Feb-Mar","Apr-May","Jun-Jul","Aug-Sep").
#' @return dt with new column Month
#' @details If input data.table has column Date than creates a new column for the Month group as a factor.
#' @export

add_MonthGroup=function(dt,monthgroups_of=3){
  dt<-copy(dt)
  if(monthgroups_of==2){
    #make groups of bimonthly
    dt[month(Date)>=10&month(Date)<=11,Month:="Oct-Nov"]
    dt[month(Date)==12 | month(Date)==1,Month:="Dec-Jan"]
    dt[month(Date)>=2&month(Date)<=3,Month:="Feb-Mar"]
    dt[month(Date)>=4&month(Date)<=5,Month:="Apr-May"]
    dt[month(Date)>=6&month(Date)<=7,Month:="Jun-Jul"]
    dt[month(Date)>=8&month(Date)<=9,Month:="Aug-Sep"]
    #to order by months
    dt[,Month:=factor(Month,levels=c("Oct-Nov","Dec-Jan","Feb-Mar","Apr-May","Jun-Jul","Aug-Sep"),ordered = TRUE)]
  }

  if(monthgroups_of==3){
    #making groups tri-monthly
    dt[month(Date)>=10&month(Date)<=12,Month:="Oct - Dec"]
    dt[month(Date)>=1 & month(Date)<=3,Month:="Jan - Mar"]
    dt[month(Date)>=4&month(Date)<=6,Month:="Apr - Jun"]
    dt[month(Date)>=7&month(Date)<=9,Month:="Jul - Sep"]
    #to order by months
    dt[,Month:=factor(Month,levels=c("Oct - Dec","Jan - Mar","Apr - Jun","Jul - Sep"),ordered = TRUE)]
  }
  return(dt[])
}#adds a grouped Month variable (default group is 3 months)





#' Add tidal filtered value
#'
#' adds column to data.table for tidally filtered values column based on the GodinFilter
#' @param dt data.table
#' @param NAlimit integer numbr of allowable NA's in averageing window
#' @param version Huston or Swift (oringally was Swift version, now is Huston version)
#' @return dt with new column tfvalue
#' @details Smoothing function applied for each Station and Parameter in data.table. Assumes input data has no missing 15 minute intervals. Adds column tfvalue as the average of (center weighted 24 hour rolling average + previous 25 hour average + future 25 hour average).  Problems occure with NA or NULL values, or missing 15 minutes intervals. Use fill="extend" to avoid gaps. Tidal filter will also elliminate short duration pulse signals.
#' @import zoo
#' @export

GodinFilter=function(dt,addNA=TRUE,version="Huston",partial=FALSE,fill=NA,na.rm=FALSE){
  #Mehtod from Ted swift Excel version
  #assumes melted file (not too big?) with Columns DateTime, Station, Parameter, value


  #print("probably needs some testing....")
  dt <- copy(dt) #copy because data.table makes unexpected changes to the original since it works by referance

  if(addNA){
    dt <- add_NAValues(dt)
  } #because rollying window will apply by number of rows, missing time stamps will screw things up

  if(version == "Swift"){
    dt[,C24:= zoo::rollapply(value,width=96,FUN=mean,align="center",fill=fill,partial=partial,na.rm=na.rm),by=.(Station,Parameter)]
    dt[,L25:= zoo::rollapply(value,width=100,FUN=mean,align="left",fill=fill,partial=partial,na.rm=na.rm),by=.(Station,Parameter)]
    dt[,R25:= zoo::rollapply(value,width=100,FUN=mean,align="right",fill=fill,partial=partial,na.rm=na.rm),by=.(Station,Parameter)]
    dt[,tfvalue:=(C24+L25+R25)/3,,by=.(Station,Parameter)]

    dt[,C24:=NULL]
    dt[,L25:=NULL]
    dt[,R25:=NULL]

  } #version from Ted Swift Excel, Center weightered 24 hour, and a forward and backward 25 hour, all averaged togethor


  if(version == "Huston"){
    dt[,C1_24:= frollmean(shift(value,1,type="lag"),n=96,align="center"),by=.(Station,Parameter)]
    dt[,C2_24:= frollmean(C1_24,n=96,align="center"),by=.(Station,Parameter)]
    dt[,tfvalue:= frollmean(shift(C2_24,1,type="lag"),n=100,align="center"),by=.(Station,Parameter)]

    dt[,C1_24:=NULL]
    dt[,C2_24:=NULL]

  }#Dave Huston version, a backward 24 hr average, which is used as the next center weighted 24 hr average, which feeds into a backwards weighted 25 hr rolling average.

  #net difference between the Swift version and Huston version is minimal. Huston version "uses" more values in each individual filtered value (ie, like a pyramid scheme) which greatly increases the propigation of NA values. Huston version is also more backwards centered, so Peaks in the tidal filtered value appear to lag the 15minute data.

  return(dt[])
}


#' Add NA's to data serise
#'
#' adds NA's for values that are missing from data serise
#' @param dt data.table
#' @param by time increment to compare to complete series'DateTime' or 'Date'. DateTime assumes 15 minute inteval
#' @return dt with NA's for missing data points
#' @details assumes even intervals of 15 minutes. No duplicates
#' @export
add_NAValues <- function( dt,by = 'DateTime'){
  # create a new data.table that has every timestamp (either 15min or date) for station/parameter combos with an NA value, and
  #  use data.table merge fucntions to join on date.time

  #kept for old script purposes. use by= 'DateTime'
  if(by=='oldDateTime'){
    if(!any(stringr::str_detect(names(dt),'DateTime'))){
      stop("Warning: DateTime not in Data Table")
    }

  #create full date time series
    range <- c(min(dt$DateTime),max(dt$DateTime))
    dtNA <- data.table(DateTime = MkDate(range,time15min=TRUE,sequence=TRUE),Station = "dtNA", Parameter = "dtNA",value=NA)

  # set keys and merge data tables
    setkey(dtNA)
    setkey(dt)

    dtNA <- merge( dt,dtNA,all=TRUE)
    dtNA <- Dcast(dtNA)
    dtNA <- Dmelt( dtNA)
    dtNA <- dtNA[!Station =="dtNA"]
    dtNA[,variable:=NULL]

    setkey(dtNA)
    dtNA <-merge(dt,dtNA,all=TRUE)
  }

  if(by=='DateTime'){
    if(!any(stringr::str_detect(names(dt),'DateTime'))){
      stop("Warning: DateTime not in Data Table")
    }

    #create full date time series
    range <- c(min(dt$DateTime),max(dt$DateTime))
    dtNA <- setDT(dt, key= c('DateTime', 'Station','Parameter'))[CJ(DateTime=MkDate(range,time15min=TRUE,sequence=TRUE),
                                                                Station=unique(Station),
                                                                Parameter=unique(Parameter))][]
  }

  if(by=='Date'){
    if(!any(stringr::str_detect(names(dt),'Date'))){
      # add Date from DateTime
      if(!any(stringr::str_detect(names(dt),'DateTime'))){
        stop("Warning: Date and DateTime not in Data Table")
      }else { dt[,Date:=MkDate(DateTime, asDate:=TRUE)] }
    }
    #available Date range
    range <- c(min(dt$Date),max(dt$Date))

    #Cross join? note: if value is not in original datatable than all values will be NA, but not a problem because all other columns are NA
    dtNA <- setDT(dt, key= c('Date', 'Station','Parameter'))[CJ(Date=seq(range[1],range[2],"1 days"),
                                                     Station=unique(Station),
                                                     Parameter=unique(Parameter))][]
  }

  return(dtNA[])
}
