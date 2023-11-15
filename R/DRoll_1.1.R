#' Rolling window function
#'
#' Apply a rolling function by groups for data.table
#' @param dt Input data.table
#' @param days number of days to apply rolling window (default is 30)
#' @param funs functions to apply rolling window to. default: c("mean","max","min","sum","sd")
#' @param ErrorCheck Use TRUE to print during error checking.
#' @param align for froll (default 'center')
#' @param merge use TRUE to return orignal table with added columns for rolling funcs
#' @return data.table of Common stats for rolling funs by input groups
#' @details returns a rolling window with funs applied data.table for a rolling window of number of days. First calculates funs for Daily values and then aggrgates by rolling window
#' @import ggplot2
#' @import stringr
#' @export

DRoll <- function(dt,days=30,funs=c("mean","max","min","sum","sd"),align="center",merge=FALSE){

  #trying to avoid affecting input table
  dt<-copy(dt)




    #add date if missing
    if(!any(stringr::str_detect(names(dt),'^Date$'))){
      if(!any(stringr::str_detect(names(dt),"DateTime"))){
        stop("Error: No Date or DateTime column")
      }else {
        dt[,Date:=MkDate(DateTime,asDate=TRUE)]
      }
    }


    dtd<-dt[!is.na(value),.(Mean=mean(value,na.rm=TRUE),
                            Max=max(value,na.rm=TRUE),
                            Min=min(value,na.rm=TRUE),
                            SD=sd(value,na.rm=TRUE),
                            Sum=sum(value,na.rm=TRUE),
                            .N),by=.(Station,Parameter,Date)]


    #with new fast add_NAValues not necessary to avoid it
      #calculate time difference
      #dtd[,ddif:= Date - shift(Date), by= .( Station, Parameter)]
      #if any values are greater than 1 day we need to add daily NAValues
      #if(any(dtd$ddif)>1){    }

    #verify complete series
    dtd <- add_NAValues(dtd,'Date')

    #compute 30 day rolling average table
    dtr <- dtd[]

    if(any(stringr::str_detect(funs,"^mean$"))){
      dtr[,Mean:=frollmean(Mean,days,align=align),by=.(Station,Parameter)]
      names(dtr)<-stringr::str_replace_all(names(dtr),"^Mean$",paste0("Mean",days))
    }

    if(any(stringr::str_detect(funs,"^max$"))){
      dtr[,Max:= frollapply(Max,days,"max",align=align),by=.(Station,Parameter)]
      names(dtr)<-stringr::str_replace_all(names(dtr),"^Max$",paste0("Max",days))
    }

    if(any(stringr::str_detect(funs,"^min$"))) {
      dtr[,Min:= frollapply(Min,days,"min",align=align),by=.(Station,Parameter)]
      names(dtr)<-stringr::str_replace_all(names(dtr),"^Min$",paste0("Min",days))
    }

    if(any(stringr::str_detect(funs,"^sum$"))) {
      dtr[,Sum:= frollsum(Sum,days,align=align),by=.(Station,Parameter)]
      names(dtr)<-stringr::str_replace_all(names(dtr),"^Sum$",paste0("Sum",days))
    }

    if(any(stringr::str_detect(funs,"^sd$")))  {
      dtr[,SD := frollapply( SD,days ,"sd",align=align),by=.(Station,Parameter)]
      names(dtr)<-stringr::str_replace_all(names(dtr),"^SD$",paste0("SD",days))
      }


    if(any(stringr::str_detect(names(dtr),"^N$"))) {
      dtr[,Np:= (frollsum(N,days,align=align,algo="exact"))/(days*24*4),by=.(Station,Parameter)]
      names(dtr)<-stringr::str_replace_all(names(dtr),"^Np$",paste0("Np",days))
    }


  if(merge){
    setkey(dt,Date,Station,Parameter)
    setkey(dtr,Date,Station,Parameter)
    return(dt[dtr][])
  } else {
    return(dtr[])
  }



}
