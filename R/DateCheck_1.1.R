#' Looking for missing times and duplicate time stamps
#'
#' old function, needs to be revisited
#' @param Xdt data.table with column "DateTime"
#' @param Fix should sequence be replaced and NULLS inserted?
#' @param PDT for input of daylight savings
#' @param Diagnos I know know
#' @return original data.table (if FIX, missing times are inserted with NULL for values)
#' @details function needs work
#' @import stringr
#' @export

DateCheck=function(Xdt,Fix=FALSE,PDT=FALSE,Diagnos=FALSE){#need to consider what happens at data.table input
  #Xdt: data.table with column "DateTime"
  #Fix: should sequence be replaced and NULLS inserted?

  #if(PDT){if(names(Xdt)[1]=="DateTime_wPDT"){
  #  names(Xdt)[1]="DateTime"}
  #}

  if(names(Xdt)[1]=="DateTime"){
    x=Xdt$DateTime[1]
    Start<-MkDate(x,PDT=PDT)
    #Start<-as.POSIXlt(round(as.double(x)/(15*60))*(15*60),origin=(as.POSIXlt('1970-01-01')),tz="GMT")
    x=Xdt$DateTime[length(Xdt$DateTime)]
    End<-MkDate(x,PDT=PDT)
    #End<-as.POSIXlt(round(as.double(x)/(15*60))*(15*60),origin=(as.POSIXlt('1970-01-01')),tz="GMT")

    Xdt[,step:=difftime(DateTime,shift(DateTime,fill=NA),units="mins")]
    Xdt[,Row:=1:length(Xdt$DateTime)]

    #duplicates
    test=length(Xdt[step==0,DateTime])
    if(test>0){print(paste(test,"dublicate time stamps"))

      if(Fix){
        #remove Duplicates
        Xdt[!Row[Xdt[step==0,Row]]]->Xdt
        #Xdt[!Xdt[step==0,Row]]->Xdt
        #Xdt[!step==0,]->Xdt  problem, deletes rows with step=NA
      }
    }

    y=seq(Start,End,by="15 min")

    test=length(Xdt$DateTime)-length(y)

    if(test<0){ print(paste(length(Xdt$DateTime[Xdt[step>20,Row]-1]),"missing gaps"))
      if(Fix){
        Xdt[,step:=difftime(DateTime,shift(DateTime,fill=NA),units="mins")]
        Xdt[,Row:=1:length(Xdt$DateTime)]
        z1=Xdt$DateTime[Xdt[step>20,Row]-1]
        z2=Xdt$DateTime[Xdt[step>20,Row]]
        zdt<-data.table()

        for(i in 1:length(z1)){
          z=seq(z1[i],z2[i],by="15 min")
          z=z[c(-1,-length(z))]
          zdt=rbind(zdt,data.table(DateTime=z),fill=TRUE)
        }

        Xdt=rbind(Xdt,zdt,fill=TRUE)
        Xdt=Xdt[order(DateTime)]
        test=length(Xdt$DateTime)-length(y)
      }
    }

    if(test==0){
      Xdt[,ts:=y]
      Xdt[,diff_ts:=difftime(DateTime,ts,units="secs")]
      if(length(Xdt[diff_ts>0,diff_ts])>0){print("Still another problem, (or times not exact-ed)")
        #Xdt[,Row:=1:length(Xdt$DateTime)]
        #Xdt[diff_ts>0,Row]
      }else {print("Times Verified to sequence")}
      if(!Diagnos){Xdt[,diff_ts:=NULL]}
      if(!Diagnos){Xdt[,ts:=NULL]}
    }

    if(!Diagnos){Xdt[,Row:=NULL]}
    if(!Diagnos){Xdt[,step:=NULL]}



  } else{print("Dates have no time and will not be fixed")
    Fix=FALSE}
  #if(PDT){names(Xdt)[1]="DateTime_wPDT"}
  if(Fix|Diagnos){return(Xdt)}
}#to fix missing date sequences

