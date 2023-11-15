#edited 20201209  WDL column names appear to have changed. removed '_' from between words
#edited 20230717 set Analyt Total Hardness to equal Parameter Hardness

#' Read Discrete lab data
#'
#' Reads Discrete lab data
#' @param file file name and path you want to read.
#' @param simple Default is TRUE, simplifies table
#' @param filter Default is NULL, (or all) list of parameters
#' @return data.table with column names in the form DateTime,(StationCode).(ParameterAbreviation)
#' @details If crashes try using fread()
#' @import stringr
#' @export



DreadDiscrete <- function(file, simple = TRUE, filter = NULL, ErrorCheck=FALSE){
  #needs to be run from within project working directory

  #simple (TRUE) reduce columns of table to just the usefull ones
  #filter 'all' / 'Ions' or list of paramters to filter on (on Parameter)

  Dtoolbag::WQES_tbls() -> tbl
  if(is.null(filter)){filter='all'}
  if(filter[1]=="ions" | filter[1]=="Ions"){ filter <- c("B","Br","Ca","Cl","Mg","NO3","Na","K","TDS","SO4","NH3","T_Alk","SpCond_","Hardness")
  }


  KeepStationName=FALSE #Will be set to TRUE in the case a station name is missing from tbl$Stations, will keep the column Long_Station_Name

  # Files4R("datafiles",file)->files


  data.table::fread(file)->dt

  #re-name columns to avoid spaces    little sloppy
  names(dt)->heads
  stringr::str_replace_all(heads," ","")->heads  #Changing again because WDL is inconsistent (no '_')
  names(dt)<-heads
  #check names because Column names are inconsistent

  #removed:  want to keep column names close to original from WDL    names(dt)[1]<-"Station_Name"
  dt[,StationName:=LongStationName]
  #need to update stations names for a FLIMS station names
  dt[tbl$Stations,on = 'StationName', Station:= i.StationCode ] #what does this do? anything?

  #apply (missing) station codes
  #this should have been accomplished above.....
  if(TRUE){
  dt[StationName=="Grant Line Canal East",Station:="GLE"]
  dt[StationName=="Bethel Island near Piper Slough",Station:="BET"]
  dt[StationName=="Doughty Cut near Grantline Canal",Station:="DGL"]
  dt[StationName=="Grant Line Canal near Old River",Station:="GLC"]
  dt[StationName=="LITTLE TRUCKEE R AB BOCA RES",Station:="LAB"]
  dt[StationName=="Middle River @ Union Point - P10A",Station:="MRU"]
  dt[StationName=="Middle River near Tracy Road",Station:="MRT"]
  dt[StationName=="Mokelumne River near Highway 12",Station:="MOK"]
  dt[StationName=="Old River Downstream DMC Barrier",Station:="ODM"]
  dt[StationName=="Old River Upstream of Mountain House Creek",Station:="ORM"]
  dt[StationName=="Old River at Tracy Wildlife Association",Station:="TWA"]
  dt[StationName=="Old River below Headwaters",Station:="OH1"]
  dt[StationName=="Old River near Bacon Island @ USGS Pile",Station:="OBI"]
  dt[StationName=="Old River near Doughty Cut - ORX",Station:="ORX"]
  dt[StationName=="Old River near Doughty Cut - ORX",Station:="ORX"]
  dt[StationName=="Old River near Frank's Tract",Station:="OSJ"]
  dt[StationName=="Rock Slough @ Contra Costa WD Fish Screen",Station:="RSCC"]
  dt[StationName=="Sacramento River Downstream of Isleton",Station:="SOI"]
  dt[StationName=="San Joaquin River @ Mossdale Bridge - C7A",Station:="MSD"]
  dt[StationName=="Sugar Cut downstream of Tom Paine Slough",Station:="SGA"]
  dt[StationName=="Steamboat Slough",Station:="SXS"]
  dt[StationName=="Three Mile Slough at San Joaquin River",Station:="TSL"]
  dt[StationName=="Truckee River @ Bridge 8/SquawCr",Station:="TRB8"]
  dt[StationName=="Truckee River @ Farad",Station:="FAR"]
  dt[StationName=="Truckee River above Truckee-Tahoe Sanitation Agenc",Station:="TRTT"]
  dt[StationName=="Turner Cut near Holt, CA",Station:="TRN"]
  dt[StationName=="Victoria Canal near Byron",Station:="VCU"]
  dt[StationName=="Werner Dredger Cut near Palm Tract",Station:="WDC"]
  }



  #set Parameter names
  dt[Analyte=="Total Alkalinity",Parameter:="T_Alk"]
  dt[Analyte=="Alkalinity",Parameter:="Alk"]
  dt[Analyte=="Dissolved Boron",Parameter:="B"]
  dt[Analyte=="Dissolved Bromide",Parameter:="Br"]
  dt[Analyte=="Dissolved Calcium",Parameter:="Ca"]
  dt[Analyte=="Dissolved Chloride",Parameter:="Cl"]
  dt[Analyte=="Chlorophyll a",Parameter:="Chlor_a"]
  dt[Analyte=="Specific Conductance",Parameter:="SpCond_"]
  dt[Analyte=="Dissolved Hardness",Parameter:="Hardness"]
  dt[Analyte=="Total Hardness",Parameter:="Hardness"]

  dt[Analyte=="Dissolved Magnesium",Parameter:="Mg"]
  dt[Analyte=="Dissolved Nitrate",Parameter:="NO3"]
  dt[Analyte=="Dissolved Sodium",Parameter:="Na"]
  dt[Analyte=="Dissolved Potassium",Parameter:="K"]
  dt[Analyte=="Total Organic Carbon",Parameter:="TOC"]
  dt[Analyte=="Dissolved Organic Carbon",Parameter:="DOC"]
  dt[Analyte=="Pheophytin a",Parameter:="Pheo_a"]
  dt[Analyte=="Total Suspended Solids",Parameter:="TSS"]
  dt[Analyte=="Volatile Suspended Solids",Parameter:="VSS"]
  dt[Analyte=="Total Dissolved Solids",Parameter:="TDS"]
  dt[Analyte=="Dissolved Sulfate",Parameter:="SO4"]
  dt[Analyte=="pH",Parameter:="pH"]
  dt[Analyte=="Total Kjeldahl Nitrogen",Parameter:="TKN"]
  dt[Analyte=="Dissolved Ammonia",Parameter:="NH3"]
  dt[Analyte=="Dissolved Organic Nitrogen",Parameter:="DON"]
  dt[Analyte=="Dissolved Nitrate + Nitrite",Parameter:="D_NO3+NO2"]






  #reconsider what to do about Field data
  #dt <- dt[!Analyte=="*No Lab Analyses (Field Measures Only)"]
  dt[,Field:=FALSE]
  dt[stringr::str_detect(Analyte,"Field *"), Field:=TRUE]
  dt[Analyte=="Field Specific Conductance",Parameter:="SpCond"]
  dt[Analyte=="Field Dissolved Oxygen",Parameter:="DOconc"]
  dt[Analyte=="Field pH",Parameter:="pH"]
  dt[Analyte=="Field Water Temperature",Parameter:="Temp"]
  dt[Analyte=="Field Turbidity",Parameter:="Turb"]




  #check if any stations were missed


  if(length(dt[is.na(Station)]$Station)>0) {#if(length(dt[is.na(Station)&Parent_Sample=="0"]$Station)>0) { #Parent_Sample has disappeared?
    print("Warning: Missing Stations Codes")
    KeepStationName=TRUE #to identify missing stations
    }

  #dealing with Non Detects
  dt[,ND:=FALSE]
  dt[stringr::str_detect(Result,"^<"),ND:=TRUE]    #new WDL has differnt marker. Changed from dt[Result=="< R.L.",ND:=TRUE]
  dt[ND==FALSE,value:=as.numeric(Result)]
  dt[ND==TRUE,value:=RptLimit/2] #to deal with less than reporting limit values    changes between Rpt_Limit and RptLimit

  dt[,DateTime:=Dtoolbag::MkDate(CollectionDate)]   #changes between CollectionDate and Collection_Date

  #re-moving duplicates and excess columns
  if(simple){
    if(KeepStationName){
      dt <- dt[,.(value=mean(value),.N,LongStationName=LongStationName),by=.(DateTime,Station,Parameter,Analyte,RptLimit,Units,ND)]  #changes between Rpt_Limit and RptLimit
    } else{
      dt <- dt[,.(value=mean(value),.N),by=.(DateTime,Station,Parameter,Analyte,RptLimit,Units,ND)]  #changes between RptLimit and Rpt_Limit
    }

    dt <- dt[!Analyte=="*No Lab Analyses (Field Measures Only)"]

    if(!filter == 'all'){ dt <- dt[Parameter %in% filter]}

  }

  return(dt[])
}
