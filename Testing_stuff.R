#GetData testing



if(FALSE){
  dt <- GetData(c("ORX","PDC"),"All",range=MkDate(c("202301010000","202310010000")),ErrorCheck=TRUE)

  dtGet=NULL
  stations=c("ORX","PDC")
  parameters="All"
  range=MkDate(c("202301010000","202310010000"))
  ErrorCheck=TRUE
  source="WDL"
  duration=NULL
  stationAS="StationCode"
  POR=FALSE
  replace=TRUE
  type="15Minute"
  time15min=TRUE
  ErrorCheck=FALSE
  report=FALSE
  dtGet=NULL
  tbl <- WQES_tbls()




  ldt<-list(c(1,2,3,4,5,6),c(1,2,3,4,5,6),NULL,1)
  ldt <- ldt[!sapply(ldt, is.null)]
}
