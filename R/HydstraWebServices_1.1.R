# Developing R script to use with Hydstra web services
# 10/20/2023
# David Colvin



#other hydstra web service URL request examples
# parameters (gets a list of all WDL available hydstra parameters)
#      'https://wdlhyd.water.ca.gov/hydstra/parameters'

# parameters/{param} (details on individual param)
#      'https://wdlhyd.water.ca.gov/hydstra/parameters/StreamLevel'

# sites (get list of sites)
#      'https://wdlhyd.water.ca.gov/hydstra/sites'

# sites/{site} (get details on individual site)
#      'https://wdlhyd.water.ca.gov/hydstra/sites/B9541000'

# site/{traces} (get available traces for a site)
#      'https://wdlhyd.water.ca.gov/hydstra/sites/B9541000/traces'

# sites/{site}/traces/{trace} (individual trace details) (data source, start and end date max and min, number of observations)
#      'https://wdlhyd.water.ca.gov/hydstra/sites/B9541000/traces/ECat25C.RAW'

# sites/{site}/traces/{trace}/points (get data, one site, one trace at a time)
#      'https://wdlhyd.water.ca.gov/hydstra/sites/B9539000/traces/WaterTemp.RAW/points?start-time=202201010000&end-time=202202010000'

#testing after sourcing script
if(FALSE){
  #library(Dtoolbag)
  #library(jsonlite)
  #library(httr)
  #StationID<-WQES_tbls()$Stations[StationCode%in%c("ORX")]$ID_HydstraWQ
  #StationID <- "B9539000"
  WDL_SiteTraces(StationID)
  WDL_SiteTraceData(StationID,"ECat25C",c("202201010000","202301010000"))

}

#' See available traces for individual WDL/Hydstra site.
#'
#' See available time series data (Traces) for individual sites to download from WDL.
#' @param hystra_id should be a valid hydstra id (eg B9539000)
#' @param json if true returns json file. (or, is a list that can be parsed as a json?)
#' @return either a table/list of available traces, or a json of available traces
#' @details use this function to see what traces (time series data) are available for a site through WDL
#' @import httr
#' @import jsonlite
#' @export

WDL_SiteTraces<-function(hydstra_id,json=FALSE,ErrorCheck=FALSE) {
  #use to get list of available traces for site
  #hystra_id: should be a valid hydstra id (eg B9539000)
  #json: if true returns json file. (or, is a list that can be parsed as a json?)

  # Define the URL
  url <- paste0('https://wdlhyd.water.ca.gov/hydstra/sites/',hydstra_id,'/traces')
  if(ErrorCheck){print(paste0("url: ",url))}
  # Fetch the content from the URL
  response <- httr::GET(url)

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    # Parse the content
    Traces <- jsonlite::fromJSON(httr::content(response, as="text",encoding="UTF-8"))

    # return (if json is true return json table, otherwise just return traces)
    if(json){
      return(Traces)
    }else {
      return(Traces$return$sites$traces)
    }
  } else {
    cat("Failed to fetch the data. Status code:", status_code(response), "\n")
  }
  print("Test: all done")
}

#' Download data from WDL using web services
#'
#' Download time series data (Traces) for individual site over specified range from WDL.
#' @param hystra_id should be a valid hydstra id (eg B9539000)
#' @param param hydstra web service param name as character (or enter a Trace)
#' @param range two variable vector for start and end date. Enter as character with format YYYYMMDDHHMM (can be entered as returned from MkDate)
#' @param json if true returns json file. (or, is a list that can be parsed as a json?)
#' @param Trace hydstra trace as character. Overirdes param if entered. (a trace in hydstra webservices is the param and duration. eg WaterTemp.Raw are point values)
#' @param type hydstra duration or processing. Default is "RAW" which is instantanious point data. Or enter MEAN for daily means ending at midnight
#' @return either a table/list of timeseries data (trace), or a json of trace
#' @details Download data from WDL hydstra web services. Data is specified from hydstra_id, param or Trace, and range.
#'          If a Trace is entered, that trace will be downloaded. otherwise trace is specified as param + type. Default type is RAW, or point data as stored in Hydstra
#' @import httr
#' @import jsonlite
#' @export

WDL_SiteTraceData<-function(hydstra_id,param="",range=NULL,json=FALSE,Trace=NULL,type="RAW",ErrorCheck=FALSE){
  #hystra_id: should be a valid hydstra id (eg B9539000)
  #param: hydstra web service param name as character (or enter a Trace)
  #range: two variable vector for start and end date. Enter as character with format YYYYMMDDHHMM
  #json: if true returns json file. (or, is a list that can be parsed as a json?)
  #Trace: hydstra trace as character. Overirdes param if entered. (a trace in hydstra webservices is the param and duration. eg WaterTemp.Raw are point values)


  #Define trace. If Trace specified use that, unless Trace is specified in input, combine param and type
  if(is.null(Trace)){Trace <- paste0(param,".",type)}

  #check if range is a posixct, if so convert to character
  if(is.null(range)){range<-c(Sys.time()-31536000,Sys.time())}
  if(class(range)[1]=="POSIXct") {range <- format(range, "%Y%m%d%H%M")}

  # Define the URL
  #example      'https://wdlhyd.water.ca.gov/hydstra/sites/B9539000/traces/WaterTemp.RAW/points?start-time=202201010000&end-time=202202010000'
  url <- paste0('https://wdlhyd.water.ca.gov/hydstra/sites/',hydstra_id,'/traces/',Trace,'/points?start-time=',range[1],'&end-time=',range[2])
  if(ErrorCheck){print(url)}

  # Fetch the content from the URL
  response <- httr::GET(url)

  # Check if the request was successful
  if (httr::status_code(response) == 200) {
    # Parse the content
    data <- jsonlite::fromJSON(httr::content(response, as="text",encoding="UTF-8"))

    # Return data (or if json is true return json object)
    if(json){
        return(data)
      } else{
        return(data$return$traces$trace[[1]]) #presumably there could be more elements to the $trace, thus the need for [[1]]
      }
  } else {
    cat("Failed to fetch the data. Status code:", status_code(response), "\n")
  }
  print("Test: all done")
}

#Dylan code as another example of using web services
if(FALSE){
  query <- list(
    'function' = 'get_ts_traces',
    'version'  = 2,
    'params'   = list(
      'site_list'  = 'HYDSYS07',
      'datasource' = 'A',
      'varfrom'    = 232.00,
      'varto'      = 232,
      'start_time' = '19970101000000',
      'end_time'   = '19970201000000',
      'data_type'  = 'mean',
      'interval'   = 'day',
      'multiplier' = 1
    )
  )

  url      <- 'https://wdlhyd.water.ca.gov/cgi/webservice.exe'
  result   <- POST(url, body=query, encode='json')
  response <- content(result, 'parsed')
}

