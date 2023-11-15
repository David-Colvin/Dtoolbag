#' Read WQES tables
#'
#' reads tbl_Stations and tbl_Parameters (first from Z drive, if available, or from R library)
#' @param update if TRUE, copies files tbl_Parameters.txt and tbl_Stations.txt from Share drive folder Z://pcdistfs1/share/Water Quality Evaluation Section/Tools/Rstuff/data_files/db_tables to user's current R library Dtoolbag folder. If Error occures, verify connection to //pcdistfs1/share/. Alternatively, copy table files from Water Quality Evaluation Section/Tools/Rstuff/data_files/db_tables and paste into your R library Dtoolbag folder (replacing existing files)
#' @details Loads the tbl_Stations and tbl_Parameters from the R library Dtoolbag folder and returns as a list with $Stations and $Parameters.
#' @return list of tbl_Stations and tbl_Parameters from Package
#' @import stringr
#' @export

WQES_tbls <-function(update=FALSE){
  #consider code to update tables in R library folders from z: drive
  if(update){
    Updates=NULL
    Updates[1] <- file.copy(from = "//pcdistfs1/share/Water Quality Evaluation Section/Tools/Rstuff/data_files/db_tables/tbl_Stations.txt",
                            to = paste0(path.package("Dtoolbag"), "/tbl_Stations.txt"), overwrite = TRUE, copy.date = TRUE)
    Updates[2] <- file.copy(from = "//pcdistfs1/share/Water Quality Evaluation Section/Tools/Rstuff/data_files/db_tables/tbl_Parameters.txt",
                            to = paste0(path.package("Dtoolbag"), "/tbl_Parameters.txt"), overwrite = TRUE, copy.date = TRUE)
    if(!any(Updates)){print( "Error updating tbl_Stations pr tbl_Parameters. See ?WQES_tbls() for details")}
  }


  tbl_Stations<-data.table::fread(file = paste0(path.package("Dtoolbag"), "/tbl_Stations.txt"))
  tbl_Parameters<-data.table::fread(file = paste0(path.package("Dtoolbag"), "/tbl_Parameters.txt"))
  return(list(Stations=tbl_Stations,Parameters=tbl_Parameters))

}
