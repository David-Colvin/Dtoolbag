#' Find file names
#'
#'  Find file names in either working directory or Documents/Rstuff/data_files
#' @param path Project name (or sub directory with either working directory or Rstuff/data_files)
#' @param string regular expression used to search file names. Default is "*" which returns all files. (use "*.csv" to return all .csv files)
#' @param nRstuff (Default is FALSE) search path srating in working directory. if TRUE: search path starting in Rstuff/data_files.
#' @return vector of file names matching string within folder specified by path
#' @details Used to search and get file names and absolute paths.
#' @import stringr
#' @export

Files4R <- function(path="",string="*",nRstuff=FALSE){
  #path: path to relevant folder starting at Documents/Rstuff/data_files/
  #string:  regular expression, most commonly "*.csv" or "*.csv". Use "*" to return ALL files
  if(!path=="" & !stringr::str_detect( path, "/$")){path=paste0(path,"/")}


  if(nRstuff){
    Search <- paste0(GetPath2Rstuff(),"/data_files/",path,string)
  }else {
    Search <- paste0(getwd(),"/",path,string)
  }

    files=Sys.glob(Search)

  return(files)
}#returns a list of files that meet the search results with in specified folder path






#' Return most recent file
#'
#' Return most recent file in a folder from specified path.
#' @param folderPath the path to a folder
#' @export

getMostRecentFile <- function(folderPath) {
  files <- list.files(folderPath, full.names = TRUE)
  fileInfos <- file.info(files)
  mostRecentFile <- files[which.max(fileInfos$mtime)]
  return(mostRecentFile)
}
