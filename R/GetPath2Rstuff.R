#' Set Path2Rstuff
#'
#' Use by Path2Rstuff<-GetPath2Rstuff()
#' Assumes computer has path Documents/(User Name)/Rstuff
#' Allows for reading and writing data files to Rstuff/data_files/(project names)
#' @return The path way to Rstuff. If path cannot be determined defaults to getwd()
#' @import stringr
#' @export

GetPath2Rstuff <- function(){ #set path to data_files
  #note: must be in getwd() must say "~/Documents" when you source script

  #"\\\\pcdistfs1/share/Water Quality Evaluation Section/Data Files/YSI FILES/WORKING FILES"
  getwd()->Path2Rstuff

  if(stringr::str_detect(Path2Rstuff,"/Users/")){
    Path2Rstuff=paste0("C:/Users/",stringr::str_match(Path2Rstuff,"/Users/([A-Za-z]{1,20})/")[2],"/Documents/Rstuff")
  }else {
    print(paste("Warning! Path2Rstuff could not be found. Use setwd('C:/Users/______/Documents/Rstuff') or see GetPath2Rstuff() to further investigate")) #, needs to be with the working directory on your personal computer. Use setwd() to change directory. GetData, Dread and Dwrite functions will not work because Path2Rstuff could not be set.")
    # Path2Rstuff=NULL
  }

  if(stringr::str_detect(Path2Rstuff,"Users/david")){
    Path2Rstuff="C:/Users/david/Documents/Computer/Rstuff"

  }

  if(!file.exists(Path2Rstuff)){warning(paste0("Cannot find folder: ",Path2Rstuff))

    return(getwd())#what if i return getwd()?
  }else {
    return(Path2Rstuff)
  }
}#set path to data_files



