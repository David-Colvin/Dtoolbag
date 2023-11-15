#' save R plots as .png
#'
#' Save R plots to working directory (or ...?) using set sizes that match well in word docs
#'
#' @param plotname file name (include extra folders to embed within working directory)
#' @param plot R plot. (not tested with non-ggplot plots)
#' @param size (Default is 'medium') (c(Width,Height) in inches) or use options: \cr
#' medium - (7.5x4) fits two plots per page \cr
#' large - (10x6.5) fits one plot per page landscape \cr
#' tall - (7.5x7.5) fits one plot per page with space for paragraph \cr
#' taller - (7.5x9.5) fits one plot per page portrate with space for caption
#' @param folder (default = 'Plots')folder Specify a folder within working directory
#' @return absolute path to saved plot file
#' @details Saves a plot into your working directory using preset sizes for uniformity
#' @import ggplot2
#' @export

Dplotsave=function(plotname,plot,size='medium',folder='plots',PlotFile=NULL){
  #edits 20200324
  #not if crashes use dev.off()
  #PlotFile is Plot with File path (what returned from function)
    Width=NULL
    Height=NULL
  #set plot size
  if(length(size)>1){
    Width <- size[1]
    Height <- size[2]
  }else{
    if(size == "medium"){
      Width <- 7.5
      Height <- 4.5
    }
    if(size == "large"){
      Width <- 10
        Height <-  6.04
    }
    if(size == "tall"){
      Width <- 7.5
        Height <-  7.5
    }
    if(size == "taller"){
      Width <- 7.5
        Height <- 9.5
    }


  }


  if(is.null(PlotFile)){path = paste0(getwd())
  if(!folder==""){path=paste0(path,"/",folder)}
  if(!file.exists(path)){dir.create(file.path(path))}
  PlotFile = paste0(path,"/",plotname,".png")
  }

  #print(Plot)

  if(is.null(plot)|class(plot)[1]=="character") { #DC20201112 previous was class(p)?
    print("you forgot something!")
  }else {
    #if(size=="medium"){ggplot2::ggsave(PlotFile,plot,width=7.5,height=4.5,unit="in")} # fits two plots with figure caps per page
    #if(size=="large") {ggplot2::ggsave(PlotFile,plot,width=10,height=6.04,unit="in")}  # fits one plot per page landscape
    #if(size=="tall") {ggplot2::ggsave(PlotFile,plot,width=7.5,height=7.5,unit="in")}   #fits one plot perpage with space for paragraph
    #if(size=="taller") {ggplot2::ggsave(PlotFile,plot,width=7.5,height=9,unit="in")} #fits one plot perpag
    ggplot2::ggsave(PlotFile,plot,width=Width,height=Height,unit="in")
  }

  return(PlotFile)
}
