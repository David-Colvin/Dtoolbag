% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dwrite.R
\name{Dwrite}
\alias{Dwrite}
\title{Write data.table to CSV file}
\usage{
Dwrite(dt, file = "", nRstuff = FALSE)
}
\arguments{
\item{file}{file name (include path if different from working directory or data_files)}

\item{nRstuff}{Default is FALSE, and writes to working directory. if data=TRUE writes to Rstuff/data_files}
}
\value{
absolute path to file
}
\description{
write data.tables to working directory (or Rstuff/data_files)
}
\details{
Times are easily read in excel and can be read back in using fread and date time column converted using MkDate()
}
