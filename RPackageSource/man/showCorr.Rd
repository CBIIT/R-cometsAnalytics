% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plottingfunctions.R
\name{showCorr}
\alias{showCorr}
\title{Function that returns top N lines of the \code{\link{runCorr}} output}
\usage{
showCorr(corr, nlines = 50)
}
\arguments{
\item{corr}{returned object from \code{\link{runCorr}}}

\item{nlines}{number of lines to return (default 50)}
}
\value{
first 50 lines of output
}
\description{
Function that returns top N lines of the \code{\link{runCorr}} output
}
\examples{
dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
csvfile <- file.path(dir, "cometsInputAge.xlsx")
exmetabdata <- readCOMETSinput(csvfile)
modeldata <- getModelData(exmetabdata,modlabel="1 Gender adjusted")
corrmatrix <- runCorr(modeldata,exmetabdata,"DPP")
showCorr(corrmatrix)
}
