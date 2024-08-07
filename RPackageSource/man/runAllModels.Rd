% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_RunAllModels.R
\name{runAllModels}
\alias{runAllModels}
\title{This function allows users to run all models that are provided in the "Models" sheet of
the input Excel file.}
\usage{
runAllModels(readData, cohortLabel = "", writeTofile = TRUE)
}
\arguments{
\item{readData}{list from \code{\link{readCOMETSinput}}}

\item{cohortLabel}{cohort label (e.g. DPP, NCI, Shanghai)}

\item{writeTofile}{TRUE/FALSE (whether or not to write results for each model into
separate xlsx files). Files are written to current directory. Default is TRUE.}
}
\value{
A list of return objects from \code{\link{runModel}} or \code{\link{runCorr}}.
      The \code{ith} element in this list is the output from 
       the \code{ith} model run.
}
\description{
This function allows users to run all models that are provided in the "Models" sheet of
the input Excel file.
}
\examples{
\dontrun{
dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
csvfile <- file.path(dir, "cometsInputAge.xlsx")
exmetabdata <- readCOMETSinput(csvfile)
allmodeloutput <- runAllModels(exmetabdata)
}
}
