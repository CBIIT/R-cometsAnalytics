% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plottingfunctions.R
\name{showHClust}
\alias{showHClust}
\title{Show interactive heatmap using heatmaply with hierarchical clustering}
\usage{
showHClust(ccorrmat, clust = TRUE, colscale = "RdYlBu")
}
\arguments{
\item{ccorrmat}{correlation matrix (output of runCorr())}

\item{clust}{Show hierarchical clustering}

\item{colscale}{colorscale, can be custom or named ("Hots","Greens","Blues","Greys","Purples") see \code{\link[heatmaply]{RColorBrewer_colors}}}
}
\value{
a heatmap with outcomes as rows and exposures in columns.
}
\description{
This function outputs a heatmap with hierarchical clustering.  It thus requires you to have at least 2 outcome and 2 exposure variables in your models.
}
\examples{
dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
csvfile <- file.path(dir, "cometsInputAge.xlsx")
exmetabdata <- readCOMETSinput(csvfile)
modeldata <- getModelData(exmetabdata, modelspec="Interactive",
colvars=c("age","bmi_grp"),rowvars=c("lactose","lactate"))
corrmatrix <-runCorr(modeldata,exmetabdata,"DPP")
showHClust(corrmatrix)
}
\references{
For colorscale reference: \code{\link[heatmaply]{RColorBrewer_colors}}
}
