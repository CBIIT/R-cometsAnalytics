
\name{runModel}
\alias{runModel}
\title{Generic function to call models}
\usage{
runModel(modeldata, metabdata, cohort="", model="pcorr", family="gaussian",
         link="identity", ...)
}
\arguments{
\item{modeldata}{list from function getModelData}
\item{metabdata}{metabolite data list}
\item{cohort}{cohort label (e.g DPP, NCI, Shanghai)}
\item{model}{string for the type of model to run, currently must be "pcorr" or "glm".
             The default is "pcorr".}
\item{family}{string for the family to use if \code{model = "glm"}.
             See \code{\link[stats]{family}} for the possible values.
             The default is "gaussian".}
\item{link}{string for the link function to use if \code{model = "glm"}.
            See \code{\link[stats]{family}} for the possible values.
            The default is "" so that the canonical link will be used.}
\item{...}{additional arguments passed to \code{\link[stats]{glm}}}
}
\value{
data frame with each row representing the model results for each combination
 of outcomes and exposures represented as specified in the
model (*spec), label (*lab), and universal id (*_uid)
with additional columns for n, pvalue, method of model specification (Interactive or Batch), 
universal id for outcomes (outcome_uid) and exposures (exposure_uid)
name of the cohort, adjustment (adjvars) and stratification (stratavar,strata)  variables. 
Attribute of dataframe includes ptime for processing time of model run.
}
\description{
This function will check for strata and if present, it will run the model within each strata. 
Each model design is checked for validity (correlation between predictors, zero variance, etc.).
}
\examples{
dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
csvfile <- file.path(dir, "cometsInputAge.xlsx")
exmetabdata <- readCOMETSinput(csvfile)
modeldata <- getModelData(exmetabdata,colvars="age",modlabel="1 Gender adjusted",
	rowvars=c("lactose","lactate"))
ret <- runModel(modeldata,exmetabdata, "DPP")
}
