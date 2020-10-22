#---------------------------------------------------------
#' Calculate correlation matrix for input model. This function will check for strata and if present, it will run the correlation matrix within each strata. Each model design is checked for validity (correlation between predictors, zero variance, etc.).
#'
#' @param modeldata list from function getModelData
#' @param metabdata metabolite data list
#' @param cohort cohort label (e.g DPP, NCI, Shanghai)
#'
#' @return data frame with each row representing the correlation for each combination of outcomes and exposures represented as specified in the
#' model (*spec), label (*lab), and universal id (*_uid)
#' with additional columns for n, pvalue, method of model specification (Interactive or Batch), universal id for outcomes (outcome_uid) and exposures (exposure_uid)
#' name of the cohort, adjustment (adjvars) and stratification (stratavar,strata)  variables. Attribute of dataframe includes ptime for processing time of model
#' run.
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modlabel="1 Gender adjusted",
#' 	rowvars=c("lactose","lactate"))
#' corrmatrix <- runCorr(modeldata,exmetabdata, "DPP")
#' @export
runCorr <- function(modeldata, metabdata, cohort = "") {
  runModel(modeldata, metabdata, cohort=cohort, model="pcorr")
}

calcCorr <- function(modeldata, metabdata, cohort = "") {
  runModel(modeldata, metabdata, cohort=cohort, model="pcorr")
}

