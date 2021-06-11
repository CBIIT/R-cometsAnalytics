#---------------------------------------------------------
#' Calculate correlations for input model.
#'
#' @param modeldata list from function \code{\link{getModelData}}
#' @param metabdata metabolite data list from \code{\link{readCOMETSinput}}
#' @param cohort cohort label (e.g DPP, NCI, Shanghai)
#'
#' @details This function is a special case of \code{\link{runModel}}
#'          with the option \code{op$model = "correlation"}, 
#'          however for backwards compatibility, it returns a data frame as
#'          in the original version of the \bold{COMETS} R package.
#'
#' @return data frame with each row representing the correlation for each combination of outcomes and 
#' exposures represented as specified in the model (*spec), label (*lab), and universal id (*_uid)
#' with additional columns for n, pvalue, method of model specification (Interactive or Batch), 
#' universal id for outcomes (outcome_uid) and exposures (exposure_uid)
#' name of the cohort, adjustment (adjvars) and stratification (stratavar,strata)  variables. 
#' Attribute of dataframe includes ptime for processing time of model run.
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,exposures="age",modlabel="1 Gender adjusted",
#' 	outcomes=c("lactose","lactate"), modelspec="Interactive")
#' corrmatrix <- runCorr(modeldata,exmetabdata, "DPP")
#' @export
runCorr <- function(modeldata, metabdata, cohort = "") {
  calcCorr(modeldata, metabdata, cohort=cohort) 
}

calcCorr <- function(modeldata, metabdata, cohort = "", op=NULL) {
  if (!length(op)) op <- list(model=getCorrModelName())
  if (is.list(op)) op$model <- getCorrModelName()
  ret <- runModel(modeldata, metabdata, cohortLabel=cohort, op=op)

  # For backwards compatibility, convert the output from runModel to old format
  ret <- newVersionOutToOldOut(ret)

  ret
}

