#---------------------------------------------------------
#' Calculate correlation matrix for input model.
#'
#' @param modeldata list from function \code{\link{getModelData}}
#' @param metabdata metabolite data list from \code{\link{readCOMETSinput}}
#' @param cohort cohort label (e.g DPP, NCI, Shanghai)
#' @param op list of options (see \code{\link{options}}).
#'
#' @details This function is a special case of \code{\link{runModel}}
#'          with the option \code{op$model = "correlation"}
#'
#' @return A list of objects with names \code{\link{ModelSummary}},
#'        \code{\link{Effects}}, and \code{\link{Errors_Warnings}}. 
#' Attribute of this list includes ptime for processing time of model run.
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,exposures="age",modlabel="1 Gender adjusted",
#' 	outcomes=c("lactose","lactate"), modelspec="Interactive")
#' corrmatrix <- runCorr(modeldata,exmetabdata, "DPP")
#' @export
runCorr <- function(modeldata, metabdata, cohort = "", op=NULL) {
  calcCorr(modeldata, metabdata, cohort=cohort, op=op) 
}

calcCorr <- function(modeldata, metabdata, cohort = "", op=NULL) {
  if (!length(op)) op <- list(model=getCorrModelName())
  if (is.list(op)) op$model <- getCorrModelName()
  runModel(modeldata, metabdata, cohort=cohort, op=op)
}

