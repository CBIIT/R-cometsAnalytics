#' This function allows users to run all models that are provided in the "Models" sheet of
#' the input Excell file.
#' @param readData list from readComets
#' @param cohort cohort label (e.g. DPP, NCI, Shanghai)
#' @param writeTofile T/F (whether or not to write results for each model onto
#' separate xls file). Files are written to current directory. Default is True.
#'
#' @return a list of data frames, where each data frame has rows representing the correlation for each combination of outcomes and exposures with additional columns for n, pvalue, metabolite_id, method of model specification (Interactive or Batch), name of the cohort and adjustment variables. Attribute of dataframe includes ptime for processing time of model run.
#'
#' @examples
#' \dontrun{
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' allmodeloutput <- runAllModels(exmetabdata)
#' }
#' @export

runAllModels <- function(readData, cohort="", writeTofile=T) {

  mymodels <- readData$mods$model

  results <- list()

  for (i in mymodels) {
    errFlag <- 0
    print(paste("Running",i))
    mymod <- try(getModelData(readData,modlabel=i))
    if (!("try-error" %in% class(mymod))) {
      mycorr <- try(runCorr(mymod,readData,cohort))
      if ("try-error" %in% class(mycorr)) errFlag <- 1
    } else {
      mycorr  <- mymod  
      errFlag <- 1
    }
    results[[i]] <- mycorr
    if (writeTofile && !errFlag) {
      OutputCSVResults(i,mycorr,cohort=cohort)
    }
  }
  return(results)
}

