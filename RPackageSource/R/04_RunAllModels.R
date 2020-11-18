#' This function allows users to run all models that are provided in the "Models" sheet of
#' the input Excel file.
#' @param readData list from \code{\link{readCOMETSinput}}
#' @param cohort cohort label (e.g. DPP, NCI, Shanghai)
#' @param writeTofile T/F (whether or not to write results for each model into
#' separate xlsx files). Files are written to current directory. Default is TRUE.
#' 
#' @return A list of return objects from \code{\link{runModel}}.
#'       The \code{ith} element in this list is the output from 
#'        the \code{ith} model run.
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
  fname0  <- gsub('.', '_', tolower(cohort), fixed = TRUE)

  for (i in mymodels) {
    errFlag <- 0
    print(paste("Running",i))
    mymod <- try(getModelData(readData,modlabel=i))
    if (!("try-error" %in% class(mymod))) {
      myobj <- try(runModel(mymod, readData, cohort=cohort, op=NULL))
      if ("try-error" %in% class(mymod)) errFlag <- 1
    } else {
      myobj   <- mymod
      errFlag <- 1 
    }
    results[[i]] <- myobj
    if (writeTofile && !errFlag) {
      fname <- paste0(i, ".", fname0, Sys.Date(), '.xlsx')
      OutputListToExcel(fname, myobj)
    }
  }
  return(results)
}

