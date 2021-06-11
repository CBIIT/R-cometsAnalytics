#' This function allows users to run all models that are provided in the "Models" sheet of
#' the input Excel file.
#' @param readData list from \code{\link{readCOMETSinput}}
#' @param cohortLabel cohort label (e.g. DPP, NCI, Shanghai)
#' @param writeTofile T/F (whether or not to write results for each model into
#' separate xlsx files). Files are written to current directory. Default is TRUE.
#' 
#' @return A list of return objects from \code{\link{runModel}} or \code{\link{runCorr}}.
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

runAllModels <- function(readData, cohortLabel="", writeTofile=T) {

  mymodels <- readData$mods$model

  results <- list()
  fname0  <- cohortLabel
  fname0  <- gsub('/', '_', fname0, fixed = TRUE)
  fname0  <- gsub('\\', '_', fname0, fixed = TRUE)

  for (i in mymodels) {
    errFlag <- 0
    cat(paste0("Running ",i, "\n"))
    mymod <- try(getModelData(readData,modlabel=i))
    if (!("try-error" %in% class(mymod))) {
      myobj <- try(run1Model(mymod, readData, cohortLabel=cohortLabel))
      if ("try-error" %in% class(mymod)) errFlag <- 1
    } else {
      myobj   <- mymod
      errFlag <- 1 
    }
    results[[i]] <- myobj
    if (writeTofile && !errFlag) {
      i2    <- gsub('/', '_', i, fixed=TRUE)
      i2    <- gsub('\\', '_', i2, fixed=TRUE)
      fname <- paste0(i2, ".", fname0, ".", Sys.Date(), ".xlsx")
      OutputListToExcel(fname, myobj)
    }
  }
  return(results)
}

# Function to call runCorr or runModel
run1Model <- function(mymod, readData, cohortLabel="") {

  flag <- mymod[[getOldCorrModelName(), exact=TRUE]]
  if (is.null(flag)) flag <- FALSE
  if (flag) {
    ret <- runCorr(mymod, readData, cohort=cohortLabel)
  } else {
    ret <- runModel(mymod, readData, cohortLabel=cohortLabel, op=NULL)
  }
  ret

} # END: run1Model

