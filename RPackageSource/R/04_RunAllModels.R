#' This function allows users to run all models that are provided in the "Models" sheet of
#' the input Excell file.
#' @param readData list from readComets
#' @param cohort cohort label (e.g. DPP, NCI, Shanghai)
#' @param writeTofile T/F (whether or not to write results for each model onto
#' separate xls file). Files are written to current directory. Default is True.
#' @param model "pcorr" or "glm". The default is "pcorr".
#' @param family one of: "binomial", "gaussian", "Gamma", "inverse.gaussian",
#'             "poisson", "quasi", "quasibinomial", "quasipoisson".
#' This option is only valid with model="pcorr", and the default value is "gaussian".
#' @param link the link function to use with the family option above. 
#'  The default value is to use the canonical link.
#' @param ... other options for model="glm".
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

runAllModels <- function(readData, cohort="", writeTofile=T,
                  model="pcorr", family="gaussian", link="", ...) {

  mymodels <- readData$mods$model

  results <- list()

  for (i in mymodels) {
       print(paste("Running",i))
	mymod <- getModelData(readData,modlabel=i)
       myobj <- runModel(mymod, readData, cohort=cohort, model=model,
                         family=family, link=link, ...)

	results[[i]] <- myobj
       if (writeTofile) {
         # myobj will be a list for glm
         d <- dim(myobj)
         if (is.null(d)) {
           OutputCSVResults(i,myobj,cohort=cohort)
         } else {
           nms <- paste(names(myobj), i, sep="")
           for (j in 1:length(nms)) OutputCSVResults(nms[j],myobj[[j]],cohort=cohort)
         }
       }
  }
  return(results)
}

