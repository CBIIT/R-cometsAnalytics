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
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAgeTest.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' allmodeloutput <- runAllModels(exmetabdata)
#'
#' @export

runAllModels <- function(readData, cohort="", writeTofile=T) {

  mymodels <- readData$mods$model

  results <- list()

  for (i in mymodels) {
        print(paste("Running",i))
	mymod <- getModelData(readData,modlabel=i)
        mycorr <- runCorr(mymod,readData,cohort)
	results[[i]] <- mycorr
        if (writeTofile) {
              OutputCSVResults(i,mycorr,cohort=cohort)
        }
  }
  return(results)
}



#' This function allows users to run all models that are provided in the "Models" sheet of
#' the input Excell file.
#' @param readData list from readComets
#' @param cohort cohort label (e.g. DPP, NCI, Shanghai)
#'
#' @return a list with 2 data frames, continuous and categorical summaries. Type of variable is defined in varmap
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAgeTest.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' allmodeloutput <- runAllModels(exmetabdata)
#' # Get descriptive data
#' descdata <-runDescrip(exmetabdata)
#' OutputXLSResults(filename="corr",datal=descdata,cohort="DPP")
#'
#' @export

runDescrip<- function(readData){
  sumcat<-variable<-value<-NULL
  # check if vartype is in vmap to see whether anyvars are categorical
  if (length(which(grepl("vartype",names(readData$vmap))))>0){

  	catvars<-names(readData$subjdata)[which(sapply(readData$subjdata, is.factor)==TRUE)]

  	msdata<-readData$subjdata %>%
    		select_(catvars)
  	msdata <- suppressWarnings(data.table::melt(readData$subjdata,measure.vars=catvars))
  	sumcat <- msdata %>%
    	group_by(variable, value) %>%
    	summarise (n = n()) %>%
    	mutate(proportion = n / sum(n))
  }

  sumcnt <-as.data.frame(psych::describe(readData$subjdata,quant = c(.05,.25,.5,.75,.95)))
  sumcnt$vars<-rownames(sumcnt)

  return(list(sumcat=sumcat,sumall=sumcnt))

}
