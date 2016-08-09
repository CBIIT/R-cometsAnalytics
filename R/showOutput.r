#---------------------------------------------------------
# Create output CSV file ---------------------------------
#---------------------------------------------------------
#' Create output CSV file
#'
#' @param filename if type_output is "CSVfile", name of file
#' @param dataf correlation output (from function getCorr())
#' @param cohort cohort name
#'
#' @return the filename of the CSV file with results named with cohort
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modbatch="1.1 Unadjusted")
#' corrmatrix <-getCorr(modeldata,exmetabdata,"DPP")
#' OutputCSVResults(filename="corr",dataf=corrmatrix,cohort="DPP") #get correlation results
#' OutputCSVResults(filename="harmonization",dataf=exmetabdata$metab,cohort="DPP") #get harmonization results
#' @export


OutputCSVResults <- function (filename,dataf,cohort=""){
  dataf=as.data.frame.list(dataf)
  fname <- gsub('.','_',tolower(cohort), fixed = TRUE)
  fname <- paste(fname,filename,Sys.Date(),'.csv',sep = '')
  utils::write.csv(dataf, fname,quote=TRUE)
  return(fname)
}
