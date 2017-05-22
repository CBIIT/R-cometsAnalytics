#---------------------------------------------------------
# Create output CSV file ---------------------------------
#---------------------------------------------------------
#' Create output CSV file
#'
#' @param filename if type_output is "CSVfile", name of file and can include path
#' @param dataf correlation output (from function getCorr())
#' @param cohort cohort name
#'
#' @return the filename of the CSV file with results named with cohort
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modlabel="1.1 Unadjusted")
#' corrmatrix <-getCorr(modeldata,exmetabdata,"DPP")
#' # Get correlation results
#' OutputCSVResults(filename="corr",dataf=corrmatrix,cohort="DPP")
#' # Get harmonization results
#' OutputCSVResults(filename="harmonization",dataf=exmetabdata$metab,cohort="DPP")
#' @export


OutputCSVResults <- function (filename,dataf,cohort=""){
  fname <- gsub('.','_',tolower(cohort), fixed = TRUE)
  fname <- paste0(filename,fname,Sys.Date(),'.csv')
  utils::write.csv(dataf, fname,quote=TRUE)
  return(fname)
}
