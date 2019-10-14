#---------------------------------------------------------
# Create output CSV file ---------------------------------
#' Create output CSV file
#'
#' @param filename name of CSV file and can include path
#' @param dataf correlation output (from function runCorr())
#' @param cohort cohort name
#'
#' @return the filename of the CSV file with results named with cohort
#'
#' @examples
#' \dontrun{
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modlabel="1 Gender adjusted",
#' 	rowvars=c("lactose","lactate"))
#' corrmatrix <-runCorr(modeldata,exmetabdata,"DPP")
#' # Get correlation results
#' OutputCSVResults(filename="corr",dataf=corrmatrix,cohort="DPP")
#' # Get harmonization results
#' OutputCSVResults(filename="harmonization",dataf=exmetabdata$metab,cohort="DPP")
#' }
#' @export


OutputCSVResults <- function (filename,dataf,cohort=""){
  fname <- gsub('.','_',tolower(cohort), fixed = TRUE)
  fname <- paste0(filename,fname,Sys.Date(),'.csv')
  utils::write.csv(dataf, fname,quote=TRUE)
  return(fname)
}




#---------------------------------------------------------
# Create output XLSX file ---------------------------------
#' Create output XLSX file
#'
#' @param filename name of file and can include path
#' @param datal data list to output (each item on list outputs to a worksheet)
#' @param cohort cohort name
#'
#' @return the filename of the XLSX file with results named with cohort
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modlabel="1 Gender adjusted",
#'	rowvars=c("lactose","lactate"))
#' # Get descriptive data
#' descdata <-runDescrip(exmetabdata)
#' OutputXLSResults(filename="corr",datal=descdata,cohort="DPP")
#' @export


OutputXLSResults <- function (filename,datal,cohort=""){
  fname <- gsub('.','_',tolower(cohort), fixed = TRUE)
  fname <- paste0(filename,fname,Sys.Date(),'.xlsx')
  rio::export(datal, fname)
  return(fname)
}
