#---------------------------------------------------------
# Create output CSV file ---------------------------------
#---------------------------------------------------------
#' Create output CSV file
#'
#' @param type_output "display" or "CSVfile"
#' @param filename if type_output is "CSVfile", name of file
#' @param dataf correlation output (from function getCorr()) data frame
#' @param cohort cohort name
#'
#' @return the filename of the CSV file with results named with cohort
#'
#' @examples
#' dir <- system.file("extdata", package="CometsAnalyticsPackage", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modbatch="1.1 Unadjusted")
#' corrmatrix <-getCorr(modeldata,exmetabdata,"DPP")
#' makeOutputCSV(type_output="CSVfile",filename="corr",dataf=corrmatrix,cohort="DPP")
#' @export


makeOutputCSV <- function (type_output="display",filename,dataf,cohort=""){
  fname <- gsub('.','_',tolower(cohort), fixed = TRUE)
  fname <- paste(fname,filename,Sys.Date(),'.csv',sep = '')
  if (type_output == "display") {
	return(dataf)
  }
  else if (type_output == "CSVfile") {
	write.csv(dataf, fname,quote=TRUE)
  }
  else {
	stop("type_output variable needs to be 'CSVfile' or 'display'")
  }
}
