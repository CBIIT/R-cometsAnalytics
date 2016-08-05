#---------------------------------------------------------
# Create output CSV file ---------------------------------
#---------------------------------------------------------
#' Create output CSV file
#'
#' @param file filename
#' @param dataf dataframe
#' @param cohort cohort name
#'
#'
#' @return the filename of the CSV file with results named with cohort
#'
#' @examples
#' dir <- system.file("extdata", package="CometsAnalyticsPackage", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modbatch="1.1 Unadjusted")
#' corrmatrix <-getCorr(modeldata,exmetabdata,"DPP")
#' makeOutputCSV("corr",corrmatrix,"DPP")
#' @export
makeOutputCSV <- function (file,dataf,cohort=""){
  fname <- gsub('.','_',tolower(cohort), fixed = TRUE)
  fname <- paste(fname,file,Sys.Date(),'.csv',sep = '')
  write.csv(dataf, fname,quote=TRUE)
  fname
}
