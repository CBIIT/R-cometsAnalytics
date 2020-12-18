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
#' modeldata <- getModelData(exmetabdata,exposures="age",modlabel="1 Gender adjusted",
#' 	outcomes=c("lactose","lactate"))
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
#' modeldata <- getModelData(exmetabdata,exposures="age",modlabel="1 Gender adjusted",
#'	outcomes=c("lactose","lactate"))
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

#' Create an excel xlsx file from a list of data frames
#'
#' @param filename Name of file and can include path. It must have a ".xlsx" extension.
#' @param obj List of data frames or matrices
#'
#' @return NULL
#'
#' @export
OutputListToExcel <- function(filename, obj) {

  N <- length(obj)
  if (!N) return(NULL)

  filename <- trimws(filename)
  len      <- nchar(filename)
  if (len < 5) stop("ERROR: filename is not valid")
  str <- tolower(substr(filename, len-4, len))
  if (str != ".xlsx") stop("ERROR: filename must have a .xlsx extension")
  if (file.exists(filename)) file.remove(filename)

  nms <- trimws(names(obj))
  if (!length(nms)) nms <- paste("object ", 1:N, sep="")
  tmp <- nchar(nms) < 1
  if (any(tmp)) nms[tmp] <- paste("object ", (1:N)[tmp], sep="")

  over <- TRUE
  for (i in 1:N) {
    tmp <- obj[[i]]
    if (length(tmp) && (is.data.frame(tmp) || is.matrix(tmp))) {
      rio::export(tmp, filename, which=nms[i], overwrite=over)
      over <- FALSE
    }
  }
  
  NULL
}

