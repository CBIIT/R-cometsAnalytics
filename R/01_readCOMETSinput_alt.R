#' Read in CSV files and create an Excell file that's formatted for input into Comets Analytics
#' Once the output Excell file is created, users will need to go in and complete
#' the sheets 'VarMap' and 'Models'
#' @param metabfile CSV file name, including path, of metabolite meta data
#' @param subjfile CSV file name, includingpath, of subject meta data
#' @param abundancesfile CSV file name, including path, of metabolite abundances
#' @param output name, including path,  of output .xlsx file
#'
#' @return NULL
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' metabfile <- file.path(dir, "metabfile.csv")
#' subjfile <- file.path(dir, "subjfile.csv")
#' abundancesfile <- file.path(dir, "abundances.csv")
#' createCOMETSinput(metabfile=metabfile, 
#'       subjfile=subjfile,
#'       abundancesfile=abundancesfile,
#'       output="MyData.xlsx")
#'
#' @export

createCOMETSinput <- function(metabfile=NULL,
                  subjfile=NULL,
                  abundancesfile=NULL,
                  output=NULL){

  if (is.null(metabfile) || is.null(subjfile) ||
       is.null(abundancesfile) || is.null(output)) {
      stop("Be sure that all input files and output are passed onto the function")
  }

  if (!file.exists(metabfile)) {
     stop("metabfile does not exist, please check name")
  }
  else if (!file.exists(subjfile)) {
     stop("subjfile does not exist, please check name")
  }
  else if (!file.exists(abundancesfile)) {
       stop("abundancesfile does not exist, please check name")
  }

  else {
   return(NULL)
  }
} # end of function
