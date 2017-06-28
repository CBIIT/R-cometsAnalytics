#' Optional function that subsets input data (e.g. only perform analysis on female subjects)
#'
#' @param readData list from readComets
#' @param where users can specify which subjects to perform the analysis by specifying this parameter. 'where' expects a vector with a variable name, a comparison operator (e.g. "<", ">", "="), and a value.  For example, "where = c("Gender","=","Female")
#' @return filtered list
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' filtexmetabdata <- filterCOMETSinput(exmetabdata, where <- c("age",">","60"))
#'
#' @export
filterCOMETSinput <- function(readData,where=NULL) {

  # Filter out samples if 'where' parameter is not null
  if (!is.null(where)) {
	if(where[2] == "<") {
 	       samplesToKeep <- which(readData$subjdata[,where[1]] < where[3])
	} else
	if(where[2] == ">") {
               samplesToKeep <- which(readData$subjdata[,where[1]] > where[3])
        } else
	if (where[2] == "=") {
               samplesToKeep <- which(readData$subjdata[,where[1]] == where[3])
        } else
		stop("Make sure the second element in the vector where is either '>', '<', or '='")
	}
	readData$subjdata <- readData$subjdata[samplesToKeep,]
return(readData)
}
