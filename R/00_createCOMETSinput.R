#' Read in CSV files and create an Excell file that's formatted for input into Comets Analytics
#' Once the output Excell file is created, users will need to go in and complete
#' the sheets 'VarMap' and 'Models'
#' @param metabfile CSV file name, including path, of metabolite meta data
#' @param subjfile CSV file name, includingpath, of subject meta data
#' @param abundancesfile CSV file name, including path, of metabolite abundances
#' @param outputfile name, including path,  of output .xlsx file
#'
#' @return NULL
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' metabfile <- file.path(dir, "testmetab.csv")
#' subjfile <- file.path(dir, "testsubject.csv")
#' abundancesfile <- file.path(dir, "testabundances.csv")
#' createCOMETSinput(metabfile=metabfile, 
#'       subjfile=subjfile,
#'       abundancesfile=abundancesfile,
#'       outputfile="MyData.xlsx")
#'
#' @export

createCOMETSinput <- function(metabfile=NULL,
                  subjfile=NULL,
                  abundancesfile=NULL,
                  outputfile=NULL){

  if (is.null(metabfile) || is.null(subjfile) ||
       is.null(abundancesfile) || is.null(outputfile)) {
      stop("Be sure that all input files and outputfile are passed onto the function")
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
	# Write the Metabolite sheet
        metab=read.csv(metabfile)
	xlsx::write.xlsx(metab,outputfile,sheetName="Metabolite",
		row.names=FALSE,showNA=FALSE)

        # Write the SubjectMetabolites sheet
	abund=read.csv(abundancesfile)
        xlsx::write.xlsx(abund,outputfile,sheetName="SubjectMetabolites",
		row.names=FALSE,append=TRUE,showNA=FALSE)

	# Write the SubjectData sheet
	subj=read.csv(subjfile)
        xlsx::write.xlsx(subj,outputfile,sheetName="SubjectData",row.names=FALSE,
		append=TRUE,showNA=FALSE)

	# Write the VarMap sheet
	varmap=data.frame(VARREFERENCE=c("id","age","bmi","metabolite_id"),
		VARDEFINITION=c("cohort subject id","age measured","BMI in kg/m2",
		"metabolite id"),
		COHORTVARIABLE=rep("",4),
		COHORTNOTES=rep("",4))
        xlsx::write.xlsx(varmap,outputfile,sheetName="VarMap",row.names=FALSE,
		append=TRUE,showNA=FALSE)

	#Write the Models sheet
	models=data.frame(MODEL=c("1.1 Unadjusted","1.2 BMI adjusted"),
		OUTCOMES=c("All metabolites","All metabolites"),
		EXPOSURE=c("age","age"),
		ADJUSTMENT=c("",""))
        xlsx::write.xlsx(models,outputfile,sheetName="Models",row.names=FALSE,
		append=TRUE,showNA=FALSE)
  }
} # end of function
