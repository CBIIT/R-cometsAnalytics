#' Read in CSV files and create an Excell file that's formatted for input into Comets Analytics
#' Once the output Excell file is created, users will need to go in and complete
#' the sheets 'VarMap' and 'Models'
#' @param filenames names of CSV files, including path, of 
#'	1) metabolite meta data (metabolite names in first column, then meta information in other columns),
#'      2) metabolite abundances (sample_id as first column, then metabolites as other columns), 
#'      3) subject meta data (sample_id as first column, then meta information)
#' 	The filenames MUST BE IN THE ORDER SPECIFIED ABOVE
#' @param varmap data.frame with the id names for subjects, metabolites, and other meta information
#' @param outputfile name, including path,  of output .xlsx file
#'
#' @return NULL
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' metabfile <- file.path(dir, "testmetab.csv")
#' subjfile <- file.path(dir, "testsubject.csv")
#' abundancesfile <- file.path(dir, "testabundances.csv")
#' filenames <- list(metabfile=metabfile, abundancesfile=abundancesfile, subjfile=subjfile)
#' varmap=data.frame(metabid="METABID", id="ID",
#'         age="AGE",bmi="BMI")
#' createCOMETSinput(filenames=filenames, 
#'       outputfile="MyData.xlsx")
#' @export

createCOMETSinput <- function(filenames=NULL,varmap=NULL,
                  outputfile=NULL){

  if(is.null(varmap)) {
        varmap=data.frame(VARREFERENCE=c("id","age","bmi","metabolite_id"),
                VARDEFINITION=c("cohort subject id","age measured","BMI in kg/m2",
                "metabolite id"),
                COHORTVARIABLE=rep("",4),
                COHORTNOTES=rep("",4))
	warning("no variable mapping is provided through the varmap parameter so that sheet will be left blank")
}
  else {
	varmap=data.frame(VARREFERENCE=colnames(varmap),
		VARDEFINITION=rep("",4),
		COHORTVARIABLE=as.character(unlist(varmap)),
		COHORTNOTES=rep("",4))
}

  if (is.null(filenames) || is.null(outputfile)) {
      stop("Be sure that all input files and outputfile are passed onto the function")
  }
  if (length(filenames) != 3) {
     stop(paste("Be sure that the input parameter 'filenames' has 3 CSV names, including path\n
      Current 'filenames' content is",filenames))
  }
  metabfile=filenames$metabfile
  abundancesfile=filenames$abundancesfile
  subjfile=filenames$subjfile

  if (!file.exists(metabfile)) {
     stop("metabfile does not exist, please check name")
  }
  else if (!file.exists(abundancesfile)) {
       stop("abundancesfile does not exist, please check name")
  }
  else if (!file.exists(subjfile)) {
     stop("subjfile does not exist, please check name")
  }

  else {
	# Write the Metabolite sheet
        metab=utils::read.csv(metabfile)
	xlsx::write.xlsx(metab,outputfile,sheetName="Metabolite",
		row.names=FALSE,showNA=FALSE)

        # Write the SubjectMetabolites sheet
	abund=utils::read.csv(abundancesfile)
        xlsx::write.xlsx(abund,outputfile,sheetName="SubjectMetabolites",
		row.names=FALSE,append=TRUE,showNA=FALSE)

	# Write the SubjectData sheet
	subj=utils::read.csv(subjfile)
        xlsx::write.xlsx(subj,outputfile,sheetName="SubjectData",row.names=FALSE,
		append=TRUE,showNA=FALSE)

	# Write the VarMap sheet
        xlsx::write.xlsx(varmap,outputfile,sheetName="VarMap",row.names=FALSE,
		append=TRUE,showNA=FALSE)

	#Write the Models sheet
	models=data.frame(MODEL=c("1.1 Unadjusted","1.2 BMI adjusted"),
		OUTCOMES=c("All metabolites","All metabolites"),
		EXPOSURE=c("age","age"),
		ADJUSTMENT=c("","bmi"))
        xlsx::write.xlsx(models,outputfile,sheetName="Models",row.names=FALSE,
		append=TRUE,showNA=FALSE)
  }
} # end of function
