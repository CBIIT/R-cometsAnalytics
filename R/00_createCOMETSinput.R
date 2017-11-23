#' Read in CSV files and create an Excell file that's formatted for input into Comets Analytics
#' Once the output Excell file is created, users will need to go in and complete
#' the sheets 'VarMap' and 'Models'
#' @param template template file to be used with preset parameters (options: 'age' or 'basic')
#' @param filenames names of CSV files, including path, of 
#'	1) metabolite meta data (metabolite names in first column, then meta information in other columns),
#'      2) metabolite abundances (sample_id as first column, then metabolites as other columns), 
#'      3) subject meta data (sample_id as first column, then meta information)
#' 	The filenames MUST BE IN THE ORDER SPECIFIED ABOVE
#' @param varmap data.frame with the id names for subjects, metabolites, and other meta information.
#'  If "age" is the template, then variable ids for 'id' (subject id), 'age', 'bmi', 'metabid' need
#'  to be defined.
#'  If "basic" is the template, then variable ids for 'id' (subject id) and 'metabid' need to be defined
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
#' varmap=data.frame(metabolite_id="METABID", id="ID",
#'         age="AGE",bmi="BMI")
#' createCOMETSinput(filenames=filenames, 
#'       outputfile="MyData.xlsx")
#' @export

createCOMETSinput <- function(template="age",filenames=NULL,varmap=NULL,
                  outputfile=NULL){

  # Check that files exist
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
  }  else if (!file.exists(abundancesfile)) {
       stop("abundancesfile does not exist, please check name")
  } else if (!file.exists(subjfile)) {
     stop("subjfile does not exist, please check name")
  } else {
  # Get appropriate template file:
  if(template=="age") {
     dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
     templatefile <- file.path(dir, "cometsInputAge.xlsx")
  }  else if (template=="basic") {
     dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
     templatefile <- file.path(dir, "cometsInputBasic.xlsx")
  } else {
	stop("Template does not exist.  Please contact COMETS-Analytics developers so it can be included.")
  }

  # Create varmap, taking variables to map from the template
    tomap<-suppressWarnings(fixData(readxl::read_excel(templatefile, 4)))

  if(is.null(varmap)) {
        varmap=data.frame(VARREFERENCE=tomap$varreference,
                VARDEFINITION=tomap$vardefinition,
                COHORTVARIABLE=rep("Needs User Input",nrow(tomap)),
                COHORTNOTES=rep("",nrow(tomap)))
        warning("no variable mapping is provided through the varmap parameter so that sheet will be left blank")
}  else {
        # Check that the input varmap file has all entries:
        numvarmap=length(intersect(names(varmap),tomap$varreference))
        if(numvarmap != length(tomap$varreference)) {
           stop(cat("Be sure that the varmap input is correct.  It should include mapped ids for ",tomap$cohortvariable),"\n") }
        # print(varmap)
        newvarmap=tomap
        newvarmap$cohortvariable=as.character(unlist(varmap[tomap$varreference]))
        varmap=newvarmap
}

  # Create models
  models<-suppressWarnings(fixData(readxl::read_excel(templatefile, 5)))
  if(nrow(models)==0) {models[1,]=rep(NA,ncol(models))}

#  else {
	# Write the Metabolite sheet
        metab=utils::read.csv(metabfile,check.names=FALSE)
	rio::export(metab,outputfile,which="Metabolites")
#	xlsx::write.xlsx(metab,outputfile,sheetName="Metabolites",
#		row.names=FALSE,showNA=FALSE)

        # Write the SubjectMetabolites sheet
	abund=utils::read.csv(abundancesfile,check.names=FALSE)
	rio::export(abund,outputfile,which="SubjectMetabolites",overwrite=FALSE)
#        xlsx::write.xlsx(abund,outputfile,sheetName="SubjectMetabolites",
#		row.names=FALSE,append=TRUE,showNA=FALSE)

	# Write the SubjectData sheet
	subj=utils::read.csv(subjfile,check.names=FALSE)
	rio::export(subj,outputfile,which="SubjectData",overwrite=FALSE) 
#       xlsx::write.xlsx(subj,outputfile,sheetName="SubjectData",row.names=FALSE,
#		append=TRUE,showNA=FALSE)

	# Write the VarMap sheet
	rio::export(varmap,outputfile,which="VarMap",overwrite=FALSE)
#        xlsx::write.xlsx(varmap,outputfile,sheetName="VarMap",row.names=FALSE,
#		append=TRUE,showNA=FALSE)

	#Write the Models sheet
	rio::export(models,outputfile,which="Models",overwrite=FALSE)
#       xlsx::write.xlsx(models,outputfile,sheetName="Models",row.names=FALSE,
#		append=TRUE,showNA=FALSE)
  }
} # end of function
