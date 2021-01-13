#' Read in Excel file that contains metabolite data, covariate data, 
#' models, and model options.
#'
#' @param file path of Excel file to be read in. This file must contain sheets
#' with names \bold{SubjectMetabolites}, \bold{SubjectData}, \bold{VarMap}, \bold{Models},
#' and \bold{ModelOptions} (see details).
#' @return a list comprising of data and information needed for \code{\link{getModelData}}.
#'
#' @details Additional information regarding each sheet in the input Excel file is given below. 
#'
#' \bold{SubjectMetabolites} \cr
#' A table with the subject ids in the first column and metabolites as the other columns. \cr

#' \bold{SubjectData} \cr
#' A table with the subject ids in the first column and covariates as the other columns. \cr

#' \bold{VarMap} \cr
#' A table with at least the required columns \code{VARREFERENCE}, \code{COHORTVARIABLE}, 
#' and \code{VARTYPE}. The \code{COHORTVARIABLE} column must contain names that match the
#' column names in the \bold{SubjectData} table. These names will be renamed to their
#' corresponding name in the \code{VARREFERENCE} column. The \code{VARTYPE} column 
#' should have values \code{continuous} or \code{categorical} for each row. \cr

#' \bold{Models} \cr
#' A table where each row represents a model to be run, and with columns \code{MODEL}, 
#' \code{OUTCOMES}, \code{EXPOSURE}, \code{ADJUSTMENT},
#'  \code{STRATIFICATION}, \code{WHERE}, and \code{MODELSPEC}. All variable names in this
#' table must match variable names in the \code{VARREFERENCE} column of the \bold{VarMap} sheet.
#' The \code{MODEL} column is a label for the model. The \code{OUTCOMES} and \code{EXPOSURE} columns define the 
#' outcome and exposure variables for the model. Use \code{All metabolites} to specify
#' that all metabolite variables are to be included as outcomes or exposures, otherwise 
#' use a space separated list of variable names. The \code{ADJUSTMENT} column contains a
#' space separated list of covariate adjustment variables; use an empty cell for no covariate adjustment.
#' The \code{STRATIFICATION} column is used for stratified analyses, with a space separated list
#' of stratification variables. If more than one stratification variable is specified, then the strata
#' are defined by all unique combinations of the stratification variables that appear in the data.
#' The \code{WHERE} column is used to define a subset of subjects to include in the analysis,
#' and has the form \code{variable operator value}, where \code{operator} can be one of the
#' following \code{>, <, >=, <= !=, =}.
#' An example \code{WHERE} condition is \code{age > 50}, which will include all subjects older
#' than 50 in the analysis. Multiple \code{WHERE} conditions must be separated by a \code{&}. 
#' For example, \code{age > 50 & bmi >= 22} will include the subjects older than 50 AND with
#' bmi >= 22. Values in the \code{MODELSPEC} column must match with the \code{MODELSPEC} column
#' in the \bold{ModelOptions} sheet. \cr

#' \bold{ModelOptions} \cr
#' A table where each row specifies an option and has columns \code{MODELSPEC}, \code{FUNCTION},
#' \code{OPTION}, and \code{VALUE}. For an example sheet and additional information about this
#' sheet, see the Excel file \code{/extdata/cometsInput.xlsx}.
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#'
#' @export

readCOMETSinput <- function(file) {

  csvfilePath <- file
  stopifnot(is.character(csvfilePath))
  if (!file.exists(csvfilePath)) {
    stop("CSV input file does not exist")
  }

  #metabolite meta data
  dta.metab <-
    suppressWarnings(fixData(readxl::read_excel(csvfilePath, "Metabolites")))
  print("Metabolites sheet is read in")
  #subject metabolite data
  # The following call does not always work with skip=1, col_names=F
  #dta.smetab <-
  #  suppressWarnings(fixData(readxl::read_excel(csvfilePath, "SubjectMetabolites",skip=1,col_names=F)))

  # New code
  dta.smetab <- suppressWarnings(fixData(readxl::read_excel(csvfilePath, "SubjectMetabolites")))
  dict_metabnames      <- tolower(colnames(dta.smetab))
  colnames(dta.smetab) <- paste("...", 1:ncol(dta.smetab), sep="")
  print("SubjectMetabolites sheet is read in")

  # Read in the first row (column names) of subject metabolite data (this is now the dictionary of metabolite names)
  #dict_metabnames <- 
  #	tolower(fixData(readxl::read_excel(csvfilePath, "SubjectMetabolites",col_names=F,n_max=1)))
  names(dict_metabnames) <- colnames(dta.smetab)

  #subject data
  dta.sdata <-
    suppressWarnings(fixData(readxl::read_excel(csvfilePath, "SubjectData")))
  print("SubjectData sheet is read in")
  #variable mapping
  dta.vmap <-
    suppressWarnings(fixData(readxl::read_excel(csvfilePath, "VarMap")))
  print("VarMap sheet is read in")
  
  # Make sure columns are in lower case
  dta.vmap$varreference   <- checkVariableNames(dta.vmap$varreference, "VarMap sheet, VARREFERENCE column") 
  dta.vmap$cohortvariable <- checkVariableNames(dta.vmap$cohortvariable, "VarMap sheet, COHORTVARIABLE column")
  dta.vmap$vartype        <- checkVariableNames(dta.vmap$vartype, "VarMap sheet, VARYTYPE column")

  #batch model specifications
  dta.models <-
    suppressWarnings(fixData(readxl::read_excel(csvfilePath, "Models"),compbl=TRUE))
  print("Models sheet is read in")

  # Make sure columns are in lower case. The adjustment and stratification columns
  #  can have missing values.
  dta.models$outcomes       <- checkVariableNames(dta.models$outcomes, "Models sheet, OUTCOMES column") 
  dta.models$exposure       <- checkVariableNames(dta.models$exposure, "Models sheet, EXPOSURE column") 
  dta.models$adjustment     <- checkVariableNames(dta.models$adjustment, "Models sheet, ADJUSTMENT column", convertMissTo=NA, stopOnMissError=0) 
  dta.models$stratification <- checkVariableNames(dta.models$stratification, "Models sheet, STRATIFICATION column", convertMissTo=NA, stopOnMissError=0) 

  # We need a different function for the WHERE column
  dta.models$where <- normalizeWhere(dta.models$where) 

  # Read in the options if models sheet has a MODELSPEC column
  dta.options <- NULL
  modnm       <- getModelOptionsIdCol()
  if (modnm %in% colnames(dta.models)) {
    # Check that this column does not contain reserved words
    checkModelspecCol(dta.models[, modnm, drop=TRUE])
    
    opnm        <- getOptionsSheetName()
    dta.options <- suppressWarnings(fixData(readxl::read_excel(csvfilePath, opnm),compbl=TRUE))

    # Check the table
    dta.options <- checkOptionsSheet(dta.options)
    print(paste0(opnm, " sheet is read in"))
  }  

  # Go through the varmap VARTYPE column and convert categorical entries into factors
  myfactors <- dta.vmap$cohortvariable[which(dta.vmap$vartype=="categorical" & dta.vmap$varreference!="metabolite_id")]
  print(paste("There are",length(myfactors),"categorical variables"))
  for (i in myfactors) {
	dta.sdata[,i] <- factor(dta.sdata[,i])
  }

  # Rename the variables in subject data to prevent errors in checkIntegrity
  oldnames <- names(dta.sdata)
  newnames <- renameSubjDataVars(oldnames, dta.vmap)
  names(dta.sdata) <- newnames

  # Check file integrity:
  ckintegrity = checkIntegrity(
    dta.metab = dta.metab,
    dta.smetab = dta.smetab,
    dta.sdata = dta.sdata,
    dta.vmap = dta.vmap,
    dta.models = dta.models,
    dict_metabnames = dict_metabnames
  )

  integritymessage = ckintegrity$outmessage
  dta.metab = ckintegrity$dta.metab
  dta.smetab = ckintegrity$dta.smetab
  dta.sdata = ckintegrity$dta.sdata

  # If an error was found during integrity check (e.g. not all metabolites or subjects
  # in the SubjectMetabolite sheet are annotated in the respective metadatasheets Subjects
  # and Metabolites), then return only integrity check
  if (length(grep("Error", ckintegrity$outmessage)) > 0) {
    dtalist = list(integritymessage = integritymessage, mods = dta.models)
  }
  else {
    dta <- dplyr::inner_join(dta.sdata, dta.smetab)

    idvar0 <- base::tolower(dta.vmap[['cohortvariable']][dta.vmap[['varreference']] == 'id'])
    idvar  <- getVarRef_subjectId()  
    metabvar <-
      base::tolower(dta.vmap[['cohortvariable']][dta.vmap[['varreference']] == "metabolite_id"])

    # run through all vmap specifications to create variables
    dtalist <- list(
      subjdata = dta,
      # metabolite abundances
      allMetabolites = names(dta.smetab)[-1],
      # metabolite names
      allSubjectMetaData = names(dta.sdata)[-1],
      # subject meta data
      allSubjects = dta.sdata[, idvar],
      # subject names
      subjId = idvar, subjId0 = idvar0,
      # id used for subject names
      metabId = metabvar,
      # id used for metabolite names
      metab = dta.metab,
      # metabolite meta data
      dict_metabnames = dict_metabnames,
      mods = dta.models,
      # model specification information
      integritymessage = integritymessage,
      # message for integrity check
      vmap = dplyr::filter(dta.vmap, !is.na(dta.vmap[["cohortvariable"]]) &
                             dta.vmap[["varreference"]] != "metabolite_id") # variable mapping
    )

    # Harmonize metabolites
    dtalist <- Harmonize(dtalist)

    # keep only columns with non-missing values
    mymets = dtalist$metab[[dtalist$metabId]] # get complete list of metabolites

    # convert the names to indexes from dictionary:
    mymets <- as.character(lapply(mymets,function(x) names(dict_metabnames)[which(dict_metabnames==x)]))

    # Make sure the metabololites are numeric
    dtalist$subjdata <- convertVarsToNumeric(dtalist$subjdata, mymets)

    # check to see which columns have non-missing values
    havedata <-
      base::apply(dtalist$subjdata, 2, function(x)
        all(is.na(x)))
    mymets = mymets[mymets %in% names(havedata[havedata == FALSE])]
    if (length(mymets) > 0) {
      # Determine whether data is already transformed:
      tst <- min(dtalist$subjdata[, c(mymets)], na.rm = T)
      if (tst < 0) {
        transformation = TRUE
      }
      else {
        transformation = FALSE
      }

      # ?? et 10/4/16 in the future, we might need to check whether each metabolite is transformed for now any negative value applies to all

      # add summary statistics
      log2metvar = as.numeric(base::lapply(mymets, function(x) {
        temp = which(colnames(dtalist$subjdata) == x)
        if (length(temp) == 0) {
          return(NA)
        } else {
          if (transformation == TRUE) {
            return(stats::var(dtalist$subjdata[[x]], na.rm = TRUE))
          } else {
            return(stats::var(log2(dtalist$subjdata[[x]]), na.rm = TRUE))
          }
        }
      }))

      # number of observations at minimum value
      num.min = as.numeric(lapply(mymets, function(x) {
        temp = which(colnames(dtalist$subjdata) == x)
        if (length(temp) == 0) {
          return(NA)
        } else
          return(length(which(
            dtalist$subjdata[[x]] == min(dtalist$subjdata[[x]], na.rm = TRUE)
          )))
      }))
      dtalist$transformation = transformation
      dtalist$metab$var[dtalist$metab[, dtalist$metabId] %in% dict_metabnames[mymets]] =
        log2metvar
      dtalist$metab$num.min[dtalist$metab[, dtalist$metabId] %in% dict_metabnames[mymets]] =
        num.min
    } else {
      # if all subject metabolite data is missing
      dtalist$transformation = NA
      dtalist$metab$var = NA
      dtalist$metab$num.min = NA
    }

    # vector of variables to display in table
    dtalist$dispvars <-
      c("outcome",
        "exposure",
        "adjvars",
        "corr",
        "pvalue",
        "n",
        "stratavar",
        "strata")

    # Return a map between the old and new variable names for all variables
    #varMap         <- c(oldnames, dict_metabnames)
    #names(varMap)  <- c(newnames, names(dict_metabnames))
    #dtalist$varMap <- varMap

    dtalist[[getMetabDataOpsName()]] <- dta.options

    print(integritymessage)
    return(dtalist)
  }
}


#' This function provides a description of the input data (for categorical data, the number of samples of each type; for continous data, the median and other statistics for each variable)
#' @param readData list from readComets
#'
#' @return a list with 2 data frames, continuous and categorical summaries. Type of variable is defined in varmap
#'
#' @examples
#' \dontrun{
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' allmodeloutput <- runAllModels(exmetabdata)
#' # Get descriptive data
#' descdata <-runDescrip(exmetabdata)
#' OutputXLSResults(filename="corr",datal=descdata,cohort="DPP")
#' }
#' @export
runDescrip<- function(readData){
  sumcat<-variable<-value<-cohort<-NULL
  # check if vartype is in vmap to see whether anyvars are categorical
  if (length(which(grepl("vartype",names(readData$vmap))))>0){

        catvars<-names(readData$subjdata)[which(sapply(readData$subjdata, is.factor)==TRUE)]

        msdata<-readData$subjdata %>%
                select_(catvars)
        msdata <- suppressWarnings(data.table::melt(readData$subjdata,measure.vars=catvars))
        sumcat <- msdata %>%
        group_by(variable, value) %>%
        summarise (n = n()) %>%
        mutate(proportion = n / sum(n))
  }

  sumcnt <-as.data.frame(psych::describe(readData$subjdata,
	quant = c(.05,.25,.5,.75,.95)))
  sumcnt$vars<-rownames(sumcnt)

  # Retrieve the original names for metabolites
  newvars <- data.frame(vars=as.character(names(readData$dict_metabname)),
		new=as.character(readData$dict_metabname),stringsAsFactors = FALSE)
  new     <- newvars$new
  vars    <- newvars$vars
  sumcnt  <- suppressWarnings(dplyr::left_join(sumcnt, newvars,by=unique(c("vars","vars")))) %>% 
	dplyr::mutate(vars = ifelse(!is.na(new), new, vars)) %>%
	dplyr::select(-new)
  colnames(newvars)[which(colnames(newvars)=="vars")]="variable"
  sumcat <- suppressWarnings(dplyr::left_join(as.data.frame(sumcat), newvars,by=unique(c("variable","variable")))) %>%
        dplyr::mutate(variable = ifelse(!is.na(new), new, variable)) %>%
        dplyr::select(-new)

  return(list(sum_categorical=sumcat,sum_continuous=sumcnt))
}

checkOptionsSheet <- function(x) {

  sheet <- getOptionsSheetName()
  if (!length(x)) stop(paste0("ERROR: ", sheet, " is empty"))
  cols <- colnames(x)

  # Required columns
  req  <- c(getModelOptionsIdCol(), getModelFunctionCol(),
            getOptionNameCol(), getOptionValueCol())
  for (i in 1:length(req)) {
    col <- req[i]
    if (!(col %in% cols)) {
      stop(paste0("ERROR: ", sheet, " sheet does not contain the ", col, " column"))
    }

    # Replace missing values with empty string
    tmp <- is.na(x[, col, drop=TRUE])
    if (any(tmp)) x[tmp, col] <- ""

    # Remove quotation marks
    if (i > 1) x[, col] <- setupOpStr(x[, col])
  }

  x

} # END: checkOptionsSheet

renameSubjDataVars <- function(oldnames, vmap) {

  # list of variables named differently for cohort
  tmp <- !is.na(vmap[["cohortvariable"]]) & vmap[["varreference"]] != getVarRef_metabId()
  tmp[is.na(tmp)] <- FALSE
  tst <- dplyr::filter(vmap, tmp)
                  
  # changes names string using mapvalues
  newnames <- plyr::mapvalues(
    oldnames,
    from = c(base::tolower(tst$cohortvariable)),
    to = c(base::tolower(tst$varreference))
  )

  newnames

} # END: renameSubjDataVars

checkModelspecCol <- function(vec) {

  reserved <- getGlobalOptionName()  
  tmp      <- trimws(vec) %in% reserved
  if (any(tmp)) {
    msg <- paste0("ERROR: the ", getModelOptionsIdCol(), 
                  " in the Models sheet contains the reseved word ", reserved)
    stop(msg)
  }
  
} # END: checkModelspecCol

