#' Read in Excel file that contains metabolite data, covariate data, 
#' models, and model options.
#'
#' @param file path of Excel file to be read in. This file must contain sheets
#' with names \bold{Metabolites}, \bold{SubjectMetabolites}, \bold{SubjectData}, \bold{VarMap}, 
#' and optionally \bold{Models}, \bold{ModelOptions} (see details).
#' @return a list comprising of data and information needed for \code{\link{getModelData}}.
#'
#' @details Additional information regarding each sheet in the input Excel file is given below. 
#'
#' \bold{Metabolites} \cr
#' A table with the columns \code{METABID}, \code{METABOLITE_NAME}, and possibly other columns \cr
#'  of information about the metabolites. The \code{METABID} column is used
#'  for harmonizing the metabolite names across different cohorts when meta-analyses are performed. \cr

#' \bold{SubjectMetabolites} \cr
#' A table with the subject ids in the first column and metabolites as the other columns. \cr

#' \bold{SubjectData} \cr
#' A table with the subject ids in the first column and covariates as the other columns. \cr

#' \bold{VarMap} \cr
#' A table with at least the required columns \code{VARREFERENCE}, \code{COHORTVARIABLE}, 
#' \code{VARTYPE}, and \code{VARDEFINITION}. The \code{COHORTVARIABLE} column must contain names that match the
#' column names in the \bold{SubjectData} table. These names will be renamed to their
#' corresponding name in the \code{VARREFERENCE} column. The \code{VARTYPE} column 
#' should have values \code{continuous} or \code{categorical} for each row. \cr

#' \bold{Models} \cr
#' A table where each row represents a model to be run, and with columns \code{MODEL}, 
#' \code{OUTCOMES}, \code{EXPOSURE}, \code{ADJUSTMENT},
#'  \code{STRATIFICATION}, \code{WHERE}, and optionally \code{MODELSPEC}. All variable names in this
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
#' in the \bold{ModelOptions} sheet. 
#' This sheet is not required when running in interactive mode, but is required when
#' running in batch mode. \cr

#' \bold{ModelOptions} \cr
#' A table where each row specifies an option and has columns \code{MODELSPEC}, \code{FUNCTION},
#' \code{OPTION}, and \code{VALUE}. For an example sheet and additional information about this
#' sheet, see the Excel file \code{/extdata/cometsInput.xlsx}.
#' This sheet is optional, but is required when the \bold{Models} sheet contains the 
#' column \code{MODELSPEC}.
#'
#' @examples
#' dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#'
#' @export

readCOMETSinput <- function(file) {

  if (!isString(file)) stop("ERROR: file must be a character string giving the complete path to the Excel file.")
  if (!file.exists(file)) stop(paste0("ERROR: input Excel file ", file, " does not exist."))

  # Get the sheet names in the file
  sheets <- try(readxl::excel_sheets(file))
  if ("try-error" %in% class(sheets)) stop("ERROR: check that the input file is an Excel file")

  # Check for the required sheet names
  nms    <- getReqSheetNames()
  tmp    <- toupper(nms) %in% toupper(sheets)
  if (!any(tmp)) {
    msg <- infile.collapseVec(nms)
    msg <- paste0("The input Excel file is missing all required sheets: ", msg)  
    stop(msg)
  }

  # Read varMap sheet first
  dta.vmap <- readExcelSheet(file, getVarMapSheetName(), sheets) 
  dta.vmap <- checkVarMapCols(dta.vmap)

  #metabolite meta data
  dta.metab <- readExcelSheet(file, getMetabSheetName(), sheets)
  dta.metab <- infile.normIdCol(dta.metab, dta.vmap, which="metab")

  #subject metabolite data
  dta.smetab <- readExcelSheet(file, getSubMetabSheetName(), sheets) 
  dta.smetab <- infile.normIdCol(dta.smetab, dta.vmap, which="submetab")
  if (length(dta.smetab)) {
    dict_metabnames        <- tolower(colnames(dta.smetab))
    colnames(dta.smetab)   <- paste("...", 1:ncol(dta.smetab), sep="")
    names(dict_metabnames) <- colnames(dta.smetab)
  }

  #subject data
  dta.sdata <- readExcelSheet(file, getSubDataSheetName(), sheets)  
  dta.sdata <- infile.normIdCol(dta.sdata, dta.vmap, which="subdata")
  
  #batch model specifications. Models sheet is now optional (for interactive use)
  dta.models  <- NULL
  dta.options <- NULL
  modelsSheet <- getModelsSheetName()
  if (toupper(modelsSheet) %in% toupper(sheets)) {
    dta.models <- readExcelSheet(file, modelsSheet, sheets, optional=1) 
    dta.models <- checkModelsCols(dta.models)
    
    # Read in the options if models sheet has a MODELSPEC column
    modnm <- getModelOptionsIdCol()
    if (modnm %in% colnames(dta.models)) {
      opnm        <- getOptionsSheetName()
      dta.options <- readExcelSheet(file, getOptionsSheetName(), sheets, optional=1) 
      if (length(dta.options)) dta.options <- checkOptionsSheet0(dta.options)
    } else {
      msg <- paste0("NOTE: the column ", toupper(modnm), " was not found in the ", modelsSheet, " sheet. ",
                    "Assuming older version of Excel file.\n")
      cat(msg)
    }
  } else {
    msg <- paste0("NOTE: the ", modelsSheet, " sheet was not found in input excel file.",
                  " Assuming COMETS will be run in interactive mode.\n")
    cat(msg)
  } 

  # Go through the varmap VARTYPE column and convert categorical entries into factors
  dta.sdata <- infile.catVarsToFactors(dta.sdata, dta.vmap)

  # Rename the variables in subject data to prevent errors in checkIntegrity
  dta.sdata <- renameSubjDataVars(dta.sdata, dta.vmap)

  # Check file integrity:
  ckintegrity = checkIntegrity(
    dta.metab = dta.metab,
    dta.smetab = dta.smetab,
    dta.sdata = dta.sdata,
    dta.vmap = dta.vmap,
    dta.models = dta.models,
    dict_metabnames = dict_metabnames,
    dta.op = dta.options
  )

  dta.metab = ckintegrity$dta.metab
  dta.smetab = ckintegrity$dta.smetab
  dta.sdata = ckintegrity$dta.sdata
  integritymessage <- ckintegrity$integritymessage
  
  idvar <- tolower(getVarRef_subjectId())  
  dta   <- dplyr::inner_join(dta.sdata, dta.smetab, by=idvar)
  rm(ckintegrity)
  gc()

  cohortv  <- tolower(getVarMapCohortVarCol())
  varrefv  <- tolower(getVarMapVarRefCol())  
  metidv   <- tolower(getVarRef_metabId())
  idvar0   <- base::tolower(dta.vmap[[cohortv]][dta.vmap[[varrefv]] == idvar])  
  metabvar <- base::tolower(dta.vmap[[cohortv]][dta.vmap[[varrefv]] == metidv])

  # Get names
  nms      <- names(dta.smetab)
  metabs   <- nms[nms != idvar]
  nms      <- names(dta.sdata)
  submeta  <- nms[nms != idvar]

  # run through all vmap specifications to create variables
  dtalist <- list(
      subjdata = dta,
      # metabolite abundances
      allMetabolites = metabs,
      # metabolite names
      allSubjectMetaData = submeta,
      # subject meta data
      allSubjects = dta.sdata[, idvar, drop=TRUE],
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
      vmap = dplyr::filter(dta.vmap, !is.na(dta.vmap[[cohortv]]) &
                             dta.vmap[[varrefv]] != metidv) # variable mapping
  )

  rm(dta, dta.sdata, dta.smetab, metabs, submeta)
  gc()

  # Harmonize metabolites
  dtalist <- Harmonize(dtalist)

  # keep only columns with non-missing values
  mymets = dtalist$metab[[dtalist$metabId]] # get complete list of metabolites

  # convert the names to indexes from dictionary:
  mymets <- as.character(lapply(tolower(mymets),function(x) names(dict_metabnames)[which(dict_metabnames==x)]))

  # check to see which columns have non-missing values
  havedata <- base::apply(dtalist$subjdata, 2, function(x) all(is.na(x)))

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
      dtalist$metab$var[dtalist$metab[, dtalist$metabId] %in% dict_metabnames[mymets]] = log2metvar
      dtalist$metab$num.min[dtalist$metab[, dtalist$metabId] %in% dict_metabnames[mymets]] = num.min
  } else {
    # if all subject metabolite data is missing
    dtalist$transformation = NA
    dtalist$metab$var = NA
    dtalist$metab$num.min = NA
  }

  # vector of variables to display in table
  dtalist$dispvars <- c("outcome", "exposure", "adjvars", "corr", "pvalue",
                        "n", "stratavar", "strata")

  dtalist[[getMetabDataOpsName()]] <- dta.options

  # Test the models in the Models sheet 
  err <- infile.checkAllModels(dtalist)
  if (err) {
    # Display warning, user could be running in interactive mode 
    msg <- paste0("ERRORS were produced for some models in the ", getModelsSheetName(),
                  " sheet. See messages above. \n")
    cat(msg)
    warning(msg)
  } 

  return(dtalist)

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

checkOptionsSheet0 <- function(x) {

  if (!length(x)) return(x)

  # Required columns
  req  <- getReqModOpSheetCols()
  for (i in 1:length(req)) {
    col <- req[i]
    
    # Replace missing values with empty string
    tmp <- is.na(x[, col, drop=TRUE])
    if (any(tmp)) x[tmp, col] <- ""

    # Remove quotation marks
    if (i > 1) x[, col] <- setupOpStr(x[, col])
  }

  x

} # END: checkOptionsSheet0
renameSubjDataVars <- function(x, vmap) {

  if (!length(x)) return(x)
  if (!length(vmap)) return(x)
  
  cohortv  <- tolower(getVarMapCohortVarCol())
  varrefv  <- tolower(getVarMapVarRefCol())  
  metidv   <- tolower(getVarRef_metabId())
  cx       <- tolower(colnames(vmap))
  if (!(cohortv %in% cx)) return(x)
  if (!(varrefv %in% cx)) return(x)

  # list of variables named differently for cohort
  tmp <- !is.na(vmap[[cohortv]]) & (vmap[[varrefv]] != metidv)
  tmp[is.na(tmp)] <- FALSE
  if (any(tmp)) {
    old  <- tolower(vmap[tmp, cohortv, drop=TRUE])
    new  <- tolower(vmap[tmp, varrefv, drop=TRUE])
    cx   <- tolower(colnames(x))
    ii   <- match(cx, old)
    tmp  <- !is.na(ii)
    ii   <- ii[tmp]
    if (length(ii)) {
      cx[tmp]     <- new[ii]
      colnames(x) <- cx 
    }
  }

  x

} # END: renameSubjDataVars



