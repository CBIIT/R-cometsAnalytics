#---------------------------------------------------------
# Get Model data and model components --------------------
#---------------------------------------------------------
#' Prepares data for the models to be run as specified in the input.  Can be run in interactive or batch mode.  Each model is checked for validity (correlation between predictors, zero variance, etc.).
#'
#' @param readData List from \code{\link{readCOMETSinput}}
#' @param modelspec How model is specified (Interactive or Batch). The default is Batch
#' @param modlabel  If batch, chosen model specified by batch mode (the MODEL column in
#'                  the Models sheet). If interactive, then the model label.
#' @param rowvars   If Interactive, a vector of outcome variables (see \code{details}), the default is All metabolites)
#' @param colvars   If Interactive, a vector of exposure variables (see \code{details})
#' @param adjvars   If Interactive, a vector adjustment covariates (see \code{details})
#' @param strvars   If Interactive, stratification covariates (see \code{details})
#' @param wgtvar    If Interactive, a variable of weights (see \code{details})
#' @param offvar    If Interactive, an offset variable (see \code{details})
#' @param where users can specify which subjects to perform the analysis on by specifying this parameter. 
#'        'where' expects a vector of strings with a variable name, 
#'        a comparison operator (e.g. "<", ">", "="), and a value.  
#'        For example, \code{where = c("age>50","bmi > 22")} uses all subjects
#'        with age > 50 AND bmi > 22.  
#'     Note that when running in Batch mode, rules in the \code{WHERE} column
#'     of the \code{Models} sheet must be separated by a comma.
#'
#' @details All metabolite variables specified should be listed in the \code{metabolite_name}
#'   column of the \code{Metabolites} sheet of the Excel file. All non-metabolite
#'   variables should be listed in the \code{VARREFERENCE} column of the
#'   \code{VarMap} sheet. The \code{wgtvar} and \code{offvar} are only used when the
#'   model function is \code{\link[stats]{glm}} or \code{\link[stats]{lm}}, see the
#'   \code{model} option in \code{\link{options}}.
#'
#' @return a list comprising: \cr
#' 1: subset data: gdta \cr
#' 2: column variables: ccovs \cr
#' 3: row variables: rcovs \cr
#' 4: adjustment variables: acovs \cr
#' 5: stratification variable: scovs \cr
#' 6: model specification: modspec \cr
#' 7: model label: modlab \cr
#' 8: whether all metabolites vs all metabolites is run: allvsall \cr
#' 9: weight variables: wgtcov \cr
#' 10: offset variables: offcov
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,modlabel="1 Gender adjusted")
#'
#' @export

getModelData <-  function(readData,
                          modelspec = "Batch",
                          modlabel  = "",
                          rowvars   = "All metabolites",
                          colvars   = "",
                          adjvars   = NULL,
                          strvars   = NULL,
                          wgtvar    = NULL,
                          offvar    = NULL,
			  where     = NULL) {
  if (is.na(match(modelspec, c("Interactive", "Batch")))) {
    stop("modelspec is not an allowable value.  Use 'Interactive' or 'Batch'")
  }

  allvsall <- FALSE
  rem.obj  <- NULL
  options  <- NULL
  wgtcov   <- NULL
  offcov   <- NULL

# figure out the model specification based on type (Interactive or Batch)
if (modelspec == "Interactive") {
  if(any(colvars=="")) {stop("Please make sure that you have identified one or more exposure variables (parameter colvars)")}

  # Normalize variables so that it is consistent with readCOMETSinput
  rowvars <- checkVariableNames(rowvars, "rowvars", default="All metabolites", only.unique=1)
  colvars <- checkVariableNames(colvars, "colvars", default="", only.unique=1)
  adjvars <- checkVariableNames(adjvars, "adjvars", default=NULL, only.unique=1)
  strvars <- checkVariableNames(strvars, "strvars", default=NULL, only.unique=1)
  wgtvar  <- checkVariableNames(wgtvar,  "wgtvar",  default=NULL, only.unique=1, max.n=1)
  offvar  <- checkVariableNames(offvar,  "offvar",  default=NULL, only.unique=1, max.n=1)

  # Names changed in readCOMETSinput
  # list of variables named differently for cohort
  #tst <-
  #  dplyr::filter(readData$vmap,
  #                !is.na(readData$vmap[["cohortvariable"]]) &
  #                  readData$vmap[["varreference"]] != "metabolite_id")
  # changes names string using mapvalues
  #newnames <- plyr::mapvalues(
  #  names(readData$subjdata),
  #  from = c(base::tolower(tst$cohortvariable)),
  #  to = c(base::tolower(tst$varreference))
  #)
  # apply changes names to data frame
  #names(readData$subjdata) <- newnames

#  if(!is.null(rowvars) && rowvars != "All metabolite") {
#	rowvars <- as.character(names(exmetabdata$dict_metabnames)[as.numeric(lapply(rowvars,function(x) 
#		which(readData$dict_metabnames==x)))])
#  }
  # Check that all variables that are input by user exist in the renamed data
  allvars <- c(setdiff(c(rowvars,colvars,adjvars,strvars,wgtvar,offvar),"All metabolites"))
  subjmetab <- as.character(lapply(colnames(readData$subjdata), function(x) {
        myind <- which(names(readData$dict_metabnames)==x)
        if(length(myind==1)) {x=readData$dict_metabnames[myind]}
        return(x) }))
  
#  if(any(is.na(match(allvars,colnames(readData$subjdata))))) {
  if(any(is.na(match(allvars,subjmetab)))) {
	stop("Check that user-input variables exist (should match VARREFERENCE column in VarMap Sheet)")
  }

  # rename the variables (Assumed to be 'All metabolites' by default)
  if (!is.na(match("All metabolites", rowvars))) {
    print("Analysis will run on 'All metabolites'")
    rcovs <-
      unique(c(rowvars[rowvars != "All metabolites"], c(readData$allMetabolites)))
  }  else {
    rcovs <- as.character(lapply(rowvars, function(x) {
        myind <- which(readData$dict_metabnames==x)
        if(length(myind==1)) {x=names(readData$dict_metabnames)[myind]}
        return(x) }))
  }

  # rename the exposure variables
  if (!is.na(match("All metabolites", colvars))) {
    ccovs <-
      unique(c(colvars[colvars != "All metabolites"], c(readData$allMetabolites)))
  } else {
    ccovs <- as.character(lapply(colvars, function(x) {
        myind <- which(readData$dict_metabnames==x)
        if(length(myind==1)) {x=names(readData$dict_metabnames)[myind]}
        return(x) }))
  }

  # rename the adjustment variables
  if (!is.null(adjvars)) {
    tempacovs <- unlist(strsplit(adjvars, " "))
    acovs <- as.character(lapply(tempacovs, function(x) {
        myind <- which(readData$dict_metabnames==x)
        if(length(myind==1)) {x=names(readData$dict_metabnames)[myind]}
        return(x) }))
  } else {
    acovs <- adjvars
  }

  # rename the stratification variables
  if (!is.null(strvars)) {
    tempscovs <- unlist(strsplit(strvars, " "))
    scovs <- as.character(lapply(tempscovs, function(x) {
        myind <- which(readData$dict_metabnames==x)
        if(length(myind==1)) {x=names(readData$dict_metabnames)[myind]}
        return(x) }))
  } else {
    scovs <- strvars
  }

  # rename the weight variable
  if (!is.null(wgtvar)) {
    wgtcov <- as.character(lapply(wgtvar, function(x) {
        myind <- which(readData$dict_metabnames==x)
        if(length(myind==1)) {x=names(readData$dict_metabnames)[myind]}
        return(x) }))
  } else {
    wgtcov <- wgtvar
  }

  # rename the offset variable
  if (!is.null(offvar)) {
    offcov <- as.character(lapply(offvar, function(x) {
        myind <- which(readData$dict_metabnames==x)
        if(length(myind==1)) {x=names(readData$dict_metabnames)[myind]}
        return(x) }))
  } else {
    offcov <- offvar
  }

  # Assign allvsall variable
  allvsall <- FALSE
  if ( (length(colvars) == 1) && (length(rowvars) == 1) ) {
    if ( (colvars == "All metabolites") && (rowvars=="All metabolites") ) {
      allvsall <- TRUE
    }
  } 

  # Check if adjusted variables are also exposures
  vartoremove <- intersect(adjvars, ccovs)
  if (length(vartoremove)) {
    ccovs <- setdiff(ccovs, adjvars)
    if (!length(ccovs)) {
      stop("ERROR: all of the exposure variables are also adjusted covariates!!
           Please make sure adjusted covariates are not exposures.")
    } else {
      msg <- paste0("Some of the adjusted covariates are also exposure variables!!\n",
                    "The variable(s) ", paste0(vartoremove, collapse=", "),
                    " will be dropped from the list of exposures")
      warning(msg)
    }
    rem.obj <- runModel.addRemVars(rem.obj, vartoremove, "colvars", 
                "are also adjvars", printWarning=0, varMap=NULL) 
  }

  # Check if adjusted variables are also outcomes
  vartoremove <- intersect(adjvars, rcovs)
  if (length(vartoremove)) {
    rcovs <- setdiff(rcovs, adjvars)
    if (!length(rcovs)) {
      stop("ERROR: all of the exposure variables are also outcomes!!
           Please make sure adjusted covariates are not outcomes.")
    } else {
      msg <- paste0("Some of the adjusted covariates are also outcome variables!!\n",
                    "The variable(s) ", paste0(vartoremove, collapse=", "),
                    " will be dropped from the list of outcomes")
      warning(msg)
    }
    rem.obj <- runModel.addRemVars(rem.obj, vartoremove, "rowvars", 
                "are also adjvars", printWarning=0, varMap=NULL) 
  }

  # end if modelspec is "Interactive"
  }
else if (modelspec == "Batch") {
  # here we need to get the covariates defined from the excel sheet
  # step 1. get the chosen model first

  if (modlabel == "") {
    stop("modelspec is set to 'Batch' yet model label (modlabel) is empty.  Please set modlabel.")
  }

  # defining global variable to remove Rcheck warnings
  model = c()

  # integrity check for unmatch modlabel -------------------------------------
  mods <-
    dplyr::filter(as.data.frame(readData[["mods"]]), model == modlabel)
  if (nrow(mods) == 0) {
    stop("The model name input does not exist in the input Excell file. Please check your Models sheet.")
  }

  # rename variables to cohortvariable definitions -----------------------------

  # Names changed in readCOMETSinput
  # list of variables named differently for cohort
  #tst <-
  #  dplyr::filter(readData$vmap,
  #                !is.na(readData$vmap[["cohortvariable"]]) &
  #                  readData$vmap[["varreference"]] != "metabolite_id")
  # changes names string using mapvalues
  #newnames <- plyr::mapvalues(
  #  names(readData$subjdata),
  #  from = c(base::tolower(tst$cohortvariable)),
  #  to = c(base::tolower(tst$varreference))
  #)
  # apply changes names to data frame
  #names(readData$subjdata) <- newnames

  # assign outcome vars -------------------------
  if (length(mods) > 0 & mods$outcomes == "All metabolites") {
    rcovs <- c(readData[[2]])
  } else {
    rcovs <- as.vector(strsplit(mods$outcomes, " ")[[1]])
    rcovs <- runModel.getNewVarName(trimws(rcovs), readData$dict_metabnames)
  }

  # assign exposure vars -------------------------
  if (length(mods) > 0 & mods$exposure == "All metabolites") {
    ccovs <- c(readData[[2]])
  } else {
    ccovs <- as.vector(strsplit(mods$exposure, " ")[[1]])
    ccovs <- runModel.getNewVarName(trimws(ccovs), readData$dict_metabnames)
  }

  # assign adjustment vars -------------------------
  if (!is.na(mods$adjustment)) {
    acovs <- as.vector(strsplit(mods$adjustment, " ")[[1]])
    acovs <- runModel.getNewVarName(trimws(acovs), readData$dict_metabnames)
  } else {
    acovs <- NULL
  }

  # assign stratification vars vars -------------------------
  if (!is.na(mods$stratification)) {
    scovs <- as.vector(strsplit(mods$stratification, " ")[[1]])
    scovs <- runModel.getNewVarName(trimws(scovs), readData$dict_metabnames)
  } else {
    scovs <- NULL
  }

  # Get the options for this model
  options <- getAllOptionsForModel(mods, readData)

  # Get weight and offset variables
  wgtvar <- NULL
  offvar <- NULL
  mop    <- options[[getModelOpsName(), exact=TRUE]]
  if (length(mop)) {
    wgtvar <- mop[["weights", exact=TRUE]]
    if (length(wgtvar)) wgtcov <- runModel.getNewVarName(wgtvar, readData$dict_metabnames)
    offvar <- mop[["offset", exact=TRUE]]
    if (length(offvar)) offcov <- runModel.getNewVarName(offvar, readData$dict_metabnames)
  }  

  # Check that all variables that are input by user exist in the renamed data
  allvars <- c(setdiff(c(rcovs,ccovs,acovs,scovs, wgtcov, offcov),"All metabolites"))
  tmp     <- is.na(match(allvars,colnames(readData$subjdata)))
  if (any(tmp)) {
    str <- paste(allvars[tmp], collapse=", ", sep="")
    msg <- paste("Check that user-input variable(s) ", str,  
             " exist (should match VARREFERENCE column in VarMap Sheet)", sep="")
    stop(msg)
  }

  # assign where filtering -------------------------
  if (!is.na(mods$where)) {
	where <- mods$where
  } else
        where <- NULL


} # end if modelspec == "Batch"

# Keep only needed variables for the data -------------------------------
# build list of variables
covlist <- c(ccovs, rcovs)
if (!is.null(acovs)) {
  covlist <- c(covlist, acovs)
}
if (!is.null(scovs)) {
  covlist <- c(covlist, scovs)
}
covlist <- c(covlist, wgtcov, offcov)
varMap  <- NULL
if (length(wgtvar) || length(offvar)) {
  varMap        <- c(wgtvar, offvar)
  names(varMap) <- c(wgtcov, offcov)
}

if (!is.null(where)) {

  # Update the where rule (variables names have changed)
  where2      <- updateWhereStr(where, readData$dict_metabnames)
  numallsamps <- nrow(readData$subjdata)
  readData    <- try(filterCOMETSinput(readData,where=where2), silent=TRUE)
  if ("try-error" %in% class(readData)) {
    print(readData)
    stop(paste("ERROR applying WHERE: ", where, sep=""))
  }
  msg         <- paste0("Filtering subjects according to the rule(s) ", where, ". ", 
                        nrow(readData$subjdata)," of ", numallsamps," are retained")
  print(msg)
}

gdta <- dplyr::select(readData$subjdata, dplyr::one_of(covlist))

if(nrow(gdta) < 3) {
  stop("Too few samples for this model, so the model will not be run")
}

# Outcomes must be numeric
gdta <- convertVarsToNumeric(gdta, rcovs)

# Create list for analyses  -------------------------------
# list for subset data
# 1: subset data: gdta
# 2: column variables: ccovs
# 3: row variables: rcovs
# 4: adjustment variables: acovs
#    if (dobug)
#      prdebug("End of getdata:", dim(gdta))

  ret <- list(
  gdta            = gdta,
  ccovs           = ccovs,
  rcovs           = rcovs,
  acovs           = acovs,
  scovs           = scovs,
  wgtcov          = wgtcov,
  offcov          = offcov,
  dict_metabnames = readData$dict_metabnames,
  modelspec       = modelspec,
  modlabel        = modlabel,
  where           = where,
  allvsall        = allvsall,
  varMap          = varMap,
  options         = options
  )
  ret[[runModel.getWarningsListName()]] <- rem.obj

  ret
}

getGlobalOptionsFromSheet <- function(opTable) {

  modnm   <- getModelOptionsIdCol()
  tmp     <- opTable[, modnm] %in% getGlobalOptionName()
  opTable <- opTable[tmp, , drop=FALSE]
  if (!nrow(opTable)) return(NULL)

  opNameCol <- getOptionNameCol()
  opValCol  <- getOptionValueCol()

  # Remove rows with empty names and values
  tmp       <- !nchar(opTable[, opNameCol]) & !nchar(opTable[, opValCol])
  opTable   <- opTable[!tmp, , drop=FALSE]
  if (!nrow(opTable)) return(NULL)

  # Check the names and values and put them in a list
  ret <- checkGlobalOpsFromCharVecs(opTable[, opNameCol], opTable[, opValCol])

  ret

} # END: getGlobalOptionsFromSheet

getModelFunFromSheet <- function(opTable) {

  col <- getModelFunctionCol()
  vec <- tolower(unique(opTable[, col]))
  tmp <- nchar(vec) > 0
  vec <- vec[tmp]
  n   <- length(vec)
  if (!n) stop("ERROR: no model function specified")
  if (n > 1) stop("ERROR: more than one model function specified")
  
  valid <- getValidModelNames()
  ret   <- runModel.check.str(vec, valid, col)

  ret

} # END: getModelFunFromSheet

getModelOptionsFromSheet <- function(opTable, modelFunc) {

  opNameCol <- getOptionNameCol()
  opValCol  <- getOptionValueCol()
  opTable   <- unique(opTable[, c(opNameCol, opValCol), drop=FALSE])
  opnames   <- opTable[, opNameCol, drop=TRUE]
  opvalues  <- opTable[, opValCol, drop=TRUE]

  # Remove ones with no name and no value
  tmp <- (nchar(opnames) < 1) & (nchar(opvalues) < 1)
  if (any(tmp)) {
    opnames  <- opnames[!tmp]
    opvalues <- opvalues[!tmp]
  }
  if (!length(opnames)) return(NULL)
 
  # Check for missing names
  if (any(nchar(opnames) < 1)) {
    stop(paste0("ERROR: missing option names in ", getOptionsSheetName(), " sheet"))
  }

  # Check for duplicate names
  tmp <- duplicated(opnames)
  if (any(tmp)) {
    str <- paste(opnames[tmp], collapse=", ", sep="")
    msg <- paste0("ERROR: the options ", str, " appear more than once in the ",
                  getOptionsSheetName(), " sheet")
    stop(msg)
  }

  ret <- convertModelOptions(opnames, opvalues, modelFunc)

  # For options that specify variables, make sure they are lower case
  vop <- runModel.getOptionsThatAreVars() 
  if (length(vop) && length(ret)) {
    for (v in vop) {
      var <- ret[[v, exact=TRUE]]
      if (!is.null(var)) ret[[v]] <- tolower(var) 
    }
  }

  ret

} # END: getModelOptionsFromSheet

getAllOptionsForModel <- function(mods, readData) {

  opTable <- readData[["options", exact=TRUE]]
  if (!length(opTable)) return(NULL)

  modnm   <- getModelOptionsIdCol()
  modelID <- mods[[modnm,  exact=TRUE]]
  if (length(modelID) != 1) stop("INTERNAL CODING ERROR 1 in getOptionsForModel")  

  # Get global options
  op <- getGlobalOptionsFromSheet(opTable)
  if (is.null(op)) op <- list()

  # Subset the sheet for the specific model id
  tmp       <- opTable[, modnm] %in% modelID
  opTable   <- opTable[tmp, , drop=FALSE]
  if (!nrow(opTable)) {
    msg <- paste0("ERROR: ", modnm, " = ", modelID, " not found in ", 
                  getOptionsSheetName(), " sheet")
    stop(msg)
  }

  # Get the model function
  modelFunc <- getModelFunFromSheet(opTable)
  op$model  <- modelFunc

  # Get model options
  mop <- getModelOptionsFromSheet(opTable, modelFunc)
  op[[getModelOpsName()]] <- mop
  
  op

} # END: getAllOptionsForModel

