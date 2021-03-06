#---------------------------------------------------------
# Get Model data and model components --------------------
#---------------------------------------------------------
#' Prepares data for the models to be run as specified in the input.  Can be run in interactive or batch mode.  Each model is checked for validity (correlation between predictors, zero variance, etc.).
#'
#' @param readData list from readComets
#' @param modelspec How model is specified (Interactive or Batch)
#' @param modlabel  if batch, chosen model specified by batch mode. If interactive model label.
#' @param rowvars   if Interactive, a vector of outcome variables (usually metabolites rendered in rows, default is All metabolites)
#' @param colvars   if Interactive, a vector of exposure variables (usually covariates rendered in columns)
#' @param adjvars   If Interactive, a vector adjustment covariates
#' @param strvars   If Interactive, stratification covariates
#' @param where users can specify which subjects to perform the analysis by specifying this parameter. 'where' expects a vector of strings with a variable name, a comparison operator (e.g. "<", ">", "="), and a value.  For example, "where = c("age>50","bmi > 22").  Note that rules must be separate by a comma.
#'
#' @return a list comprising:
#' 1: subset data: gdta
#' 2: column variables: ccovs
#' 3: row variables: rcovs
#' 4: adjustment variables: acovs
#' 5: stratification variable: scovs
#' 6: model specification: modspec
#' 7: model label: modlab
#' 8: whether all metabolites vs all metabolites is run: allvsall
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modlabel="1 Gender adjusted")
#'
#' @export

getModelData <-  function(readData,
                          modelspec = "Batch",
                          modlabel  = "",
                          rowvars   = "All metabolites",
                          colvars   = "",
                          adjvars   = NULL,
                          strvars   = NULL,
			  where     = NULL) {
  if (is.na(match(modelspec, c("Interactive", "Batch")))) {
    stop("modelspec is not an allowable value.  Use 'Interactive' or 'Batch'")
  }

allvsall=F

# All original metabolite names
  allmetabs   <- readData$dict_metabnames
  tmp         <- !(allmetabs %in% readData$subjId)
  allmetabs   <- allmetabs[tmp]
  allmetabStr <- getAllMetabsName()
  acovs       <- NULL
  scovs       <- NULL

# figure out the model specification based on type (Interactive or Batch)
if (modelspec == "Interactive") {
  if(any(colvars=="")) {stop("Please make sure that you have identified one or more exposure variables (parameter colvars)")}

  # list of variables named differently for cohort
  tst <-
    dplyr::filter(readData$vmap,
                  !is.na(readData$vmap[["cohortvariable"]]) &
                    readData$vmap[["varreference"]] != "metabolite_id")

  # changes names string using mapvalues
  newnames <- plyr::mapvalues(
    names(readData$subjdata),
    from = c(base::tolower(tst$cohortvariable)),
    to = c(base::tolower(tst$varreference))
  )

  # apply changes names to data frame
  names(readData$subjdata) <- newnames

#  if(!is.null(rowvars) && rowvars != "All metabolite") {
#	rowvars <- as.character(names(exmetabdata$dict_metabnames)[as.numeric(lapply(rowvars,function(x) 
#		which(readData$dict_metabnames==x)))])
#  }
  # Check that all variables that are input by user exist in the renamed data
  allvars <- c(setdiff(c(rowvars,colvars,adjvars,strvars),"All metabolites"))
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
    #rcovs <- rowvars
    #rcovs <- unlist(strsplit(rowvars, " "))
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
    # ccovs <- colvars
    #ccovs <- unlist(strsplit(colvars, " "))
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

  # Assign allvsall variable
  if(("All metabolites" %in% colvars) && ("All metabolites" %in% rowvars)) {
	allvsall=TRUE
  } else {
	allvsall=FALSE
  }

  # Throw error if an ajdusted covariate is also an exposure and there is only 1 exposure
  if (length(intersect(adjvars, ccovs)) > 0 &&
      length(ccovs) == 1) {
    stop(
      "ERROR: one of the adjusted covariates is also an exposure and there is only one exposure!!
      Please make sure adjusted covariates are not exposures."
    )
  }

  # Throw a warning if an ajdusted covariate is also an exposure and there is more than 1 exposure
  if (length(intersect(adjvars, ccovs)) > 0 &&
      length(ccovs) > length(adjvars)) {
    vartoremove = intersect(adjvars, ccovs)
    print(
      paste0(
        "WARNING: one of the adjusted covariates is also an exposure!!\n",
        "The variable ",
        vartoremove,
        " will be dropped from the list of exposures"
      )
    )
    ccovs = setdiff(ccovs, adjvars)
  }

  # Throw error if an adjusted covariate is also an outcome and there is only 1 outcome
  if (length(intersect(adjvars, rcovs)) > 0 &&
      length(rcovs) == 1) {
    stop(
      "ERROR: one of the adjusted covariates is also an outcome and there is only 1 outcome!!
      Please make sure adjusted covariates are not outcomes."
    )
  }

  # Throw a warning if an adjusted covariate is also an outcome amd there is more than 1 outcome
  if (length(intersect(adjvars, rcovs)) > 0 &&
      length(rcovs) > length(adjvars)) {
    vartoremove = intersect(adjvars, rcovs)
    print(
      paste0(
        "WARNING: one of the adjusted covariates is also an outcome!!\n",
        "The variable(s) ",
        vartoremove,
        " will be dropped from the list of outcomes"
      )
    )
    rcovs = setdiff(rcovs, adjvars)
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
    stop("The model provided does not exist in the input Excell file")
  }


  # rename variables to cohortvariable definitions -----------------------------

  # list of variables named differently for cohort
  tst <-
    dplyr::filter(readData$vmap,
                  !is.na(readData$vmap[["cohortvariable"]]) &
                    readData$vmap[["varreference"]] != "metabolite_id")

  # changes names string using mapvalues
  newnames <- plyr::mapvalues(
    names(readData$subjdata),
    from = c(base::tolower(tst$cohortvariable)),
    to = c(base::tolower(tst$varreference))
  )

  # apply changes names to data frame
  names(readData$subjdata) <- newnames

  # assign outcome vars -------------------------
  #if (length(mods) > 0 & mods$outcomes == "All metabolites") {
  #  rcovs <- c(readData[[2]])
  #} else
  #  rcovs <- as.vector(strsplit(mods$outcomes, " ")[[1]])
  rcovs <- getCovNames_allMetabs(mods$outcomes, allmetabs, readData$dict_metabnames)

  # assign exposure vars -------------------------
  #if (length(mods) > 0 & mods$exposure == "All metabolites") {
  #  ccovs <- c(readData[[2]])
  #} else
  #  ccovs <- as.vector(strsplit(mods$exposure, " ")[[1]])
  ccovs <- getCovNames_allMetabs(mods$exposure, allmetabs, readData$dict_metabnames)

  # assign adjustment vars -------------------------
  #if (!is.na(mods$adjustment)) {
  #  acovs <- as.vector(strsplit(mods$adjustment, " ")[[1]])
  #} else
  #  acovs <- NULL
  if (!is.na(mods$adjustment)) {
      acovs <- as.vector(strsplit(mods$adjustment, " ")[[1]])
      acovs <- runModel.getNewVarName(unique(trimws(acovs)), readData$dict_metabnames)
  } 

  # assign stratification vars vars -------------------------
  #if (!is.na(mods$stratification)) {
  #  scovs <- as.vector(strsplit(mods$stratification, " ")[[1]])
  #} else
  #  scovs <- NULL
  if (!is.na(mods$stratification)) {
      scovs <- as.vector(strsplit(mods$stratification, " ")[[1]])
      scovs <- runModel.getNewVarName(unique(trimws(scovs)), readData$dict_metabnames)
  } 

  # Check that all variables that are input by user exist in the renamed data
  allvars <- c(setdiff(c(rcovs,ccovs,acovs,scovs),"All metabolites"))
  allcols <- c(colnames(readData$subjdata), readData$dict_metabnames)
  if(any(is.na(match(allvars, allcols)))) {
        stop("Check that user-input variables exist (should match VARREFERENCE column in VarMap Sheet)")
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

if(!is.null(where)) {
      numallsamps <- nrow(readData$subjdata)
      readData <- filterCOMETSinput(readData,where=where)
      print(paste0("Filtering subjects according to the rule(s)",where,". ",nrow(readData$subjdata)," of ", numallsamps,"are retained"))
}

gdta <- dplyr::select(readData$subjdata, dplyr::one_of(covlist))

if(nrow(gdta) == 0) {
        warning("The number of samples for this model is zero so the model will not be run")
}

# Create list for analyses  -------------------------------
# list for subset data
# 1: subset data: gdta
# 2: column variables: ccovs
# 3: row variables: rcovs
# 4: adjustment variables: acovs
#    if (dobug)
#      prdebug("End of getdata:", dim(gdta))
list(
  gdta = gdta,
  ccovs = ccovs,
  rcovs = rcovs,
  acovs = acovs,
  scovs = scovs,
  dict_metabnames = readData$dict_metabnames,
  modelspec = modelspec,
  modlabel = modlabel,
  where = where,
  allvsall = allvsall
)
}
