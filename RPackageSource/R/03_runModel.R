#---------------------------------------------------------
#' Main function for running the models
#'
#' @param modeldata list from function \code{\link{getModelData}}
#' @param metabdata metabolite data list from \code{\link{readCOMETSinput}}
#' @param cohortLabel cohort label (e.g DPP, NCI, Shanghai)
#' @param op list of options when running in \code{Interactive} mode (see \code{\link{options}}).
#' @param out.file NULL or the name of an output file to save the results.
#'          The file extension must be ".xlsx" or ".rda".
#'
#' @return A list of objects with names \code{\link{ModelSummary}},
#'        \code{\link{Effects}}, \code{\link{Errors_Warnings}},
#'        \code{\link{Table1}}, \code{Info}. \cr
#' \bold{Important: check the \code{adjvars.removed} column in the
#'  \code{\link{ModelSummary}} data frame to see if any adjustment variables
#'  were dropped from the model, and use caution interpreting results
#'  when variables are removed. The \code{\link{Errors_Warnings}} object may
#'  also contain additional variables removed.} 
#' Attribute of this list includes ptime for processing time of model run. 
#'
#' @details This function will check for stratification variables, and if
#'          present run within each stratum. The design matrix is
#'          checked for validity (see \code{\link{options}}). 
#'          When running in \code{Batch} mode, the options are obtained from
#'          the \code{Options} sheet in the excel file. \cr
#'
#' @examples
#' dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,exposures="age",modlabel="1 Age",
#' 	outcomes=c("lactose","lactate"))
#' obj <- runModel(modeldata,exmetabdata, cohortLabel="DPP")
#' @export

runModel <- function(modeldata, metabdata, cohortLabel="", op=NULL, out.file=NULL) {

  ptm <- base::proc.time() # start processing time

  # Error checks
  runModel.checkModeldata(modeldata)
  runModel.checkMetabdata(metabdata)
  if (!isString(cohortLabel)) stop(msg_mod_1())
  if (length(out.file)) out.file <- check_out.file(out.file, valid.ext=getOutTypeOpVals())

  # Check if options were obtained in getModelData
  if (modeldata$modelspec == getMode_batch()) {
    modeldata <- runModel.checkOpBatch(modeldata, op)
    op        <- modeldata$options 
  }
  op        <- runModel.checkOptions(op, modeldata)
  op$cohort <- cohortLabel

  ret       <- runModel.start(modeldata, metabdata, op)
  ret       <- runModel.checkRetlist(ret, op) 
  ret       <- runModel.addMetabCols(ret, metabdata, op)
  ret       <- runModel.getTable1(ret, modeldata, op) 
  ret       <- runModel.getInfoDF(ret, modeldata, metabdata, op) 
  ret       <- runModel.dupMetabHarmIds(ret, metabdata, op)

  # Stop the clock
  ptm <- base::proc.time() - ptm
  attr(ret, "ptime") <- paste("Processing time:", round(ptm[3], digits=3), "sec")
  class(ret) <- class_runModel()
       
  if (length(out.file)) try(saveObjectByFileExt(ret, out.file))

  ret

} # END: runModel 

runModel.dupMetabHarmIds <- function(ret, metabdata, op) {

  ids <- metabdata[[dupMetabHarmIds(), exact=TRUE]]
  if (!length(ids)) return(ret)
  idstr     <- paste0(ids, collapse=", ")
  nm        <- runModel.getWarningsListName()
  obj       <- ret[[nm, exact=TRUE]]
  c1        <- runModel.getWarningCol()
  c2        <- runModel.getObjectCol()
  c3        <- runModel.getMessageCol()
  lst       <- list()
  lst[[c1]] <- "WARNING"
  lst[[c2]] <- idstr
  lst[[c3]] <- msg_mod_25()
  obj       <- runmodel.addWarning(obj, lst) 
  ret[[nm]] <- obj

  ret
}

# Function to check for passing in options with running in batch
runModel.checkOpBatch <- function(modeldata, op) {

  DEBUG <- 0
  if (!is.null(op)) {
    if (is.list(op)) DEBUG <- op[["DEBUG", exact=TRUE]]
    tmp   <- list()
    msg   <- msg_mod_2()
    tmp[[runModel.getWarningCol()]] <- "WARNING"
    tmp[[runModel.getObjectCol()]]  <- "op"
    tmp[[runModel.getMessageCol()]] <- msg
    warning(msg)
    nm              <- runModel.getWarningsListName()
    modeldata[[nm]] <- runmodel.addWarning(modeldata[[nm, exact=TRUE]], tmp)
    if (length(DEBUG)) modeldata$options$DEBUG <- DEBUG
  }
   
  modeldata

} # END: runModel.checkOpBatch

runModel.getEmptyErrorWarn <- function() {

  c1  <- runModel.getWarningCol() 
  c2  <- runModel.getObjectCol()  
  c3  <- runModel.getMessageCol() 
  str <- paste("data.frame(", c1, "=character(0), ", c2, "=character(0), ",
                              c3, "=character(0), stringsAsFactors=FALSE)", sep="")
  ret <- eval(parse(text=str))   
  ret

}

# Function to check the return list
runModel.checkRetlist <- function(ret, op) {

  nm1  <- getModelSummaryName() # Sheet name
  nm2  <- getEffectsName()
  nm3  <- runModel.getWarningsListName()

  # Check for errors/warnings data frame
  df  <- ret[[nm3, exact=TRUE]]
  if (is.null(df)) {
    ret[[nm3]] <- runModel.getEmptyErrorWarn()
  }

  # Re-arrange order of sheets if needed
  nms <- names(ret)
  if ((nms[1] == nm3) && (length(nms) > 1)) {
    obj1       <- ret[[nm1, exact=TRUE]]
    obj2       <- ret[[nm2, exact=TRUE]]
    obj3       <- ret[[nm3, exact=TRUE]]
    ret        <- list()
    ret[[nm1]] <- obj1
    ret[[nm2]] <- obj2
    ret[[nm3]] <- obj3  
  }

  # To exponentiate parameter estimates. This must be called before runModel.addCI.
  ret <- runModel.expParms(ret, op)

  # For confidence intervals
  ret <- runModel.addCI(ret, op) 

  ret

} # END: runModel.checkRetlist

runModel.getEffectsCols <- function(obj, cols, ret.data=TRUE) {

  ret <- list()

  # Get the effects table
  eff.df.nm <- getEffectsName()
  x         <- obj[[eff.df.nm, exact=TRUE]]
  if (!length(x)) return(ret)
  if (!is.data.frame(x)) return(ret)
  if (!nrow(x)) return(ret)
  if (!all(cols %in% colnames(x))) return(ret)
  
  for (v in unique(cols)) ret[[v]] <- x[, v, drop=TRUE]
  if (ret.data) ret$data___ <- x
  
  ret
}

runModel.expParms <- function(obj, op) {

  nm <- getExpParmsOpName()
  if (!op[[nm]]) return(obj)
  if (!is.list(obj)) return(obj)

  eff.v <- getEffectsEstName()
  se.v  <- getEffectsEstSeName() 
  tmp   <- runModel.getEffectsCols(obj, c(eff.v, se.v))
  if (!length(tmp)) return(obj)
  eff.vec <- tmp[[eff.v]]
  se.vec  <- tmp[[se.v]]
  x       <- tmp$data___

  # Get new estimates and column names
  eff.v2      <- getEffectsExpEstName()
  se.v2       <- getEffectsExpEstSeName()
  tmp         <- expParms_deltaMethod(tmp[[eff.v]], tmp[[se.v]])
  x[, eff.v2] <- tmp$exp.beta
  x[, se.v2]  <- tmp$exp.beta.se

  obj[[getEffectsName()]] <- x

  obj

} # END: runModel.expParms

runModel.addCI <- function(obj, op) {

  alpha <- op[[getAddCiOpName(), exact=TRUE]]
  if (alpha <= 0) return(obj)
  if (!is.list(obj)) return(obj)

  zval <- qnorm((1-alpha)/2, lower.tail=FALSE)

  # Get the effects table cols
  eff.v <- getEffectsEstName()
  se.v  <- getEffectsEstSeName()
  tmp   <- runModel.getEffectsCols(obj, c(eff.v, se.v))
  if (!length(tmp)) return(obj)
  eff.vec <- tmp[[eff.v]]
  se.vec  <- tmp[[se.v]]
  x       <- tmp$data___
  
  # Confidence limits for estimate  
  lv0      <- getEffectsLowerName()
  uv0      <- getEffectsUpperName()
  x[, lv0] <- eff.vec - zval*se.vec
  x[, uv0] <- eff.vec + zval*se.vec

  # For exponentiated estimates
  if (op[[getExpParmsOpName()]]) {
    lv      <- getEffectsExpLowerName()
    uv      <- getEffectsExpUpperName()
    x[, lv] <- exp(x[, lv0, drop=TRUE])
    x[, uv] <- exp(x[, uv0, drop=TRUE])
  }

  obj[[getEffectsName()]] <- x

  obj
  
} # END: runModel.addCI

# Function to get the original variable name
runModel.varMap <- function(vars, varMap) {

  if (!length(vars)) return(vars)

  new        <- names(varMap)
  rows       <- match(vars, new)
  tmp        <- !is.na(rows)
  if (any(tmp)) {
    rows      <- rows[tmp]
    vars[tmp] <- varMap[rows]
  }

  vars

} # END: runModel.varMap

# Function to get the new variable name
runModel.getNewVarName <- function(vars, varMap) {

  if (!length(vars)) return(vars)

  new        <- varMap
  rows       <- match(vars, new)
  tmp        <- !is.na(rows)
  if (any(tmp)) {
    rows      <- rows[tmp]
    vars[tmp] <- names(varMap)[rows]
  }

  vars

} # END: runModel.getNewVarName

runModel.setReturnDF <- function(x, varMap) {

  if (!length(x)) return(x)

  x     <- as.data.frame(x, stringsAsFactors=FALSE)
  cvars <- c("adj", "exposure", "exposurespec", "outcomespec", "term",
             "adjspec", "adjvars", "adjvars.removed", "adj_uid",
             "cohort", "exposure_uid", "exposurespec", "message", "model",
             "outcome", "outcome_uid", getModelSummaryRunModeName(), "stratavar", "strata",
             "type", "object", getModelSummaryCovStrCol()) 
  cx    <- colnames(x)
  tmp   <- !(cx %in% cvars)
  vars  <- cx[tmp]
  tmp   <- vars %in% colnames(x)
  if (any(tmp)) {
    for (v in vars[tmp]) x[, v] <- as.numeric(x[, v])
  }

  # Remove columns
  rem <- c("adj", "cohort", getModelSummaryFunCol(), 
           getModelSummaryRunModeName(), getModelSummaryModelCol())
  tmp <- rem %in% colnames(x)
  rem <- rem[tmp]
  if (length(rem)) {
    for (v in rem) x[[v]] <- NULL
  }

  # Make sure terms and exposure columns that have metabolites have
  #   the metabolite name instead of ...j
  # The adj vars are taken care of earlier, since they are not
  #   individual variables in general
  vars <- c("exposurespec", "term")
  tmp  <- vars %in% colnames(x)
  vars <- vars[tmp]
  if (length(vars)) {
    for (v in vars) x[, v] <- runModel.varMap(x[, v], varMap)
  }

  x

} # END: runModel.setReturnDF

runModel.combineResObjects <- function(base, new, strat, stratNum) {

  d <- dim(new)
  if (!length(d)) return(base)
  tmp <- colnames(new)
  if (length(strat) != 1) stop("INTERNAL CODING ERROR 1 in runModel.combineResObjects")
  new <- cbind(new, strat, stratNum)
  colnames(new) <- c(tmp, runModel.getStrataColName(), runModel.getStrataNumColName())
  if (!length(base)) {
    ret <- new
  } else {
    if (!identical(colnames(base), colnames(new))) stop("INTERNAL CODING ERROR 2 in runModel.combineResObjects")
    ret <- rbind(base, new)
  }

  ret

} # END: runModel.combineResObjects

runModel.combineResults <- function(base, new, strat, stratNum) {

  wr.nm <- runModel.getWarningsListName()

  if (!length(base)) base <- list()
  warn1 <- base[[wr.nm, exact=TRUE]]
  if (("try-error" %in% class(new)) || isString(new)) {
    msg <- runModel.getErrorMsg(new)
    tmp <- list()
    tmp[[runModel.getWarningCol()]] <- "ERROR"
    tmp[[runModel.getObjectCol()]]  <- ""
    tmp[[runModel.getMessageCol()]] <- msg
    obj2 <- runmodel.addWarning(NULL, tmp)
    base[[wr.nm]] <- runModel.combineResObjects(warn1, obj2, strat, stratNum) 
    return(base)
  }

  nms <- getAllRetSheetNames()
  nms <- nms[!(nms %in% getTable1DfName())]
  n   <- length(nms)
  if (!n) stop("INTERNAL CODING ERROR")

  rnms <- c(getModelSummaryName(), getEffectsName())
  rv   <- getEffectsRunName()

  for (i in 1:n) {
    nm         <- nms[i]
    obj1       <- base[[nm, exact=TRUE]]
    obj2       <- new[[nm, exact=TRUE]]

    # Check if it is the ModelSummary or Effects data frame
    # If so then update the run column
    if ( (nm %in% rnms) && length(obj1) && length(obj2) ) {
      if ( (rv %in% colnames(obj1)) && (rv %in% colnames(obj2)) ) {
        n <- max(obj1[, rv, drop=TRUE], na.rm=TRUE)
        if (is.finite(n)) obj2[, rv] <- as.numeric(obj2[, rv, drop=TRUE]) + n
      }
    }

    base[[nm]] <- runModel.combineResObjects(obj1, obj2, strat, stratNum) 
  }

  base

} # END: runModel.combineResults

runModel.getStratVec <- function(gdta, scovs) {

  n   <- length(scovs)
  ret <- gdta[, scovs[1], drop=TRUE]
  if (n > 1) {
    for (i in 2:n) {
      ret <- paste(ret, gdta[, scovs[i], drop=TRUE], sep=runModel.getVarSep())
    }
  }
  
  ret

} # END: runModel.getStratVec

runModel.start <- function(modeldata, metabdata, op) {
   
  if (op$DEBUG) print(op)  

  nm <- getMaxNpairwiseOpName()
  m  <- op[[nm, exact=TRUE]]
  if ((length(modeldata$rcovs) > m) && (length(modeldata$ccovs) > m)) {
    msg <- msg_mod_3(nm)
    stop(msg)        
  }

  # Object for warning messages
  wr.nm    <- runModel.getWarningsListName()
  warn.obj <- modeldata[[wr.nm, exact=TRUE]]

  if (op$DONOTRUN) {
    scovs <- NULL
  } else {
    scovs <- modeldata[["scovs", exact=TRUE]]
  }
  nscovs <- length(scovs)

  if (!nscovs) return(runModel.main(modeldata, metabdata, op))
  gdta      <- modeldata$gdta

  stratvec  <- runModel.getStratVec(gdta, scovs)
  stratlist <- unique(stratvec)
  nstrata   <- length(stratlist)

  # Original strat var names
  svars0    <- runModel.varMap(scovs, modeldata$dict_metabnames)
  strat2    <- paste0(svars0, collapse=runModel.getVarSep())

  if (nstrata > op$max.nstrata) {
    msg <- msg_mod_4(c(paste0(scovs, collapse=","), op$max.nstrata))
    stop(msg)        
  }

  # Combine any warnings from getModelData() at the end
  modeldata[[wr.nm]] <- NULL

  c1      <- runModel.getWarningCol() 
  c2      <- runModel.getObjectCol()  
  c3      <- runModel.getMessageCol() 
  retList <- list()
  for (i in 1:nstrata) {
    strat <- stratlist[i]
    tmp   <- stratvec %in% strat
    n     <- sum(tmp)
    if (n < op$check.nsubjects) {
      # Too few subjects, do not run
      tmp  <- runModel.getStratTooFewSubStr()
    } else {
      modeldata$gdta <- gdta[tmp, , drop=FALSE]
      tmp <- try(runModel.main(modeldata, metabdata, op), silent=FALSE)
    }

    # Combine results
    #strat2  <- updateStrWithNewVars(strat, scovs, svars0)
    retList <- runModel.combineResults(retList, tmp, strat2, strat)
  }

  if (length(warn.obj)) {
    tmp              <- retList[[wr.nm, exact=TRUE]]
    retList[[wr.nm]] <- runModel.combineResObjects(tmp, warn.obj, "getModelData", NA) 
  } 
  tmp              <- retList[[wr.nm, exact=TRUE]]
  retList[[wr.nm]] <- runModel.setReturnDF(tmp, modeldata$varMap)

  retList

} # END: runModel.start

runModel.main <- function(modeldata, metabdata, op) {

  # Get the initial design matrix and other objects
  modeldata <- runModel.checkModelDesign(modeldata, metabdata, op)

  if (!length(names(modeldata))) return(modeldata)
  if (op$DONOTRUN) return(NULL)

  # Check if any adjustment vars are metabolites
  modeldata$designMatCols0 <- runModel.varMap(modeldata$designMatCols0, modeldata$varMap)
  if (op$DEBUG) print(modeldata)

  ms.nm <- getModelSummaryName()
  ef.nm <- getEffectsName()
  wr.nm <- runModel.getWarningsListName()

  # Run all metabolite and exposures
  tmp     <- runModel.runAllMetabs(modeldata, op) 
  ret     <- tmp[[ms.nm, exact=TRUE]]
  ret2    <- tmp[[ef.nm, exact=TRUE]]
  rem.obj <- tmp[[wr.nm, exact=TRUE]]
  if (!length(ret)) {
    retlist          <- list()
    retlist[[ms.nm]] <- ret
    retlist[[ef.nm]] <- ret2
    retlist[[wr.nm]] <- rem.obj
    return(retlist)
  }

  # Add on some additional cols to the fit df
  ret[, "cohort"]                       <- op$cohort
  ret[, getModelSummaryRunModeName()]   <- modeldata$modelspec
  ret[, "model"]                        <- modeldata$modlabel
  tmp                                   <- modeldata[["acovs", exact=TRUE]]
  if (length(tmp)) tmp <- runModel.varMap(tmp, modeldata$varMap)
  ret[, "adjspec"] <- runModel.getVarStr(tmp)
  ret <- fixData(ret)

  # Add metabolite info
  ret <- addMetabInfo(ret, modeldata, metabdata)

  # Let run, cohort, spec, model column be the first columns
  ret <- orderVars(ret, c("run", "cohort", getModelSummaryRunModeName(), "model"))

  # Append adjvars variables removed from checkModelDesign
  rem <- runModel.getVarsRemoved(rem.obj, type="adjvars")
  if (length(rem)) {
    sep <- runModel.getVarSep()
    rem <- runModel.getVarStr(rem, collapse=sep, default="") 
    v   <- "adjvars.removed"
    tmp <- nchar(ret[, v]) > 0
    if (any(tmp))  ret[tmp, v]  <- paste(rem, ret[tmp, v], sep=sep)
    if (any(!tmp)) ret[!tmp, v] <- rem
  }

  # Change outcomespec to correct name
  rows <- match(ret2[, "run"], ret[, "run"])
  tmp  <- !is.na(rows)
  rows <- rows[tmp]
  ret2[tmp, "outcomespec"] <- ret[rows, "outcomespec"]

  # Make sure certain columns in the retured data frames are numeric
  ret  <- runModel.setReturnDF(ret,  modeldata$varMap)
  ret2 <- runModel.setReturnDF(ret2, modeldata$varMap)
  
  # Chemical enrichment using RaMP
  retChem <- NULL
  if (op[[getRampCallChemEnrichOpName()]]) {
    retChem <- try(runModel.chemClassEnrichment(ret2, metabdata, op), silent=FALSE) 
    rem.obj <- runmodel.checkForError(retChem, warnStr="ERROR", objStr="runModel.chemClassEnrichment", rem.obj=rem.obj) 
  } 
  if (length(rem.obj)) rem.obj <- runModel.setReturnDF(rem.obj, modeldata$varMap)

  retlist          <- list()
  retlist[[ms.nm]] <- ret
  retlist[[ef.nm]] <- ret2
  retlist[[wr.nm]] <- rem.obj
  if (is.data.frame(retChem)) retlist[[getRampChemEnrichDfName()]] <- retChem

  retlist

} # END: runModel.main

runModel.defRetObj <- function(model, dmatCols0, op) {

  if (model == getCorrModelName()) {
    ret <- runModel.defRetObj.pcor(dmatCols0)
  } else if (model == getGlmModelName()) {
    ret <- runModel.defRetObj.glm(dmatCols0, op)
  } else if (model == getLmModelName()) {
    ret <- runModel.defRetObj.lm(dmatCols0, op)
  } else if (model == getCoxphModelName()) {
    ret <- runModel.defRetObj.coxph(dmatCols0, op)
  } else if (model == getClogitModelName()) {
    ret <- runModel.defRetObj.clogit(dmatCols0, op)
  } else {
    stop("INTERNAL CODING ERROR in runModel.defRetObj")
  }

  ret

} # END: runModel.defRetObj

runModel.getDesignSubs <- function(x) {
  
  # x: the design matrix. Must be a matrix, not a data frame
  # Get the rows of the design matrix x have no missing values
  tmp <- !is.finite(x)
  ret <- rowSums(tmp) == 0

  ret

} # END: runModel.getDesignSubs

runModel.getResponseSubs <- function(y, family) {

  if ((family == "") || (family == "gaussian")) {
    ret <- rep(TRUE, length(y))
  } else if ((family == "binomial") || (family == "quasibinomial")) {
    ret <- (y == 0) | (y == 1) 
  } else if ((family == "Gamma") || (family == "inverse.gaussian")) {
    ret <- y > 0
  } else if (family == "poisson") {
    ret <- ((y %% 1) == 0) & (y >= 0)
  } else if (family == "quasipoisson") {
    ret <- y > 0
  } else {
    ret <- rep(TRUE, length(y))
  }
  ret             <- ret & is.finite(y)
  ret[is.na(ret)] <- FALSE 

  ret

} # END: runModel.getResponseSubs

runModel.updateDesignMat <- function(modeldata, expVar, catvar, expref) {

  eStartCol  <- modeldata$designMatExpStartCol
  designMat  <- modeldata$designMat
  subOrder   <- modeldata$designSubOrder
  designCols <- colnames(designMat)

  # Initialize 
  designMat[, modeldata$designMatExpCols] <- NA

  if (catvar) {
    # Set reference
    if (length(expref)) {
      modeldata$gdta[, expVar] <- relevel(modeldata$gdta[, expVar], expref)
    }

    # Get dummy variables
    mat              <- runModel.getDummyVars(modeldata$gdta, expVar)
    expNames         <- colnames(mat)
    nc               <- ncol(mat)
    eEndCol          <- eStartCol + nc - 1
    ids              <- eStartCol:eEndCol
    cols             <- 1:eEndCol

    # If subjects were removed when getting the dummy variables,
    #   then we need to get the new subject order
    if (nrow(mat) != modeldata$gdta.nrow) {
      subOrder            <- match(rownames(designMat), rownames(mat))
      tmp                 <- !is.na(subOrder)
      designMat[tmp, ids] <- mat[subOrder[tmp], , drop=FALSE]
    } else { 
      designMat[, ids] <- mat[subOrder, , drop=FALSE]
    }
  } else {
    # continuous variable
    vec                    <- as.numeric(modeldata$gdta[[expVar]])
    designMat[, eStartCol] <- vec[subOrder] 
    cols                   <- 1:eStartCol
    expNames               <- expVar
    ids                    <- eStartCol
  }
  designCols[ids]     <- expNames
  colnames(designMat) <- designCols
  designMat           <- designMat[, cols, drop=FALSE]
  
  list(designMat=designMat, expNames=expNames)

} # END: runModel.updateDesignMat

runModel.callFunc <- function(x, y, expVars, op) {

  if (op$pcorrFlag) {
    ret <- runModel.calcCorr(x, y, expVars, op)
  } else if (op$glmFlag) {
    ret <- runModel.callGLM(x, y, op)
  } else if (op$lmFlag) {
    ret <- runModel.callLM(x, y, op)
  } else if (op$coxphFlag) {
    ret <- runModel.callCoxph(x, y, op)
  } else if (op$clogitFlag) {
    ret <- runModel.callClogit(x, y, op)
  } else {
    stop("INTERNAL CODING ERROR in runModel.callFunc")
  }

  ret

} # END: runModel.callFunc

runModel.tidy <- function(nsubs, fit, exposure, expVars, defObj, designMat, modeldata, op, expRef) {

  if (op$pcorrFlag) {
    ret <- runModel.tidyPcorr(nsubs, fit, expVars, defObj, designMat, modeldata$designMatCols)
  } else if (op$glmFlag || op$lmFlag) {
    # runModel.tidyGLM is used for glm and lm
    ret <- runModel.tidyGLM(nsubs, fit, exposure, expVars, defObj, modeldata, op, expRef) 
  } else if (op$coxphFlag) {
    ret <- runModel.tidyCoxph(nsubs, fit, exposure, expVars, defObj, modeldata, op, expRef) 
  } else {
    ret <- runModel.tidyClogit(nsubs, fit, exposure, expVars, defObj, modeldata, op, expRef) 
  }

  ret

} # END: runModel.tidy 

runModel.checkDesignWithExp <- function(dmat, op, expVar, varMap=NULL) {

  tmp  <- runModel.checkDesignMatCols(dmat, op, varMap=varMap)
  cols <- colnames(tmp$designMat)
  rem  <- tmp$rem.obj
  msg  <- ""
  tmp  <- expVar %in% cols
  vars <- expVar[tmp]

  # Return NULL if exposure was removed
  if (!length(vars)) {
    cols <- NULL
    msg  <- runModel.getRemMessage(rem, expVar, collapse=";", varMap=varMap)
    #msg  <- runModel.getExpRemFromDesign()
  }

  list(cols=cols, msg=msg, expVar=vars)

} # END: runModel.checkDesignWithExp

runModel.getMaxEffRows <- function(nrcovs, nlevels, nadjv, op) {

  if (op[[getOutEffectsOpName(), exact=TRUE]] == getOutEffectsOpDefault()) {
    N <- nrcovs*sum(nlevels)
  } else {
    N <- nrcovs*sum(nlevels) + nrcovs*length(nlevels)*(nadjv - 1)
  }

  N

} # END: runModel.getMaxEffRows

runModel.getMaxModSumRows <- function(newmodeldata, op) {

  nacovs <- length(newmodeldata[["acovs", exact=TRUE]])
  nccovs <- length(newmodeldata$ccovs)
  nrcovs <- length(newmodeldata$rcovs)

  if (op[[getOutEffectsOpName(), exact=TRUE]] == getOutEffectsOpDefault()) {
    N <- nrcovs*nccovs
  } else {
    N <- nrcovs*(nccovs*(nacovs + 1))     
  }

  N

} # END: runModel.getMaxModSumRows

runModel.getOutMessage <- function(msg1, msg2, sep="; ") {

  flag1 <- nchar(msg1)
  flag2 <- nchar(msg2)
  if (!flag1 && !flag2) return("")
  if (flag1 && flag2) {
    ret <- paste0(msg1, sep, msg2)
  } else if (flag1) {
    ret <- msg1
  } else {
    ret <- msg2
  }
  ret

} # END: runModel.getOutMessage

runModel.updateOpForSubset <- function(op, subset) {

  nm  <- getModelOpsName()
  mop <- op[[nm, exact=TRUE]]
  if (mop$weightsFlag) mop$weights.vec <- (mop$weights.vec)[subset]
  if (mop$offsetFlag) mop$offset.vec   <- (mop$offset.vec)[subset]
  if (mop$timeFlag) {
    ntime <- mop$n.time.vars
    mop$time1.vec <- (mop$time1.vec)[subset]
    if (ntime > 1) mop$time2.vec   <- (mop$time2.vec)[subset]
  }
  if (mop$groupFlag) mop$group.vec <- (mop$group.vec)[subset]
  op[[nm]] <- mop

  op

} # END: runModel.updateOpForSubset

runModel.runAllMetabs <- function(newmodeldata, op) {

  # Minimum number of subjects required
  minNsubs     <- op$check.nsubjects
  rcovs        <- newmodeldata$rcovs
  nrcovs       <- length(rcovs)
  ccovs        <- newmodeldata$ccovs
  nccovs       <- length(ccovs)
  dmatCols0    <- newmodeldata$designMatCols  
  varMap       <- newmodeldata$varMap
  checkDesign  <- op$check.design
  rem.obj      <- newmodeldata[[runModel.getWarningsListName(), exact=TRUE]]
  DEBUG        <- op$DEBUG
  if (DEBUG) print(rem.obj)
  tooFewSubs   <- runModel.getTooFewSubsStr()
  exprefs      <- as.character(newmodeldata[["exposurerefs", exact=TRUE]])

  # Get a default summary object when model fails 
  defObj <- runModel.defRetObj(op$model, dmatCols0, op)

  # Initialize objects to store results. For the special case of
  #  model=correlation and no adjusted covs, the continuous
  #  exposure variables will be taken care of in this call (for efficiency).
  tmp      <- runModel.initSaveObjects(defObj, newmodeldata, op) 
  rname    <- tmp$rname
  cname    <- tmp$cname
  coefMat  <- tmp$coefMat
  runRows1 <- tmp$runRows1
  runRows2 <- tmp$runRows2
  msgVec   <- tmp$msgVec
  adjVec   <- tmp$adjVec
  remVec   <- tmp$remVec
  fitMat   <- tmp$fitMat
  conv     <- tmp$conv
  waldP    <- tmp$waldP
  index1   <- tmp$index1
  index2   <- tmp$index2
  term     <- tmp$term
  ccovs    <- tmp[["ccovs", exact=TRUE]]
  nccovs   <- length(ccovs)
  isfactor <- tmp[["isfactor", exact=TRUE]]
  cov.str  <- tmp$cov.str
  if (nccovs) {
    CVEC <- 1:nccovs
  } else {
    CVEC <- numeric(0)
  }
  runNumber   <- index1
  exprefsFlag <- length(exprefs)
    
  # Loop over each exposure in the outer loop
  for (j in seq_along(CVEC)) {
    ccovj  <- ccovs[j]
    expref <- NULL 

    # Check reference
    if (isfactor[j]) {
      if (exprefsFlag) {
        expref <- exprefs[j]
        if (is.na(expref)) expref <- NULL
      }  
      levs <- levels(newmodeldata$gdta[, ccovj, drop=TRUE])
      if (!length(expref) && length(levs)) {
        # Get default reference
        expref <- levs[1]
      }
      if (length(expref) && !(expref %in% levs)) expref <- NULL
      if (!length(expref)) {  
        rem.obj <- runModel.addRemVars(rem.obj, ccovj, "exposures", 
                    runModel.expRefInvalid() , varMap=varMap, errType="ERROR")
        next
      }
    } 

    # Update the design matrix for this exposure var 
    tmp       <- runModel.updateDesignMat(newmodeldata, ccovj, isfactor[j], expref)
    x         <- tmp$designMat
    ccovNames <- tmp$expNames  # ccovj or dummy variables for this exposure
    tmp       <- NULL
    dcols     <- colnames(x)

    # Get the initial subset of subjects to use
    subset0 <- runModel.getDesignSubs(x)
    n0      <- sum(subset0)

    # Check for min number of subs
    if (n0 < minNsubs) {
      rem.obj <- runModel.addRemVars(rem.obj, ccovj, "exposures", 
                      tooFewSubs, varMap=varMap)
      next
    }

    # Design matrix checks, returns design columns kept 
    if (checkDesign) {
      tmp   <- runModel.checkDesignWithExp(x[subset0, , drop=FALSE], op, ccovNames,
                                           varMap=varMap)
      dcols     <- tmp[["cols", exact=TRUE]]
      ccovNames <- tmp$expVar
      if (!length(dcols)) {
        rem.obj <- runModel.addRemVars(rem.obj, ccovj, "exposures", tmp$msg, varMap=varMap)
        next
      }
    }

    # Final check of exposure columns
    ccovNames <- ccovNames[ccovNames %in% colnames(x)]
    if (!length(ccovNames)) {
      rem.obj <- runModel.addRemVars(rem.obj, ccovj, "exposures", 
                    runModel.getExpRemFromDesign(), varMap=varMap)
      next
    } 
    dcols0 <- dcols

    # Loop over each outcome
    for (i in 1:nrcovs) {

      rcovi <- rcovs[i]
      if (rcovi == ccovj) next

      # Get the outcome and other vars needed
      tmp    <- runModel.getModelVectors(newmodeldata, rcovi, op)
      y      <- tmp$response
      subset <- tmp$subset
      op     <- tmp$op 
      msg0   <- tmp$msg

      # Get the final subset of subjects to use 
      subset <- subset0 & subset
      nsubs  <- sum(subset)
      fit    <- NULL
      ccovs2 <- ccovNames
      dcols  <- dcols0

      if (nsubs >= minNsubs) {
        # Check design matrix if subjects were removed due to missing values
        if (checkDesign && (nsubs < n0)) {
          tmp    <- runModel.checkDesignWithExp(x[subset, , drop=FALSE], op, ccovNames,
                                                varMap=varMap)
          dcols  <- tmp[["cols", exact=TRUE]]
          fit    <- tmp$msg 
          ccovs2 <- tmp$expVar
        } 
        if (length(dcols)) {
          # Update vectors in op list for the final subset. Perhaps later change this to pass in the 
          #   final subset to each model function.
          op   <- runModel.updateOpForSubset(op, subset)
          fit  <- try(runModel.callFunc(x[subset, dcols, drop=FALSE], y[subset], 
                      ccovs2, op), silent=TRUE)
        }
      } else {
        fit <- tooFewSubs
      }
      if (DEBUG) {
        print(fit)
        print(summary(fit))
      } 

      # Get the results to save
      tmp <- runModel.tidy(nsubs, fit, ccovj, ccovNames, defObj, dcols, newmodeldata, op, expref)

      # Save results
      runNumber          <- runNumber + 1
      rname[runNumber]   <- rcovi
      cname[runNumber]   <- ccovj
      conv[runNumber]    <- tmp$converged
      msgVec[runNumber]  <- runModel.getOutMessage(msg0, tmp$msg)
      adjVec[runNumber]  <- tmp$adj
      remVec[runNumber]  <- tmp$adj.rem
      fitMat[runNumber,] <- tmp$fit.stats
      obj                <- tmp$wald.pvalue
      a                  <- index1 + 1
      b                  <- a + length(obj) - 1
      vec                <- a:b
      runRows1[vec]      <- runNumber
      waldP[vec]         <- obj
      term[vec]          <- names(obj)
      index1             <- b
      obj                <- tmp$coef.stats
      a                  <- index2 + 1 
      b                  <- a + nrow(obj) - 1
      vec                <- a:b
      runRows2[vec]      <- runNumber
      coefMat[vec, ]     <- obj
      index2             <- b
      obj                <- tmp[["cov.str", exact=TRUE]]  # covariance string for changing reference
      if (length(obj)) cov.str[runNumber] <- obj
    }
  }

  # Get objects to return
  tmp <- runModel.returnSaveObj(rname, cname, term, coefMat, runRows1, runRows2,
                                msgVec, adjVec, remVec, fitMat, conv, waldP, 
                                defObj, op, cov.str)

  ret                                   <- list()
  ret[[getModelSummaryName()]]          <- tmp[["ret1", exact=TRUE]]
  ret[[getEffectsName()]]               <- tmp[["ret2", exact=TRUE]]
  ret[[runModel.getWarningsListName()]] <- rem.obj
 
  ret

} # END: runModel.runAllMetabs

runModel.returnSaveObj <- function(rname, cname, term, coefMat, runRows1, runRows2, 
                                   msgVec, adjVec, remVec, fitMat, conv, waldP, 
                                   defObj, op, cov.str) {

  ret1 <- NULL 
  ret2 <- NULL
 
  if (!is.na(runRows1[1])) {
    # ModelSummary
    tmp             <- runRows1 > 0
    tmp[is.na(tmp)] <- FALSE 
    runRows1        <- runRows1[tmp]  
    ret1            <- data.frame(runRows1, rname[runRows1], cname[runRows1], 
                         conv[runRows1], term[tmp], waldP[tmp], 
                         fitMat[runRows1, , drop=FALSE], msgVec[runRows1], 
                         adjVec[runRows1], remVec[runRows1], cov.str[runRows1], 
                         stringsAsFactors=FALSE)
    colnames(ret1)  <- c(getEffectsRunName(), getEffectsOutcomespecName(), getEffectsExposurespecName(),
                         "converged", getEffectsTermName(), "wald.pvalue",
                         names(defObj$fit.stats), "message", 
                         "adjvars", "adjvars.removed", getModelSummaryCovStrCol())

    # Effects
    tmp             <- runRows2 > 0
    tmp[is.na(tmp)] <- FALSE 
    runRows2        <- runRows2[tmp]  
    ret2            <- data.frame(runRows2, rname[runRows2], cname[runRows2], 
                         coefMat[tmp, , drop=FALSE], stringsAsFactors=FALSE)
    colnames(ret2)  <- c(getEffectsRunName(), getEffectsOutcomespecName(), getEffectsExposurespecName(),
                        colnames(defObj$coef.stats))

    if (op$pcorrFlag) {
      ret1$converged                     <- NULL
      ret1$wald.pvalue                   <- NULL
      ret1[[getModelSummaryCovStrCol()]] <- NULL
    } else if (op$lmFlag) {
      ret1$converged                 <- NULL
      ret1$statistic                 <- NULL
      ret1$df                        <- NULL 
      ret1[[getEffectsPvalueName()]] <- NULL 
    } else if (op$coxphFlag || op$clogitFlag) {
      ret1$converged                 <- NULL
    }
  }

  list(ret1=ret1, ret2=ret2)

} # END: runModel.returnSaveObj

# Get vectors needed for analysis
runModel.getModelVectors <- function(modeldata, yvar, op) {

  subOrder <- modeldata$designSubOrder
  nm       <- getModelOpsName()
  mop      <- op[[nm]]
  msg      <- ""
  msgvec   <- rep(FALSE, 5)

  # response vector
  tmp      <- as.numeric(modeldata$gdta[[yvar]])  
  y        <- tmp[subOrder]

  # Determine the subset to use from the response
  subset <- runModel.getResponseSubs(y, mop$family)
  if (!all(subset)) msgvec[1] <- TRUE

  # Get other vectors if needed and store them in op
  if (mop$weightsFlag) {
    tmp             <- as.numeric(modeldata$gdta[[modeldata$wgtcov]])  
    vec             <- tmp[subOrder]
    mop$weights.vec <- vec
    tmp             <- (vec > 0) & is.finite(vec)
    subset          <- subset & tmp
    if (!all(tmp)) msgvec[2] <- TRUE
  }
  if (mop$offsetFlag) {
    tmp            <- as.numeric(modeldata$gdta[[modeldata$offcov]])  
    vec            <- tmp[subOrder]
    mop$offset.vec <- vec
    tmp            <- is.finite(vec)
    subset         <- subset & tmp
    if (!all(tmp)) msgvec[3] <- TRUE
  }
  if (mop$timeFlag) {
    timecov        <- modeldata$timecov
    tmp            <- as.numeric(modeldata$gdta[[timecov[1]]])  
    vec            <- tmp[subOrder]
    mop$time1.vec  <- vec
    tmp            <- is.finite(vec) & (vec >= 0)
    subset         <- subset & tmp
    if (!all(tmp)) msgvec[4] <- TRUE
    if (mop$n.time.var > 1) {
      tmp            <- as.numeric(modeldata$gdta[[timecov[2]]])  
      vec2           <- tmp[subOrder]
      mop$time2.vec  <- vec2
      tmp            <- is.finite(vec2) & (vec2 > vec)
      subset         <- subset & tmp
      if (!all(tmp)) msgvec[4] <- TRUE
    }
  }

  if (mop$groupFlag) {
    tmp            <- modeldata$gdta[[modeldata$groupcov]]
    vec            <- tmp[subOrder]
    mop$group.vec  <- vec
    tmp            <- is.finite(vec)
    subset         <- subset & tmp
    if (!all(tmp)) msgvec[5] <- TRUE
  }


  tmp <- is.na(subset)
  if (any(tmp)) subset[tmp] <- FALSE
  op[[nm]] <- mop

  if (any(msgvec)) {
    tmp <- c("OUTCOME", "WEIGHTS", "OFFSET", "TIME", "GROUP")
    msg <- infile.collapseVec(tmp[msgvec], sep=", ", begin="(", end=")", removeMiss=1)
    msg <- paste0("Invalid values for ", msg, " have been removed")
  }

  list(response=y, subset=subset, op=op, msg=msg)

} # END: runModel.getModelVectors 

runModel.designMat <- function(data, vars, categorical=NULL) {
  
  if (is.null(categorical)) categorical <- areCategorical(data, vars)
  if (any(categorical)) {
    # Add a "." to make dummy vars look nicer
    catVars           <- vars[categorical] 
    cx                <- colnames(data)
    tmp               <- cx %in% catVars
    cx[tmp]           <- paste(cx[tmp], ".", sep="")
    vars[categorical] <- paste(catVars, ".", sep="")
    colnames(data)    <- cx
  }  

  nvars <- length(vars)
  if (nvars) {
    form <- paste(vars, collapse=" + ", sep="")
  } else {
    form <- "1"
  }
  form <- as.formula(paste("~ ", form, sep=""))
  ret  <- model.matrix(form, data=data)

  ret

} # END: runModel.designMat

runModel.getDummyVars <- function(data, var) {

  ret <- runModel.designMat(data, var)
  if (ncol(ret) < 2) stop("INTERNAL CODING ERROR in runModel.getDummyVars")
  ret <- ret[, -1, drop=FALSE]

  ret

} # END: runModel.getDummyVars

runModel.getNlevels <- function(data, ccovs, isfactor) {

  n    <- length(ccovs)
  ret  <- rep(1, n)
  ids  <- (1:n)[isfactor]
  nids <- length(ids)
  if (nids) {
    # Loop over each categorical var
    for (i in 1:nids) {
      id      <- ids[i]
      ret[id] <- length(unique(data[, ccovs[id]])) - 1
    }
  }

  ret

} # END: runModel.getNlevels

runModel.getVarStr <- function(vars, collapse=NULL, default="") {

  if (length(vars)) {
    if (is.null(collapse)) collapse <- runModel.getVarSep()
    ret <- paste(vars, collapse=collapse, sep="")
  } else {
    ret <- default
  }

  ret

} # END: runModel.getVarStr

runModel.getAdjVarStr <- function(nms, dmatCols0, replaceSpace=1) {

  if (length(nms)) {
    orig <- dmatCols0[nms]
    if (length(orig) && replaceSpace) orig <- gsub(" ", runModel.replaceSpace(), orig, fixed=TRUE) 
  } else {
    orig <- NULL
  }
  ret <- runModel.getVarStr(orig)

  ret

} # END: runModel.getAdjVarStr 

# Function to compute the Wald test (2 - sided)
runModel.getWaldTest <- function(fit, parmNames, sfit=NULL) {

  # fit        Return object from glm, list with names "coefficients"
  #            and "cov.scaled", return object from snp.logistic or snp.matched.
  # parmNames  Character vector of parameters to test  
  # sfit       summary(fit)

  tmp   <- runModel.getEstCov(fit, sfit=sfit)
  parms <- tmp[["estimates", exact=TRUE]]
  cov   <- tmp[["cov", exact=TRUE]]
  ret   <- runModel.waldTest.main(parms, cov, parmNames)

  ret   
  
} # END: runModel.getWaldTest

# Function to get Wald p-values 
runModel.getWaldPvalues <- function(exposure, expVars, parmList, fit, sfit=NULL) {

  len   <- length(parmList)
  N     <- len + 1
  tmp   <- runModel.getEstCov(fit, sfit=sfit)
  parms <- tmp[["estimates", exact=TRUE]]
  cov   <- tmp[["cov", exact=TRUE]]
  ret   <- rep(NA, N)

  # For the exposure
  ret[1] <- runModel.waldTest.main(parms, cov, expVars)$pvalue
  
  if (len) {
    for (i in 1:len) { 
      tmp      <- runModel.waldTest.main(parms, cov, parmList[[i]])
      ret[i+1] <- tmp$pvalue 
    }
    names(ret) <- c(exposure, names(parmList))
  } else {
    names(ret) <- exposure
  }

  ret
}

# Function to return point estimates and covariance matrix from an object
runModel.getEstCov <- function(fit, sfit=NULL) {

  clss <- class(fit)
  
  if (any(clss == "glm")) {
    parms <- fit$coefficients
    if (!length(sfit)) sfit <- summary(fit)
    cov   <- sfit$cov.scaled
  } else if (any(clss == "lm")) {
    parms <- fit$coefficients
    if (!length(sfit)) sfit <- summary(fit)
    sigma <- sfit$sigma
    cov   <- (sfit$cov.unscaled)*sigma*sigma
  } else if (any(clss == "coxph")) {  # For coxph, clogit
    parms         <- fit$coefficients
    cov           <- fit$var
    vnames        <- names(parms)
    rownames(cov) <- vnames
    colnames(cov) <- vnames
  } else {
    parms <- NULL
    cov   <- NULL
  }

  list(estimates=parms, cov=cov)

} # END: runModel.getEstCov

# Function to compute the Wald test (2 - sided) 
runModel.waldTest.main <- function(parms, cov, varnames) {

  # parms      Parameter vector
  # cov        Covariance matrix
  # varnames   Character vector of parameters to test

  ret    <- list(pvalue=NA, df=0, test=NA)
  if ((!length(varnames)) || (!length(parms)) || (!length(cov))) return(ret)

  tmp <- (varnames %in% names(parms)) & (varnames %in% colnames(cov))
  varnames <- varnames[tmp]
  if (!length(varnames)) return(ret)

  pvec   <- parms[varnames]
  covmat <- cov[varnames, varnames, drop=FALSE]
  tmp    <- is.finite(pvec) & is.finite(diag(covmat))
  if (!all(tmp)) {
    pvec   <- pvec[tmp]
    covmat <- covmat[tmp, tmp]
  }
  df <- length(pvec)

  if (df == 1) {
    test   <- pvec/sqrt(as.numeric(covmat))
    pvalue <- 2*pnorm(abs(test), lower.tail=FALSE) 
  } else {
    test   <- NA
    pvalue <- NA

    # See if matrix is invertible
    inv <- try(solve(covmat), silent=TRUE)
    if (!("try-error" %in% class(inv))) {
      dim(pvec) <- c(df, 1)
      test      <- as.numeric(t(pvec) %*% inv %*% pvec)
      if (!is.finite(test)) test <- NA
      if (test >= 0) pvalue <- pchisq(test, df=df, lower.tail=FALSE) 
    }
  } 

  list(pvalue=pvalue, df=df, test=test)

} # END: runModel.waldTest.main

runModel.getFormulaStr <- function(yvar, dmatCols, time1.var=NULL, time2.var=NULL, type=NULL, strata.var=NULL) {

  str0 <- paste(dmatCols, collapse=" + ", sep="")
  if (is.null(time1.var)) {
    str <- paste(yvar, " ~ ", str0, sep="")
    if (!is.null(strata.var)) str <- paste0(str, " + strata(", strata.var, ")")  # For clogit
  } else { # For coxph
    str <- paste0("Surv(", time1.var, ", ")
    if (!is.null(time2.var)) str <- paste0(str, time2.var, ", ")
    str <- paste0(str, yvar)
    if (!is.null(type)) str <- paste0(str, ", type=", type)
    str <- paste0(str, ") ~ ", str0)
  }
  str

} # END: runModel.getFormulaStr

# Function to initialize objects to store results
runModel.initSaveObjects <- function(defObj, modeldata, op) {

  nrcovs       <- length(modeldata$rcovs)
  nccovs       <- length(modeldata$ccovs)
  nruns        <- nrcovs*nccovs
  ndmatCols    <- length(modeldata$designMatCols)  

  # N1 max number of rows for ModelSummary
  # N2 max number of rows for Effects

  # Get the maximum number of rows in the return ModelSummary data frame
  N1 <- runModel.getMaxModSumRows(modeldata, op) 

  # Get the maximum number of rows in the return Effects data frame
  N2 <- runModel.getMaxEffRows(nrcovs, modeldata$nlevels, ndmatCols, op)

  # Length 1 objects for each outcome/exposure
  runVec  <- rep(NA, nruns)
  rname   <- rep("", nruns)
  cname   <- rep("", nruns)
  conv    <- rep(FALSE, nruns)
  msgVec  <- rep("", nruns)
  adjVec  <- rep("", nruns)
  remVec  <- rep("", nruns)

  # For ModelSummary data frame
  runRows1 <- rep(NA, N1)
  fitMat   <- matrix(data=NA, nrow=N1, ncol=length(defObj$fit.stats))
  waldP    <- rep(NA, N1)
  term     <- rep("", N1)
  cov.str  <- rep("", N1)

  # For Effects data frame
  coefMat  <- matrix(data=NA, nrow=N2, ncol=ncol(defObj$coef.stats))
  runRows2 <- rep(NA, N2)

  k        <- 0
  k1       <- 0
  ccovs0   <- modeldata$ccovs
  isfac0   <- modeldata$isfactor
  nlev0    <- modeldata$nlevels

  # Check for a special case of the correlation: no covariate adjustments
  #   and some continuous exposure variables
  if (1 && op$pcorrFlag && !length(modeldata[["acovs", exact=TRUE]]) &&
      any(!(modeldata$isfactor))) {
    tmp               <- runModel.pcor.special(modeldata, op) 
    ccovs             <- tmp$ccovVec
    colnames(coefMat) <- colnames(defObj$coef.stats)
    colnames(fitMat)  <- names(defObj$fit.stats)
    nruns             <- length(ccovs)
    vec               <- 1:nruns
    rname[vec]        <- tmp$rcovVec
    cname[vec]        <- ccovs
    v                 <- getEffectsCorrEstName()
    coefMat[vec, v]   <- tmp$corr
    v                 <- getEffectsPvalueName()
    coefMat[vec, v]   <- tmp$pvalue
    v                 <- getEffectsTermName()
    coefMat[vec, v]   <- ccovs
    runVec[vec]       <- vec
    runRows1[vec]     <- vec
    runRows2[vec]     <- vec
    v                 <- getModelSummaryNobsName()
    nobs              <- tmp$nobs
    fitMat[vec, v]    <- nobs
    tmp               <- nobs < op$check.nsubjects
    tmp[is.na(tmp)]   <- TRUE
    if (any(tmp)) msgVec[tmp] <- runModel.getTooFewSubsStr()
     
    k         <- nruns
    k1        <- nruns
    tmp       <- !(ccovs0 %in% unique(ccovs))
    ccovs0    <- ccovs0[tmp]  
    isfac0    <- isfac0[tmp]
    nlev0     <- nlev0[tmp]
  }

  list(rname=rname, cname=cname, coefMat=coefMat, runVec=runVec, 
       runRows1=runRows1, runRows2=runRows2, msgVec=msgVec, adjVec=adjVec, 
       remVec=remVec, fitMat=fitMat, conv=conv, waldP=waldP, index1=k, index2=k1, 
       ccovs=ccovs0, isfactor=isfac0, nlevels=nlev0, term=term, cov.str=cov.str)

} # END: runModel.initSaveObjects

# Function to add metabolite columns on to data frames from runModel
runModel.addMetabCols <- function(obj, metabdata, op) {

  DEBUG <- op$DEBUG
  if (DEBUG) print("Begin: runModel.addMetabCols")
  add   <- op[[getAddMetabColsOpName(), exact=TRUE]]
  if (DEBUG) print(add)
  if (!length(add)) return(obj)
  if (!any(nchar(add))) return(obj)
  metab <- metabdata$metab
  if (DEBUG) print(metab[1, , drop=FALSE])
  metabcols       <- tolower(colnames(metab))
  colnames(metab) <- metabcols
  add             <- tolower(add)
  tmp             <- add %in% metabcols
  add             <- add[tmp]
  if (DEBUG) print(add)
  if (!length(add)) return(obj)
  
  metabidv <- metabdata$metabId
  ov       <- getEffectsOutcomespecName()
  ev       <- getEffectsExposurespecName()

  nm       <- getModelSummaryName()
  x        <- obj[[nm, exact=TRUE]]
  if (length(x)) {
    x         <- addColsToDF(x, ov, metab, metabidv, add, init=1, DEBUG=DEBUG) 
    x         <- addColsToDF(x, ev, metab, metabidv, add, init=0, DEBUG=DEBUG) 
    obj[[nm]] <- x
  }

  nm       <- getEffectsName()
  x        <- obj[[nm, exact=TRUE]]
  if (length(x)) {
    x         <- addColsToDF(x, ov, metab, metabidv, add, init=1, DEBUG=DEBUG) 
    x         <- addColsToDF(x, ev, metab, metabidv, add, init=0, DEBUG=DEBUG) 
    obj[[nm]] <- x
  }

  obj

}

runModel.chemClassEnrichment <- function(df, metabdata, op) {

  if (is.list(op)) {
    ramp_op <- op[[getRampOpName(), exact=TRUE]]
  } else {
    ramp_op <- NULL
  }
  #ret <- chemClassEnrichment(df, metabdata, op=ramp_op)
  ret <- NULL
  ret
} 

runModel.getInfoDF <- function(ret, modeldata, metabdata, op) {

  if (!is.list(ret)) return(ret)
  x <- try(getInfoTableDF(modeldata, metabdata, op), silent=TRUE)
  ret[[getInfoTableDfName()]] <- x
  ret
}

runModel.getUpperTriCovStr <- function(fit, sfit, parms, expRef) {

  ret <- ""
  n   <- length(parms)
  if ((n < 2) || ("try-error" %in% class(fit))) return(ret)
  if (!length(expRef)) return(ret)

  cov <- runModel.getEstCov(fit, sfit=sfit)$cov
  if (length(cov)) {
    nms <- colnames(cov)
    tmp <- parms %in% nms
    if (!any(tmp)) return(ret)
    parms0 <- parms
    parms  <- parms[tmp]
    cov    <- cov[parms, parms]
    m      <- nrow(cov)
    if (m < n) {
      tmp               <- matrix(data=NA, nrow=n, ncol=n)
      rownames(tmp)     <- parms0
      colnames(tmp)     <- parms0
      tmp[parms, parms] <- cov
      cov               <- tmp
    }
    vec   <- cov[upper.tri(cov)]
    sep1  <- getModelSummaryCovStrSep1()
    sep2  <- getModelSummaryCovStrSep2()
    s1    <- paste0(parms0, collapse=sep1)
    s2    <- paste0(vec, collapse=sep1)
    ret   <- paste0(expRef, sep2, s1, sep2, s2) 
  }
  ret
}
