#---------------------------------------------------------
#' Main function for running the models
#'
#' @param modeldata list from function \code{\link{getModelData}}
#' @param metabdata metabolite data list from \code{\link{readCOMETSinput}}
#' @param cohort cohort label (e.g DPP, NCI, Shanghai)
#' @param op list of options when running in \code{Interactive} mode (see \code{\link{options}}).
#'
#' @return A list of objects with names \code{\link{ModelSummary}},
#'        \code{\link{Effects}}, and \code{\link{Errors_Warnings}}. \cr
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
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,exposures="age",modlabel="1 Gender adjusted",
#' 	outcomes=c("lactose","lactate"))
#' obj <- runModel(modeldata,exmetabdata, "DPP")
#' @export

runModel <- function(modeldata, metabdata, cohort="", op=NULL) {

  ptm <- base::proc.time() # start processing time

  # Error checks
  runModel.checkModeldata(modeldata)
  runModel.checkMetabdata(metabdata)
  if (!isString(cohort)) stop("ERROR: cohort must be a string")

  # Check if options were obtained in getModelData
  if (modeldata$modelspec == getMode_batch()) {
    if (!is.null(op)) {
      tmp <- list()
      msg <- paste0("options for runModel were obtained from the excel options sheet,",
                    " not from the op list passed in")
      tmp[[runModel.getWarningCol()]] <- "WARNING"
      tmp[[runModel.getObjectCol()]]  <- "op"
      tmp[[runModel.getMessageCol()]] <- msg
      warning(msg)
      nm              <- runModel.getWarningsListName()
      modeldata[[nm]] <- runmodel.addWarning(modeldata[[nm, exact=TRUE]], tmp)
    }
    op <- modeldata$options 
  }

  op        <- runModel.checkOptions(op, modeldata)
  op$cohort <- cohort
  ret       <- runModel.start(modeldata, metabdata, op)

  # Stop the clock
  ptm <- base::proc.time() - ptm
  attr(ret, "ptime") <- paste("Processing time:", round(ptm[3], digits=3), "sec")
       
  ret

} # END: runModel 

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
             "outcome", "outcome_uid", "spec", "strata",
             "type", "object") 
  cx    <- colnames(x)
  tmp   <- !(cx %in% cvars)
  vars  <- cx[tmp]
  tmp   <- vars %in% colnames(x)
  if (any(tmp)) {
    for (v in vars[tmp]) x[, v] <- as.numeric(x[, v])
  }

  # Remove adj and exposure columns (duplicated information)
  if ("adj" %in% colnames(x)) x$adj <- NULL
  if ("exposure" %in% colnames(x)) x$exposure <- NULL

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

  ms.nm <- getModelSummaryName()
  ef.nm <- getEffectsName()
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

  obj1 <- base[[ms.nm, exact=TRUE]]
  obj2 <- new[[ms.nm, exact=TRUE]]
  base[[ms.nm]] <- runModel.combineResObjects(obj1, obj2, strat, stratNum) 

  obj1 <- base[[ef.nm, exact=TRUE]]
  obj2 <- new[[ef.nm, exact=TRUE]]
  base[[ef.nm]] <- runModel.combineResObjects(obj1, obj2, strat, stratNum) 

  obj2 <- new[[wr.nm, exact=TRUE]]
  base[[wr.nm]] <- runModel.combineResObjects(warn1, obj2, strat, stratNum)

  base

} # END: runModel.combineResults

runModel.getStratVec <- function(gdta, scovs) {

  n   <- length(scovs)
  ret <- paste(scovs[1], "=", gdta[, scovs[1]], sep="")
  if (n > 1) {
    for (i in 2:n) {
      tmp <- paste(scovs[i], "=", gdta[, scovs[i]], sep="")
      ret <- paste(ret, tmp, sep=";")
    }
  }
  
  ret

} # END: runModel.getStratVec

runModel.start <- function(modeldata, metabdata, op) {
   
  if (op$DEBUG) print(op)  

  # Object for warning messages
  wr.nm    <- runModel.getWarningsListName()
  warn.obj <- modeldata[[wr.nm, exact=TRUE]]

  scovs  <- modeldata[["scovs", exact=TRUE]]
  nscovs <- length(scovs)

  if (!nscovs) return(runModel.main(modeldata, metabdata, op))
  gdta      <- modeldata$gdta

  stratvec  <- runModel.getStratVec(gdta, scovs)
  stratlist <- unique(stratvec)
  nstrata   <- length(stratlist)

  # Original strat var names
  svars0    <- runModel.varMap(scovs, modeldata$dict_metabnames)

  if (nstrata > op$max.nstrata) {
    msg <- paste("The stratification variable(s) ",
                 paste(scovs, collapse=",", sep=""),
                 " contains more than ", op$max.nstrata, 
                 " unique values, which is too many for our software.",
                 " Please check your stratification variable(s)",
                 sep="")
    stop(msg)        
  }

  # Combine any warnings from getModelData() at the end
  modeldata[[wr.nm]] <- NULL

  c1      <- runModel.getWarningCol 
  c2      <- runModel.getObjectCol  
  c3      <- runModel.getMessageCol 
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
      tmp <- try(runModel.main(modeldata, metabdata, op), silent=TRUE)
    }

    # Combine results
    strat2  <- updateStrWithNewVars(strat, scovs, svars0)
    retList <- runModel.combineResults(retList, tmp, strat2, i)
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
  ret[, "cohort"] <- op$cohort
  ret[, "spec"]   <- modeldata$modelspec
  ret[, "model"]  <- modeldata$modlabel
  tmp <- modeldata[["acovs", exact=TRUE]]
  if (length(tmp)) tmp <- runModel.varMap(tmp, modeldata$varMap)
  ret[, "adjspec"] <- runModel.getVarStr(tmp)
  ret <- fixData(ret)

  # Add metabolite info
  ret <- addMetabInfo(ret, modeldata, metabdata)

  # Let run, cohort, spec, model column be the first columns
  ret <- orderVars(ret, c("run", "cohort", "spec", "model"))

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
  if (length(rem.obj)) rem.obj <- runModel.setReturnDF(rem.obj, modeldata$varMap)

  retlist          <- list()
  retlist[[ms.nm]] <- ret
  retlist[[ef.nm]] <- ret2
  retlist[[wr.nm]] <- rem.obj

  retlist

} # END: runModel.main

runModel.defRetObj <- function(model, dmatCols0) {

  if (model == getCorrModelName()) {
    ret <- runModel.defRetObj.pcor(dmatCols0)
  } else if (model == getGlmModelName()) {
    ret <- runModel.defRetObj.glm(dmatCols0)
  } else if (model == getLmModelName()) {
    ret <- runModel.defRetObj.lm(dmatCols0)
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

} # END: runModel.getUseSubjects

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

runModel.updateDesignMat <- function(modeldata, expVar, catvar) {

  eStartCol  <- modeldata$designMatExpStartCol
  designMat  <- modeldata$designMat
  subOrder   <- modeldata$designSubOrder
  designCols <- colnames(designMat)

  # Initialize 
  designMat[, modeldata$designMatExpCols] <- NA

  if (catvar) {
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
  } else {
    ret <- runModel.callLM(x, y, op)
  }

  ret

} # END: runModel.callFunc

runModel.tidy <- function(nsubs, fit, expVars, defObj, designMat, dmatCols0, op) {

  if (op$pcorrFlag) {
    ret <- runModel.tidyPcorr(nsubs, fit, expVars, defObj, designMat, dmatCols0)
  } else {
    ret <- runModel.tidyGLM(nsubs, fit, expVars, defObj, dmatCols0) 
  }

  ret

} # END: runModel.tidy 

runModel.runAllMetabs <- function(newmodeldata, op) {

  # Minimum number of subjects required
  minNsubs     <- op$check.nsubjects
  rcovs        <- newmodeldata$rcovs
  nrcovs       <- length(rcovs)
  ccovs        <- newmodeldata$ccovs
  nccovs       <- length(ccovs)
  nruns        <- nrcovs*nccovs
  dmatCols0    <- newmodeldata$designMatCols  
  varMap       <- newmodeldata$varMap
  checkDesign  <- op$check.design
  rem.obj      <- newmodeldata[[runModel.getWarningsListName(), exact=TRUE]]
  DEBUG        <- op$DEBUG
  if (DEBUG) print(rem.obj)
  tooFewSubs   <- runModel.getTooFewSubsStr()

  # Get the maximum number of rows in the return object for effects
  N <- nrcovs*sum(newmodeldata$nlevels)

  # Get a default summary object when model fails 
  defObj <- runModel.defRetObj(op$model, dmatCols0)

  # Initialize objects to store results. For the special case of
  #  model=correlation and no adjusted covs, the continuous
  #  exposure variables will be taken care of in this call (for efficiency).
  tmp      <- runModel.initSaveObjects(N, nruns, defObj, newmodeldata, op) 
  rname    <- tmp$rname
  cname    <- tmp$cname
  coefMat  <- tmp$coefMat
  runVec   <- tmp$runVec
  runRows  <- tmp$runRows
  msgVec   <- tmp$msgVec
  adjVec   <- tmp$adjVec
  remVec   <- tmp$remVec
  fitMat   <- tmp$fitMat
  conv     <- tmp$conv
  waldP    <- tmp$waldP
  k        <- tmp$k
  k1       <- tmp$k1
  ccovs    <- tmp[["ccovs", exact=TRUE]]
  nccovs   <- length(ccovs)
  isfactor <- tmp[["isfactor", exact=TRUE]]
  if (nccovs) {
    CVEC <- 1:nccovs
  } else {
    CVEC <- numeric(0)
  }
    
  # Loop over each exposure in the outer loop
  for (j in seq_along(CVEC)) {
    ccovj <- ccovs[j]

    # Update the design matrix for this exposure var 
    tmp       <- runModel.updateDesignMat(newmodeldata, ccovj, isfactor[j])
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
      tmp <- runModel.tidy(nsubs, fit, ccovNames, defObj, dcols, dmatCols0, op)

      # Save results
      k              <- k + 1
      k1             <- k1 + 1
      coef           <- tmp$coef.stats
      k2             <- k + nrow(coef) - 1
      vec            <- k:k2
      runRows[k1]    <- k
      rname[vec]     <- rcovi
      cname[vec]     <- ccovj
      coefMat[vec, ] <- coef
      runVec[vec]    <- k1
      msgVec[k1]     <- tmp$msg
      adjVec[k1]     <- tmp$adj
      remVec[k1]     <- tmp$adj.rem
      fitMat[k1,]    <- tmp$fit.stats
      conv[k1]       <- tmp$converged
      waldP[k1]      <- tmp$wald.pvalue
      k              <- k2
    }
  }

  # Get objects to return
  tmp <- runModel.returnSaveObj(k, k1, N, rname, cname, coefMat, runVec, runRows,
                                msgVec, adjVec, remVec, fitMat, conv, waldP, 
                                defObj, op)

  ret                                   <- list()
  ret[[getModelSummaryName()]]          <- tmp[["ret1", exact=TRUE]]
  ret[[getEffectsName()]]               <- tmp[["ret2", exact=TRUE]]
  ret[[runModel.getWarningsListName()]] <- rem.obj
 
  ret

} # END: runModel.runAllMetabs

runModel.returnSaveObj <- function(k, k1, N, rname, cname, coefMat, runVec, runRows,
                                   msgVec, adjVec, remVec, fitMat, conv, waldP, 
                                   defObj, op) {

  ret1 <- NULL 
  ret2 <- NULL
  
  if (k) {
    tmp  <- 1:k1
    ret1 <- data.frame(tmp, rname[runRows], cname[runRows], conv[tmp], waldP[tmp], 
                       fitMat[tmp, , drop=FALSE], 
                       msgVec[tmp], adjVec[tmp], remVec[tmp], stringsAsFactors=FALSE)
    colnames(ret1) <- c("run", "outcomespec", "exposurespec", "converged", "wald.pvalue",
                        names(defObj$fit.stats), "message", "adjvars", "adjvars.removed")
    ret2 <- data.frame(runVec, rname, cname, coefMat, stringsAsFactors=FALSE)
    colnames(ret2) <- c("run", "outcomespec", "exposurespec", 
                        colnames(defObj$coef.stats))

    # Subset if needed
    if (k < N) ret2 <- ret2[1:k, , drop=FALSE]
    if (op$pcorrFlag) {
      ret1$converged   <- NULL
      ret1$wald.pvalue <- NULL
    } else if (op$lmFlag) {
      ret1$converged   <- NULL
      ret1$statistic   <- NULL
      ret1$df          <- NULL 
      ret1$p.value     <- NULL 
    }
  }

  list(ret1=ret1, ret2=ret2)

} # END: runModel.returnSaveObj

# Get vectors needed for analysis
runModel.getModelVectors <- function(modeldata, yvar, op) {

  subOrder <- modeldata$designSubOrder
  nm       <- getModelOpsName()
  mop      <- op[[nm]]

  # response vector
  tmp      <- as.numeric(modeldata$gdta[[yvar]])  
  y        <- tmp[subOrder]

  # Determine the subset to use from the response
  subset <- runModel.getResponseSubs(y, mop$family)

  # Get other vectors if needed and store them in op
  if (mop$weightsFlag) {
    tmp             <- as.numeric(modeldata$gdta[[modeldata$wgtcov]])  
    vec             <- tmp[subOrder]
    mop$weights.vec <- vec
    subset          <- subset & (vec > 0) & is.finite(vec) 
  }
  if (mop$offsetFlag) {
    tmp            <- as.numeric(modeldata$gdta[[modeldata$offcov]])  
    vec            <- tmp[subOrder]
    mop$offset.vec <- vec
    subset         <- subset & is.finite(vec) 
  }
  tmp <- is.na(subset)
  if (any(tmp)) subset[tmp] <- FALSE
  op[[nm]] <- mop

  list(response=y, subset=subset, op=op)

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

runModel.getVarStr <- function(vars, collapse=";", default="") {

  if (length(vars)) {
    ret <- paste(vars, collapse=collapse, sep="")
  } else {
    ret <- default
  }

  ret

} # END: runModel.getVarStr

runModel.getAdjVarStr <- function(nms, dmatCols0) {

  if (length(nms)) {
    orig <- dmatCols0[nms]
  } else {
    orig <- NULL
  }
  ret <- runModel.getVarStr(orig)

  ret

} # END: runModel.getAdjVarStr 

# Function to compute the Wald test (2 - sided)
runModel.getWaldTest <- function(fit, parmNames, sfit=NULL, whichCov=NULL) {

  # fit        Return object from glm, list with names "coefficients"
  #            and "cov.scaled", return object from snp.logistic or snp.matched.
  # parmNames  Character vector of parameters to test  
  # sfit       summary(fit)
  # whichCov   For survival, robust or non-robust

  tmp   <- runModel.getEstCov(fit, sfit=sfit, which=whichCov)
  parms <- tmp[["estimates", exact=TRUE]]
  cov   <- tmp[["cov", exact=TRUE]]
  ret   <- runModel.waldTest.main(parms, cov, parmNames)

  ret   
  
} # END: runModel.getWaldTest

# Function to return point estimates and covariance matrix from an object
runModel.getEstCov <- function(fit, sfit=NULL, which=NULL) {

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
  } else if (any(clss == "coxph")) {
    parms <- fit$coefficients
    if (!is.null(which)) {
      cov <- fit[["which", exact=TRUE]]
    } else {
      cov <- fit$var
    }
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

runModel.getFormulaStr <- function(yvar, dmatCols) {

  str <- paste(dmatCols, collapse=" + ", sep="")
  str <- paste(yvar, " ~ ", str, sep="")
  str

} # END: runModel.getFormulaStr

# Function to initialize objects to store results
runModel.initSaveObjects <- function(N, nruns, defObj, modeldata, op) {

  rname    <- rep("", N)
  cname    <- rep("", N)
  coefMat  <- matrix(data=NA, nrow=N, ncol=ncol(defObj$coef.stats))
  runVec   <- rep(0, N)
  runRows  <- rep(0, nruns)
  msgVec   <- rep("", nruns)
  adjVec   <- rep("", nruns)
  remVec   <- rep("", nruns)
  fitMat   <- matrix(data=NA, nrow=nruns, ncol=length(defObj$fit.stats))
  conv     <- rep(FALSE, nruns)
  waldP    <- rep(NA, nruns)
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
    runRows[vec]      <- vec
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
       runRows=runRows, msgVec=msgVec, adjVec=adjVec, remVec=remVec,
       fitMat=fitMat, conv=conv, waldP=waldP, k=k, k1=k1, 
       ccovs=ccovs0, isfactor=isfac0, nlevels=nlev0)

} # END: runModel.initSaveObjects



