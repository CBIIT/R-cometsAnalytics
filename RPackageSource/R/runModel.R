
runModel <- function(modeldata, metabdata, cohort="", 
                     model="pcorr", family="gaussian", link="", ...) {

  # model    "pcorr" or "glm"
  # family   String for the family to use
  # link     String for the link function. The default will be to use
  #          the canonical link.
  # ...      Additional arguments

  ptm <- base::proc.time() # start processing time

  op        <- list(...)
  op$cohort <- cohort
  model     <- runModel.check.model(model)
  op$model  <- model
  op        <- runModel.checkOptions(op)
  if (model != "pcorr") {
    op$family <- runModel.check.family(family)
    op$link   <- runModel.check.link(link, op$family)
  }
  ret       <- runModel.start(modeldata, metabdata, op)

  # Stop the clock
  ptm <- base::proc.time() - ptm
  attr(ret, "ptime") <- paste("Processing time:", round(ptm[3], digits=3), "sec")
       
  ret

} # END: runModel 

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

runModel.setReturnDF <- function(x, varMap) {

  if (!length(x)) return(x)

  x    <- as.data.frame(x, stringsAsFactors=FALSE)
  vars <- c("estimate", "std.error", "statistic", "p.value",
            "null.deviance", "df.null", "logLik", "AIC",
            "BIC", "deviance", "df.residual", "nobs", "corr", "run")
  tmp  <- vars %in% colnames(x)
  if (any(tmp)) {
    for (v in vars[tmp]) x[, v] <- as.numeric(x[, v])
  }

  # Remove adj and exposure columns (duplicated information)
  if ("adj" %in% colnames(x)) x$adj <- NULL
  if ("exposure" %in% colnames(x)) x$exposure <- NULL

  # Make sure terms and exposure columns that have metabolites have
  #   the metabolite name instead of ...j
  vars <- c("exposurespec", "term")
  tmp  <- vars %in% colnames(x)
  vars <- vars[tmp]
  if (length(vars)) {
    for (v in vars) x[, v] <- runModel.varMap(x[, v], varMap)
  }

  x

} # END: runModel.setReturnDF

runModel.combineResObjects <- function(base, new, strat) {

  d <- dim(new)
  if (!length(d)) return(base)
  tmp <- colnames(new)
  if (length(strat) != 1) stop("INTERNAL CODING ERROR 1")
  new <- cbind(new, strat)
  colnames(new) <- c(tmp, "strata")
  if (!length(base)) {
    ret <- new
  } else {
    if (!identical(colnames(base), colnames(new))) stop("INTERNAL CODING ERROR 2")
    ret <- rbind(base, new)
  }

  ret

} # END: runModel.combineResObjects

runModel.combineResults <- function(base, new, strat) {

  if (!length(base)) base <- list()
  warn1 <- base[["Warnings", exact=TRUE]]
  if (("try-error" %in% class(new)) || isString(new)) {
    msg <- runModel.getErrorMsg(new)
    tmp <- list()
    tmp[[runModel.getWarningCol()]] <- "ERROR"
    tmp[[runModel.getObjectCol()]]  <- ""
    tmp[[runModel.getMessageCol()]] <- msg
    obj2 <- runmodel.addWarning(NULL, tmp)
    base[["Warnings"]] <- runModel.combineResObjects(warn1, obj2, strat) 
    return(base)
  }

  obj1 <- base[["ModelSummary", exact=TRUE]]
  obj2 <- new[["ModelSummary", exact=TRUE]]
  base[["ModelSummary"]] <- runModel.combineResObjects(obj1, obj2, strat) 

  obj1 <- base[["Effects", exact=TRUE]]
  obj2 <- new[["Effects", exact=TRUE]]
  base[["Effects"]] <- runModel.combineResObjects(obj1, obj2, strat) 

  obj2 <- new[["Warnings", exact=TRUE]]
  base[["Warnings"]] <- runModel.combineResObjects(warn1, obj2, strat)

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

  if (op$model == "glm") op$famFun <- runModel.getFamilyFun(op$family, op$link)   
  if (op$DEBUG) print(op)

  # Object for warning messages
  warn.obj <- NULL

  scovs  <- modeldata[["scovs", exact=TRUE]]
  nscovs <- length(scovs)

  if (!nscovs) return(runModel.main(modeldata, metabdata, op))
  gdta      <- modeldata$gdta

  stratvec  <- runModel.getStratVec(gdta, scovs)
  stratlist <- unique(stratvec)
  nstrata   <- length(stratlist)
  if (nstrata > op$max.nstrata) {
    msg <- paste("The stratification variable(s) ",
                 paste(scovs, collapse=",", sep=""),
                 " contains more than ", op$max.nstrata, 
                 " unique values, which is too many for our software.",
                 " Please check your stratification variable(s)",
                 sep="")
    stop(msg)        
  }

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
      tmp  <- "Stratum contains to few subjects"
    } else {
      modeldata$gdta <- gdta[tmp, , drop=FALSE]
      tmp <- try(runModel.main(modeldata, metabdata, op), silent=TRUE)
    }

    # Combine results
    retList <- runModel.combineResults(retList, tmp, strat)
  }

  retList

} # END: runModel.start

runModel.main <- function(modeldata, metabdata, op) {

  # Get the initial design matrix and other objects
  modeldata <- runModel.checkModelDesign(modeldata, metabdata, op)
  if (!length(names(modeldata))) return(modeldata)

  # Check if any adjustment vars are metabolites
  modeldata$designMatCols0 <- runModel.varMap(modeldata$designMatCols0, modeldata$varMap)
  if (op$DEBUG) print(modeldata)

  # Run all metabolite and exposures
  tmp     <- runModel.runAllMetabs(modeldata, op) 
  ret     <- tmp[["fit", exact=TRUE]]
  ret2    <- tmp[["coef", exact=TRUE]]
  rem.obj <- tmp$variables.removed
  if (!length(ret)) {
    return(list(ModelSummary=ret, Effects=ret2, Warnings=rem.obj))
  }

  # Add on some additional cols to the fit df
  ret[, "cohort"] <- op$cohort
  ret[, "spec"]   <- modeldata$modelspec
  ret[, "model"]  <- modeldata$modlabel
  tmp <- modeldata[["acovs", exact=TRUE]]
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
  ret  <- runModel.setReturnDF(ret, metabdata)
  ret2 <- runModel.setReturnDF(ret2, metabdata)

  list(ModelSummary=ret, Effects=ret2, Warnings=rem.obj)

} # END: runModel.main

runModel.defRetObj <- function(model, dmatCols0) {

  if (model == "pcorr") {
    ret <- runModel.defRetObj.pcor(dmatCols0)
  } else {
    ret <- runModel.defRetObj.glm(dmatCols0)
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
    ret <- (y >= 0) & (y <= 1) 
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
    #   then we ned to get the new subject order
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
    ret <- runModel.calcCorr(x, y, expVars, op$cor.method)
  } else {
    ret <- runModel.callGLM(x, y, op$famFun)
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
  isfactor     <- newmodeldata$isfactor
  k            <- 0
  k1           <- 0
  anyFactor    <- any(isfactor[ccovs])
  nruns        <- nrcovs*nccovs
  subOrder     <- newmodeldata$designSubOrder
  dmatCols0    <- newmodeldata$designMatCols  
  varMap       <- newmodeldata$varMap
  checkDesign  <- op$check.design
  family       <- op$family
  rem.obj      <- newmodeldata$variables.removed
  DEBUG        <- op$DEBUG
  if (DEBUG) print(rem.obj)

  # Get the maximum number of rows in the return object for effects
  N <- nrcovs*sum(newmodeldata$nlevels)

  # Get a default summary object when model fails 
  defObj <- runModel.defRetObj(op$model, dmatCols0)

  # objects to store results
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
    
  # Loop over each exposure in the outer loop
  for (j in 1:nccovs) {
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
      rem.obj <- runModel.addRemVars(rem.obj, ccovj, "colvars", 
                      "too few subjects", varMap=varMap)
      next
    }

    # Design matrix checks, returns design columns kept 
    if (checkDesign) {
      tmp   <- runModel.checkDesignWithExp(x[subset0, , drop=FALSE], op, ccovNames,
                                           varMap=varMap)
      dcols <- tmp[["cols", exact=TRUE]]
      if (!length(dcols)) {
        rem.obj <- runModel.addRemVars(rem.obj, ccovj, "colvars", tmp$msg, varMap=varMap)
        next
      }
    }

    # Loop over each outcome
    for (i in 1:nrcovs) {

      # Get the outcome
      rcovi <- rcovs[i]
      if (rcovi == ccovj) next
      tmp   <- as.numeric(newmodeldata$gdta[[rcovi]])  
      y     <- tmp[subOrder]
      
      # Determine the subset to use from the response
      subset <- runModel.getResponseSubs(y, family)

      # Get the final subset of subjects to use 
      subset <- subset0 & subset
      nsubs  <- sum(subset)
      fit    <- NULL
 
      if (nsubs >= minNsubs) {
        # Check design matrix if subjects were removed due to missing values
        if (checkDesign && (nsubs < n0)) {
          tmp    <- runModel.checkDesignWithExp(x[subset, , drop=FALSE], op, ccovNames,
                                                varMap=varMap)
          dcols  <- tmp[["cols", exact=TRUE]]
          fit    <- tmp$msg 
        } 
        if (length(dcols)) {
          fit  <- try(runModel.callFunc(x[subset, dcols, drop=FALSE], y[subset], 
                      ccovNames, op), silent=TRUE)
        }
      } else {
        fit <- "too few subjects"
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
      k              <- k2
    }
  }

  # Check for no results
  ret1 <- ret2 <- NULL
  if (k) {
    tmp  <- 1:k1
    ret1 <- data.frame(tmp, rname[runRows], cname[runRows], conv[tmp], fitMat[tmp, , drop=FALSE], 
                       msgVec[tmp], adjVec[tmp], remVec[tmp], stringsAsFactors=FALSE)
    colnames(ret1) <- c("run", "outcomespec", "exposurespec", "converged",
                       names(defObj$fit.stats), "message", "adjvars", "adjvars.removed")
    ret2 <- data.frame(runVec, rname, cname, coefMat, stringsAsFactors=FALSE)
    colnames(ret2) <- c("run", "outcomespec", "exposurespec", 
                      names(defObj$coef.stats))

    # Subset if needed
    if (k < N) ret2 <- ret2[1:k, , drop=FALSE]
    if (op$pcorrFlag) ret1$converged <- NULL
  }

  list(fit=ret1, coef=ret2, variables.removed=rem.obj)

} # END: runModel.runAllMetabs

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
  if (ncol(ret) < 2) stop("INTERNAL CODING ERROR")
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

runModel.getVarSep <- function() { ";" }

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
