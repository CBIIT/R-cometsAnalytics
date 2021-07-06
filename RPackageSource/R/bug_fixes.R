getMode_batch       <- function() {"Batch"}
getMode_interactive <- function() {"Interactive"}
getModelSummaryRunModeName <- function() {"spec"}
getAllMetabsName    <- function() {"All metabolites"}

checkForSameVars <- function(v1, v2) {

  # Variables should be "tolowered" at this point
  ret <- NULL
  n1  <- length(v1)
  if (n1 != length(v2)) stop("Vectors do not have the same length")
  v1  <- trimws(v1)
  v2  <- trimws(v2)
  tmp <- !is.na(v1) & !is.na(v2) & (nchar(v1) > 0) & (nchar(v2) > 0)
  tmp[is.na(tmp)] <- FALSE
  if (any(tmp)) {
    rows <- (1:n1)[tmp]
    for (row in rows) {
      vars1 <- trimws(unlist(strsplit(v1[row], " ", fixed=TRUE)))
      vars2 <- trimws(unlist(strsplit(v2[row], " ", fixed=TRUE)))
      tmp   <- intersect(vars1, vars2)
      if (length(tmp)) ret <- unique(c(ret, tmp))
    }
  }

  ret

} # END: checkForSameVars

getCovNames_allMetabs <- function(varString, allmetabs, varMap) {

  varString   <- trimws(tolower(varString))
  allmetabStr <- tolower(getAllMetabsName())
  allFlag     <- grepl(allmetabStr, varString, fixed=TRUE)
  if (allFlag) varString <- gsub(allmetabStr, "", varString, fixed=TRUE)
  vars        <- trimws(as.vector(strsplit(varString, " ")[[1]]))
  if (length(vars)) {
    tmp  <- vars != ""
    vars <- vars[tmp] 
  }
  if (allFlag) vars <- c(vars, allmetabs)
  covs <- runModel.getNewVarName(unique(vars), varMap)

  covs

} # END: getCovNames_allMetabs

runModel.checkFor1Val <- function(data, vars, nvals=2, min.nsubs=25) {

  n    <- length(vars)
  ret  <- rep(FALSE, n)
  ret2 <- rep(FALSE, n)
  for (i in 1:n) {
    vec <- data[, vars[i], drop=TRUE]
    tmp <- !is.na(vec)
    if (length(unique(vec[tmp])) < nvals) ret[i] <- TRUE
    ret2[i] <- sum(tmp) < min.nsubs
  }
  rem  <- ret | ret2
  
  list(rem=rem, oneVal=ret, nsubs=ret2)

} # END: runModel.checkFor1Val

runModel.updateFor1Val <- function(obj, vars, vars.type, rem.obj, varMap=NULL) {

  # Remove vars that have too few non-missing unique values
  rem <- obj$rem
  if (!any(rem)) return(list(vars=vars, rem.obj=rem.obj))

  oneVal <- obj$oneVal
  miss   <- obj$nsubs

  if (any(miss)) {
    rem.obj <- runModel.addRemVars(rem.obj, vars[miss], vars.type, 
                          "too many missing values", varMap=varMap)
  }
  
  # Do not repeat the same variable
  oneVal <- oneVal & !miss
  if (any(oneVal)) {
    rem.obj <- runModel.addRemVars(rem.obj, vars[oneVal], vars.type, 
                          "too few unique non-missing values", varMap=varMap)
  }
  
  vars <- vars[!rem]

  return(list(vars=vars, rem.obj=rem.obj))
 
} # END: runModel.updateFor1Val

runModel.checkDesignMatCols <- function(dmat, op, rem.obj=NULL, varMap=NULL, 
                               varSet="design matrix") {

  # First column of dmat will always be an intercept

  if (ncol(dmat) < 2) return(list(designMat=dmat, rem.obj=rem.obj))
  method <- op$check.cor.method

  # Remove linearly dependent cols
  rem <- caret::findLinearCombos(dmat)$remove
  if (length(rem)) {
    tmp     <- colnames(dmat)
    rem.obj <- runModel.addRemVars(rem.obj, tmp[rem], varSet, "linearly dependent",
                                   varMap=varMap)
    dmat    <- dmat[, -rem, drop=FALSE]
  }

  # Check for correlated predictors
  corMat     <- NULL
  cor.cutoff <- op$check.cor.cutoff
  if ((ncol(dmat) > 2) && (cor.cutoff > 0)) { # Need at least 2 non-intercept columns
    corMat <- stats::cor(dmat[, -1, drop=FALSE], method=method)
    rem    <- caret::findCorrelation(corMat, cutoff=cor.cutoff)
    if (length(rem)) {
      tmp     <- colnames(dmat)[-1]
      rem.obj <- runModel.addRemVars(rem.obj, tmp[rem], varSet, 
                        "correlated with another predictor", varMap=varMap)
      dmat    <- dmat[, -(rem+1), drop=FALSE]
      corMat  <- NULL
    }
  }

  # check for ill conditioned square matrix for cor 
  if ((ncol(dmat) > 2) && (op$check.illCond)) {
    if (is.null(corMat)) corMat <- stats::cor(dmat[, -1, drop=FALSE], method=method)
    rem <- subselect::trim.matrix(corMat)
    rem <- rem$names.discarded
    if (length(rem)) {
      rem.obj <- runModel.addRemVars(rem.obj, rem, varSet, "ill conditioned",
                                     varMap=varMap)
      tmp     <- !(colnames(dmat) %in% rem)
      dmat    <- dmat[, tmp, drop=FALSE]
      corMat  <- NULL
    }
  }

  list(designMat=dmat, rem.obj=rem.obj)		 

} # END: runModel.checkDesignMatCols

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
             "type", "object") 
  cx    <- colnames(x)
  tmp   <- !(cx %in% cvars)
  vars  <- cx[tmp]
  tmp   <- vars %in% colnames(x)
  if (any(tmp)) {
    for (v in vars[tmp]) x[, v] <- as.numeric(x[, v])
  }

  # Remove adj column (duplicated information)
  #if ("adj" %in% colnames(x)) x$adj <- NULL

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

runModel.addRemVars <- function(obj, vars, type, reason, printWarning=1, varMap=NULL) {

  if (!is.null(varMap)) vars <- runModel.varMap(vars, varMap)

  #msg       <- paste("variable removed from ", type, ": ", reason, sep="")
  #c1        <- runModel.getWarningCol()
  #c2        <- runModel.getObjectCol()
  #c3        <- runModel.getMessageCol()
  #tmp       <- list()
  #tmp[[c1]] <- "WARNING"
  #tmp[[c2]] <- vars
  #tmp[[c3]] <- msg
  #obj       <- runmodel.addWarning(obj, tmp)

  if (printWarning) {
    str <- paste(vars, collapse=", ", sep="") 
    msg <- paste("The variable(s) ", str, " have been removed from ", 
                 type, " because of: ", reason, sep="")
    warning(msg)
  }

  NULL

} # END: runModel.addRemVars

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

runModel.checkModelDesign <- function (modeldata, metabdata) {

  if (is.null(modeldata)) stop("INTERNAL CODING ERROR in runModel.checkModelDesign")

  op     <- list(check.cor.method="spearman", check.cor.cutoff=0.97, check.illCond=TRUE,
                 colNamePrefix="...x")
  nunq   <- 2
  minN   <- 25
  varMap <- metabdata$dict_metabnames

  # Object for variables removed
  rem.obj <- NULL

  acovs  <- modeldata[["acovs", exact=TRUE]]
  ccovs  <- modeldata$ccovs
  rcovs  <- modeldata$rcovs
  gdta   <- modeldata$gdta

  # Set rownames to match subjects later
  rownames(gdta) <- paste(op$rowNamePrefix, 1:nrow(gdta), sep="")  

  # Remove adjustment vars that have to few non-missing unique values or too many missing
  if (length(acovs)) {
    tmp     <- runModel.checkFor1Val(gdta, acovs, nvals=nunq, min.nsubs=minN)
    tmp     <- runModel.updateFor1Val(tmp, acovs, "adjvars", rem.obj, varMap=varMap)
    acovs   <- tmp$vars
    rem.obj <- tmp$rem.obj
  }

  # Get the design matrix of adjusted variables and intercept
  dmat  <- runModel.designMat(gdta, acovs) 

  # Check that there is at least a minimum number of subjects
  if (nrow(dmat) < minN){
    stop(paste(modeldata$modlabel," has less than ", minN, " observations and will not be run.", sep=""))
  }

  # Remove linearly dependent cols
  if (ncol(dmat) > 1) {
    tmp     <- runModel.checkDesignMatCols(dmat, op, rem.obj=rem.obj, varMap=varMap,
                             varSet="adjvars")
    dmat    <- tmp$designMat
    rem.obj <- tmp$rem.obj
  }

  # Change column names of the design matrix to prevent names colliding later
  if (ncol(dmat) < 1) stop("INTERNAL CODING ERROR in runModel.checkModelDesign")
  dmatCols        <- colnames(dmat)
  colnames(dmat)  <- paste(op$colNamePrefix, 0:(ncol(dmat)-1), sep="")
  names(dmatCols) <- colnames(dmat)

  # If subjects were removed, then update gdta.
  # Possibly change this to keep a logical vector of subs to keep.
  if (nrow(dmat) < nrow(gdta)) {
    tmp  <- rownames(gdta) %in%  rownames(dmat)
    gdta <- gdta[tmp, , drop=FALSE]
  }  

  # Drop unused levels from factors
  gdta <- droplevels(gdta)

  # Remove exposures that have too few non-missing unique value or too many missing values
  tmp     <- runModel.checkFor1Val(gdta, ccovs, nvals=nunq, min.nsubs=minN)
  tmp     <- runModel.updateFor1Val(tmp, ccovs, "exposures", rem.obj, varMap=varMap)
  ccovs   <- tmp$vars
  rem.obj <- tmp$rem.obj
  if (!length(ccovs)) {
    stop(paste(modeldata$modlabel," has all exposure variables removed.", sep=""))
  }

  # Remove outcomes that have too few non-missing unique value
  tmp     <- runModel.checkFor1Val(gdta, rcovs, nvals=nunq, min.nsubs=minN)
  tmp     <- runModel.updateFor1Val(tmp, rcovs, "outcomes", rem.obj, varMap=varMap)
  rcovs   <- tmp$vars
  rem.obj <- tmp$rem.obj
  if (!length(rcovs)) {
    stop(paste(modeldata$modlabel," has all outcome variables removed.", sep=""))
  }

  # check if any of the exposures are factors
  ckfactor <- sapply(dplyr::select(gdta,dplyr::one_of(ccovs)),class)
  isfactor <- ckfactor %in% "factor"

  # Get then number of levels (minus 1) for categorical exposures, 1 if continuous
  nlevels <- runModel.getNlevels(gdta, ccovs, isfactor)

  # Get the maximum number of cols for any exposure variable
  maxncols <- max(nlevels)
  
  # Add additional columns onto design matrix for the exposures
  ncdmat <- ncol(dmat)
  tmp    <- matrix(data=NA, nrow=nrow(dmat), ncol=maxncols)
  colnames(tmp) <- paste("...e", 1:maxncols, sep="") # temporary names
  dmat   <- cbind(dmat, tmp)
  
  # Get the order of subjects in design matrix
  rows <- match(rownames(dmat), rownames(gdta))
  tmp  <- !is.na(rows)
  rows <- rows[tmp]

  modeldata$acovs                <- acovs
  modeldata$ccovs                <- ccovs
  modeldata$rcovs                <- rcovs
  modeldata$gdta                 <- gdta
  modeldata$designMat            <- dmat
  modeldata$isfactor             <- isfactor
  modeldata$designMatCols0       <- dmatCols
  modeldata$designSubOrder       <- rows
  modeldata$nlevels              <- nlevels
  modeldata$designMatExpStartCol <- ncdmat + 1
  modeldata$designMatExpCols     <- (ncdmat + 1):ncol(dmat)
  modeldata$gdta.nrow            <- nrow(gdta)
  modeldata$varMap               <- c(metabdata$dict_metabnames, dmatCols,
                                      modeldata[["varMap", exact=TRUE]])
  #modeldata[[wr.nm]]             <- rem.obj

  modeldata

} # END: runModel.checkModelDesign

getVariableClass <- function(data, vars) {

  ret <- sapply(data[, vars, drop=FALSE], class)
  ret

} # END: getVariableClass

areCategorical <- function(data, vars) {

  cls <- getVariableClass(data, vars)
  ret <- cls %in% c("factor", "character")
  ret 

} # END: areCategorical

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

runModel.getDesignSubs <- function(x) {
  
  # x: the design matrix. Must be a matrix, not a data frame
  # Get the rows of the design matrix x have no missing values
  tmp <- !is.finite(x)
  ret <- rowSums(tmp) == 0

  ret

} # END: runModel.getDesignSubs

runModel.getModelVectors <- function(modeldata, yvar) {

  subOrder <- modeldata$designSubOrder
  
  # response vector
  tmp      <- as.numeric(modeldata$gdta[[yvar]])  
  y        <- tmp[subOrder]

  # Determine the subset to use from the response
  subset <- is.finite(y)

  list(response=y, subset=subset)

} # END: runModel.getModelVectors 

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
    #msg  <- runModel.getRemMessage(rem, expVar, collapse=";", varMap=varMap)
    #msg  <- runModel.getExpRemFromDesign()
  }

  list(cols=cols, msg=msg, expVar=vars)

} # END: runModel.checkDesignWithExp

runModel.getVarStr <- function(vars, collapse=" ", default="") {

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

runModel.runAllMetabs <- function(newmodeldata, op) {

  # Minimum number of subjects required
  minNsubs     <- 2  # Original version did not check for min # of subs here
  rcovs        <- newmodeldata$rcovs
  nrcovs       <- length(rcovs)
  ccovs        <- newmodeldata$ccovs
  nccovs       <- length(ccovs)
  dmatCols0    <- newmodeldata$designMatCols  
  nms0         <- names(dmatCols0)
  adj0         <- runModel.getVarStr(dmatCols0[-1])
  varMap       <- newmodeldata$varMap
  checkDesign  <- op$check.design
  rem.obj      <- NULL
  N            <- nrcovs*sum(newmodeldata$nlevels)
  isfactor     <- newmodeldata$isfactor
  
  # Initialize objects to store results.
  cname    <- rep("", N)
  rname    <- rep("", N)
  a        <- 0
  b        <- 0
  pval     <- corr <- n <- rep(NA, N)
  adjVec   <- rep("", N)

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
      rem.obj <- runModel.addRemVars(NULL, ccovj, "exposures", 
                      "too few subjects", varMap=varMap)
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
      rem.obj <- runModel.addRemVars(NULL, ccovj, "exposures", 
                    "exposure removed from design matrix", varMap=varMap)
      next
    } 
    dcols0 <- dcols

    # Loop over each outcome
    for (i in 1:nrcovs) {

      rcovi <- rcovs[i]

      # Get the outcome and other vars needed
      tmp    <- runModel.getModelVectors(newmodeldata, rcovi)
      y      <- tmp$response
      subset <- tmp$subset

      # Get the final subset of subjects to use 
      subset <- subset0 & subset
      nsubs  <- sum(subset)
      fit    <- NULL
      ccovs2 <- ccovNames
      dcols  <- dcols0
      err    <- 1
 
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
          fit  <- try(runModel.pcor.test(x[subset, dcols, drop=FALSE], y[subset], 
                      ccovs2, "spearman"), silent=TRUE)
          if (!("try-error" %in% class(fit))) err <- 0
        }
      } 

      len        <- length(ccovs2)
      a          <- b + 1
      b          <- a + len - 1
      tmp        <- a:b
      cname[tmp] <- ccovs2
      rname[tmp] <- rcovi
      if (!err) {
        corr[tmp] <- fit$corr 
        pval[tmp] <- fit$pvalue
        n[tmp]    <- fit$nsubs
      } else {
        n[tmp]    <- nsubs
      }
      if (length(dcols)) {
        tmp2        <- dcols %in% nms0
        tmp2[1]     <- FALSE
        adjVec[tmp] <- runModel.getAdjVarStr(dcols[tmp2], dmatCols0)
      } else {
        adjVec[tmp] <- adj0
      }
      a <- b
    }
  }

  if (b && (b < N)) {
    corr   <- corr[1:b]
    pval   <- pval[1:b]
    n      <- n[1:b]
    cname  <- cname[1:b]
    rname  <- rname[1:b]
    adjVec <- adjVec[1:b] 
  }

  list(corr=corr, pval=pval, n=n, cname=cname, rname=rname, adj=adjVec)

} # END: runModel.runAllMetabs

runModel.calcCorr <- function(designMat, y, expVars) {

  method <- "spearman"

  # Use pcor.test for categorical exposure variables or if there are
  #   adjusted covariates
  nc <- ncol(designMat)
  if ((length(expVars) > 1) || (nc > 2)) {
    ret <- runModel.pcor.test(designMat, y, expVars, method) 
  } else {
    nsubs <- length(y)
    if (nc > 1) {
      corr  <- cor(designMat[, 2], y, method=method) 
      df    <- nsubs - 2
      test  <- sqrt(df)*corr/sqrt(1 - corr*corr)
      pval  <- 2*stats::pt(abs(test), df=df, lower.tail=FALSE)
      msg   <- ""
    } else {
      corr  <- NA
      pval  <- NA 
      msg   <- "exposure has been removed"
    }
    ret <- list(corr=corr, pvalue=pval, nsubs=nsubs, msg=msg)
  }

  ret

} # END: runModel.calcCorr 

runModel.getPcorData <- function(designMat, y, expVarsInd) {

  # Order the columns as outcome, exposure, adjustment vars (including exposure dummies)

  # Intercept column will be used for the outcome y
  designMat[, 1] <- y
  
  # Order the remaining cols 
  cols <- 1:ncol(designMat)
  cols <- cols[-c(1, expVarsInd)]
  ord  <- c(1, expVarsInd, cols) 
  x    <- designMat[, ord, drop=FALSE]

  x

} # END: runModel.getPcorData

# For a categorical exposure
runModel.pcor.test <- function(designMat, y, expVars, method) {

  n        <- length(expVars)
  nsub     <- length(y)
  nvec     <- rep(nsub, n)
  pvec     <- rep(NA, n)
  rvec     <- pvec
  msg      <- rep("", n)
  startCol <- ncol(designMat) - n
  if (startCol < 1) {
    stop("INTERNAL CODING ERROR in runModel.pcor.test")
  }

  # Loop over each dummy var
  for (i in 1:n) {
    # Get the pcor input data matrix
    x   <- runModel.getPcorData(designMat, y, startCol+i)
    fit <- try(ppcor::pcor(x, method=method), silent=TRUE)
    if ("try-error" %in% class(fit)) {
      msg[i]  <- "Error calling ppcor::pcor"
    } else {
      rvec[i] <- fit$estimate[1, 2]
      pvec[i] <- fit$p.value[1, 2] 
    }
  }

  list(corr=rvec, pvalue=pvec, nsubs=nvec, msg=msg)

} # END: runModel.pcor.test

# Common code for adding metabolite info
addMetabInfo <- function(corrlong, modeldata, metabdata) {

  # Defining global variables to pass Rcheck()
  metabid = uid_01 = biochemical = outmetname = outcomespec = exposuren =
    exposurep = metabolite_id = c()
  cohortvariable = vardefinition = varreference = outcome = outcome_uid =
    exposure = exposure_uid = c()
  metabolite_name = expmetname = exposurespec = c()
  adjname = adjvars = adj_uid = c()


  # patch in metabolite info for exposure or outcome by metabolite id  ------------------------
  # Add in metabolite information for outcome
  # look in metabolite metadata match by metabolite id
  corrlong$outcomespec <- as.character(lapply(corrlong$outcomespec, function(x) {
	myind <- which(names(metabdata$dict_metabnames)==x)
	if(length(myind==1)) {x=metabdata$dict_metabnames[myind]}
	return(x) }))

  corrlong <- dplyr::left_join(
    corrlong,
    dplyr::select(
      metabdata$metab,
      metabid,
      outcome_uid = uid_01,
      outmetname = biochemical
    ),
    by = c("outcomespec" = metabdata$metabId)
  ) %>%
    dplyr::mutate(outcome_uid = ifelse(!is.na(outcome_uid), outcome_uid, outcomespec)) %>%
    dplyr::mutate(outcome = ifelse(!is.na(outmetname), outmetname, outcomespec)) %>%
    dplyr::select(-outmetname)


  # Add in metabolite information and exposure labels:
  # look in metabolite metadata
  corrlong$exposurespec <- as.character(lapply(corrlong$exposurespec, function(x) {
        myind <- which(names(metabdata$dict_metabnames)==x)
        if(length(myind==1)) {x=metabdata$dict_metabnames[myind]}
        return(x) }))
  corrlong <- dplyr::left_join(
    corrlong,
    dplyr::select(
      metabdata$metab,
      metabid,
      exposure_uid = uid_01,
      expmetname = biochemical
    ),
    by = c("exposurespec" = metabdata$metabId)
  ) %>%
    #dplyr::mutate(exposure = ifelse(!is.na(expmetname), expmetname, modeldata$ccovs)) %>%
    dplyr::mutate(exposure = ifelse(!is.na(expmetname), expmetname, exposurespec)) %>%
    dplyr::mutate(exposure_uid = ifelse(!is.na(exposure_uid), exposure_uid, exposurespec)) %>%
    dplyr::select(-expmetname)

  # Add in metabolite info for adjusted variables
  # This commented-out block of code does not work correctly
  	#corrlong$adjvars <- corrlong$adjspec <- 
	#      as.character(lapply(corrlong$adjspec, function(x) {
  	#      myind <- which(names(metabdata$dict_metabnames)==x)
  	#      if(length(myind==1)) {x=metabdata$dict_metabnames[myind]}
  	#      return(x) }))

  	corrlong <- dplyr::left_join(
  	  corrlong,
  	  dplyr::select(
  	    metabdata$metab,
  	    metabid,
  	    adj_uid = uid_01,
  	    adjname = biochemical
  	  ),
  	  by = c("adjspec" = metabdata$metabId)
  	) %>%
  	  dplyr::mutate(adj = ifelse(!is.na(adjname), adjname, adjvars)) %>%
  	  dplyr::mutate(adj_uid = ifelse(!is.na(adj_uid), adj_uid, adjvars)) %>%
  	  dplyr::select(-adjname) 

  # patch in variable labels for better display and cohortvariables------------------------------------------
  # look in varmap
  vmap <-
    dplyr::select(metabdata$vmap, cohortvariable, vardefinition, varreference) %>%
    mutate(
      cohortvariable = tolower(cohortvariable),
      vardefinition = ifelse(
        regexpr("\\(", vardefinition) > -1,
        substr(vardefinition, 0, regexpr("\\(", vardefinition) - 1),
        vardefinition
      )
    )

  # get good labels for the display of outcome and exposure
  if (modeldata$modelspec == getMode_interactive()) {
    # fill in outcome vars from varmap if not a metabolite:
    if(length(suppressWarnings(grep(corrlong$outcomespec,vmap$cohortvariable)) != 0)) {
    	corrlong <-
    	  dplyr::left_join(corrlong, vmap, by = c("outcomespec" = "cohortvariable")) %>%
    	  dplyr::mutate(
    	    outcome_uid = ifelse(!is.na(varreference), varreference, outcomespec),
    	    outcome = ifelse(
    	      !is.na(outcome),
    	      outcome,
    	      ifelse(!is.na(vardefinition), vardefinition, outcomespec)
    	    )
    	  ) %>%
    	  dplyr::select(-vardefinition, -varreference)
    }

    # fill in exposure vars from varmap if not a metabolite:
    if(length(suppressWarnings(grep(corrlong$exposurespec,vmap$cohortvariable)) != 0)) {
    	corrlong <-
    	  dplyr::left_join(corrlong, vmap, by = c("exposurespec" = "cohortvariable")) %>%
    	  dplyr::mutate(
    	    exposure_uid = ifelse(!is.na(varreference), varreference, exposurespec),
    	    exposure = ifelse(!is.na(vardefinition), vardefinition, exposurespec)
    	  ) %>%
    	  dplyr::select(-vardefinition, -varreference)
       }
  }
  else if (modeldata$modelspec == getMode_batch()) {
    # fill in outcome vars from varmap if not a metabolite
    if(length(suppressWarnings(grep(corrlong$outcomespec,vmap$cohortvariable)) != 0)) {
    	corrlong <-
    	  dplyr::left_join(corrlong, vmap, by = c("outcomespec" = "varreference")) %>%
    	  dplyr::mutate(
    	    outcome_uid = ifelse(is.na(outcome_uid), outcomespec, outcome_uid),
    	    outcome = ifelse(
    	      !is.na(outcome),
    	      outcome,
    	      ifelse(!is.na(vardefinition), vardefinition, outcomespec)
    	    ),
    	    outcomespec = ifelse(!is.na(cohortvariable), cohortvariable, outcomespec)
    	  ) %>%
    	  dplyr::select(-vardefinition, -cohortvariable)
    }

    # fill in exposure vars from varmap if not a metabolite:
    if(length(suppressWarnings(grep(corrlong$exposurespec,vmap$cohortvariable)) != 0)) {
    	corrlong <-
    	  dplyr::left_join(corrlong, vmap, by = c("exposurespec" = "varreference")) %>%
    	  dplyr::mutate(
    	    exposure_uid = exposurespec,
    	    exposure = ifelse(
    	      !is.na(exposure),
    	      exposure,
    	      ifelse(!is.na(vardefinition), vardefinition, exposurespec)
    	    ),
    	    exposure = ifelse(!is.na(vardefinition), vardefinition, exposurespec),
    	    exposurespec = ifelse(!is.na(cohortvariable), cohortvariable, exposurespec)
    	  ) %>%
    	  dplyr::select(-vardefinition, -cohortvariable)
   }
  }

  corrlong

} # END: addMetabInfo



