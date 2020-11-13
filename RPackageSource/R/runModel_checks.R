
# Function to check an argument 
runModel.check.str <- function(obj, valid, parm) {

  # obj:   A character string (length 1)
  # valid: Character vector of valid values
  # parm:  The name of the argument being checked

  errFlag <- 0
 
  # Check for errors
  if (!isString(obj)) errFlag <- 1 
  obj <- trimws(obj)
  if (!(obj %in% valid)) errFlag <- 1

  if (errFlag) {
    msg <- paste(valid, collapse=", ")
    msg <- paste("ERROR: ", parm, " must be one of ", msg, sep="")
    stop(msg)
  }

  obj

} # END: runModel.check.str

runModel.check.model <- function(obj) {

  valid <- getValidModelNames()
  obj   <- runModel.check.str(obj, valid, "model") 
  
  obj

} # END: runModel.check.model

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

  # Check for zero-variance predictors (e.g. a stratified group that only has 1 value)
  freqCut <- op$check.nearZeroVar.freqCut
  if ((ncol(dmat) > 1) && (freqCut > 0)) { # Col 1 is intercept
    rem <- caret::nearZeroVar(dmat[, -1, drop=FALSE], freqCut=freqCut)
    if (length(rem)) {
      tmp     <- colnames(dmat)[-1]
      rem.obj <- runModel.addRemVars(rem.obj, tmp[rem], varSet, "near zero variance",
                                     varMap=varMap)
      dmat    <- dmat[, -(rem+1), drop=FALSE]
    }
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

runModel.checkModelDesign <- function (modeldata, metabdata, op) {

  if (is.null(modeldata)) stop("Please make sure that modeldata is defined")

  nunq   <- op$check.n.unique.vals
  minN   <- op$check.nsubjects
  varMap <- metabdata$dict_metabnames
  wr.nm  <- runModel.getWarningsListName()

  # Object for variables removed
  rem.obj <- modeldata[[wr.nm, exact=TRUE]]

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
  tmp     <- runModel.updateFor1Val(tmp, ccovs, "colvars", rem.obj, varMap=varMap)
  ccovs   <- tmp$vars
  rem.obj <- tmp$rem.obj
  if (!length(ccovs)) {
    stop(paste(modeldata$modlabel," has all exposure variables removed.", sep=""))
  }

  # Remove outcomes that have too few non-missing unique value
  tmp     <- runModel.checkFor1Val(gdta, rcovs, nvals=nunq, min.nsubs=minN)
  tmp     <- runModel.updateFor1Val(tmp, rcovs, "rowvars", rem.obj, varMap=varMap)
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
  modeldata[[wr.nm]]             <- rem.obj

  modeldata

} # END: runModel.checkModelDesign


