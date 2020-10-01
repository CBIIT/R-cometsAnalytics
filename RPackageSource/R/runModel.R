
runModel <- function(modeldata, metabdata, cohort="", 
                     model="pcorr", family="gaussian", link="", ...) {

  # model    "pcorr" or "glm"
  # family   String for the family to use
  # link     String for the link function. The default will be to use
  #          the canonical link.
  # ...      Additional arguments

  model <- runModel.check.model(model)
  if (model == "pcorr") {
    ret <- runCorr(modeldata, metabdata, cohort=cohort)
  } else {
    family <- runModel.check.family(family)
    link   <- runModel.check.link(link, family)
    ret    <- runModel.runGLM(modeldata, metabdata, cohort=cohort, 
                              family=family, link=link)
  }

  ret

} # END: runModel 

runModel.adjVars <- function(adjvars, metabdata) {

  if (!length(adjvars)) return(adjvars)

  metabs     <- metabdata$dict_metabnames
  metabs_new <- names(metabdata$dict_metabnames)
  rows       <- match(adjvars, metabs_new)
  tmp        <- !is.na(rows)
  if (any(tmp)) {
    rows         <- rows[tmp]
    adjvars[tmp] <- metabs[rows]
  }

  adjvars

} # END: runModel.adjVars

runModel.runGLM <- function(modeldata, metabdata, cohort=cohort, 
                            family=family, link=link) {

  famFun <- runModel.getFamilyFun(family, link)
  op     <- NULL

  # Get the initial design matrix and other objects
  modeldata <- runModel.checkModelDesign(modeldata, op=op)
  if (!length(names(modeldata))) return(modeldata)

  # Check if any adjustment vars are metabolites
  modeldata$designMatCols0 <- runModel.adjVars(modeldata$designMatCols0, metabdata)

  # Run all metabolite and exposures
  tmp  <- runModel.runAllMetabs(modeldata, famFun, family) 
  ret  <- tmp$fit
  ret2 <- tmp$coef

  # Add on some additional cols to the fit df
  ret[, "cohort"] <- cohort
  ret[, "spec"]   <- modeldata$modelspec
  ret[, "model"]  <- modeldata$modlabel
  tmp <- modeldata[["acovs", exact=TRUE]]
  if (length(tmp)) {
    adjspec <- paste(tmp, collapse = " ")
  } else {
    adjspec <- "None"
  }
  ret[, "adjspec"] <- adjspec
  ret <- fixData(ret)
  #tmp <- ret[, "adjvars", drop=TRUE]

  # Add metabolite info, fix bug in this function call with adjvars, adjspec
  ret <- addMetabInfo(ret, modeldata, metabdata)
  #ret[, "adjvars"] <- tmp

  # Let run, cohort, spec, model column be the first columns
  ret <- orderVars(ret, c("run", "cohort", "spec", "model"))

  # Change outcomespec to correct name
  rows <- match(ret2[, "run"], ret[, "run"])
  tmp  <- !is.na(rows)
  rows <- rows[tmp]
  ret2[tmp, "outcomespec"] <- ret[rows, "outcomespec"]

  list(fit=ret, coef=ret2)

} # END: runModel.runGLM 

runModel.callGLM <- function(x, y, famFun) {

  fit <- glm.fit(x, y, family=famFun)
  class(fit) <- c("glm", "lm")
  
  fit

} # END: runModel.callGLM

runModel.getErrorMsg <- function(obj) {

  ret <- paste(as.character(obj), collapse=" ", sep="")
  ret <- gsub("\n", " ", ret, fixed=TRUE)
  ret <- gsub("\r", " ", ret, fixed=TRUE)

  ret

} # END: runModel.getErrorMsg

runModel.tidyGLM <- function(fit, expVars, defObj, dmatCols0) {

  if (!length(fit)) {
    ret     <- defObj
    ret$msg <- "too few subjects"
  } else if ("try-error" %in% class(fit)) {
    ret     <- defObj
    ret$msg <- runModel.getErrorMsg(fit)
  } else {
    obj2 <- as.numeric(glance(fit))
    conv <- fit$converged
    obj  <- tidy(fit)
    msg  <- ""

    # Get the rows and columns we need and convert to a named vector
    terms <- obj[, 1, drop=TRUE]
    tmp   <- terms %in% expVars
    m     <- sum(tmp)
    if (m) {    
      terms  <- terms[tmp]
      obj1   <- as.matrix(obj[tmp, ]) 
    } else {
      # No exposure names found in fitted object, return default 
      obj1 <- defObj$coef.stats
      msg  <- "exposure could not be estimated"
    }
    vec    <- fit$coefficients
    nms    <- names(vec)
    tmp    <- is.finite(vec) & (nms %in% names(dmatCols0))
    tmp[1] <- FALSE
    if (any(tmp)) {
      nms  <- nms[tmp]
      orig <- dmatCols0[nms]
      adj  <- paste(orig, collapse=" ", sep="")
    } else {
      adj  <- "" 
    }
    
    ret  <- list(converged=conv, coef.stats=obj1, fit.stats=obj2, 
                 msg=msg, adj=adj)  
  } 
  ret

} # END: runModel.tidyGLM

runModel.defRetObj <- function(dmatCols0) {

  vec               <- c("term", "estimate", "std.error", "statistic", "p.value")
  coef.names        <- vec
  fit.names         <- c("null.deviance", "df.null", "logLik", "AIC",
                         "BIC", "deviance", "df.residual", "nobs")
  coef.stats        <- matrix(data=NA, nrow=1, ncol=length(coef.names))
  names(coef.stats) <- coef.names
  fit.stats         <- rep(NA, length(fit.names))
  names(fit.stats)  <- fit.names
  adj               <- dmatCols0[-1]
  if (length(adj)) {
    adj <- paste(adj, collapse=" ", sep="")

  } else {
    adj <- ""
  }

  list(converged=FALSE, coef.stats=coef.stats, fit.stats=fit.stats, 
       msg="", adj=adj)

} # END: runModel.defRetObj

runModel.getDesignSubs <- function(x) {
  
  # x: the design matrix. Must be a matrix, not a data frame
  # Get the rows of the design matrix x have no missing values
  tmp <- !is.finite(x)
  ret <- rowSums(tmp) == 0

  ret

} # END: runModel.getUseSubjects

runModel.getResponseSubs <- function(y, family) {

  if (family == "gaussian") {
    ret <- rep(TRUE, length(y))
  } else if (family == c("binomial", "quasibinomial")) {
    ret <- (y >= 0) & (y <= 1) 
  } else if (family %in% c("Gamma", "inverse.gaussian")) {
    ret <- y > 0
  } else if (family == "poisson") {
    ret <- ((y %% 1) == 0) & (y >= 0)
  } else if (family == "quasipoisson") {
    ret <- y > 0
  } else {
    ret <- rep(TRUE, length(y))
  }
  ret[is.na(ret)] <- FALSE 

  ret

} # END: runModel.getResponseSubs

runModel.updateDesignMat <- function(modeldata, expVar, catvar) {

  eStartCol  <- modeldata$designMatExpStartCol
  designMat  <- modeldata$designMat
  subOrder   <- modeldata$designSubOrder
  designCols <- colnames(designMat)

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
      subOrder <- match(rownames(designMat), rownames(mat))
      subOrder <- subOrder[!is.na(subOrder)]
    }   
    designMat[, ids] <- mat[subOrder, , drop=FALSE]
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

runModel.runAllMetabs <- function(newmodeldata, famFun, family) {

  # Minimum number of subjects to call glm
  minNsubs    <- 1
  rcovs       <- newmodeldata$rcovs
  nrcovs      <- length(rcovs)
  ccovs       <- newmodeldata$ccovs
  nccovs      <- length(ccovs)
  isfactor    <- newmodeldata$isfactor
  allColNames <- colnames(newmodeldata$gdta)
  k           <- 0
  k1          <- 0
  anyFactor   <- any(isfactor[ccovs])
  nruns       <- nrcovs*nccovs
  subOrder    <- newmodeldata$designSubOrder
  dmatCols0   <- newmodeldata$designMatCols0

  # Get the maximum number of rows in the return objects
  N <- nrcovs*sum(newmodeldata$nlevels)

  # Get a default summary object when glm fails 
  defObj <- runModel.defRetObj(dmatCols0)

  # objects to store results
  conv     <- rep(FALSE, nruns)
  rname    <- rep("", N)
  cname    <- rep("", N)
  coefMat  <- matrix(data=NA, nrow=N, ncol=ncol(defObj$coef.stats))
  fitMat   <- matrix(data=NA, nrow=nruns, ncol=length(defObj$fit.stats))
  runVec   <- rep(0, N)
  runRows  <- rep(0, nruns)
  msgVec   <- rep("", nruns)
  adjVec   <- rep("", nruns)

  # Loop over each exposure in the outer loop
  for (j in 1:nccovs) {
    ccovj <- ccovs[j]

    # Update the design matrix for this exposure var 
    tmp       <- runModel.updateDesignMat(newmodeldata, ccovj, isfactor[j])
    x         <- tmp$designMat
    ccovNames <- tmp$expNames
    tmp       <- NULL

    # Get the initial subset of subjects to use
    subset0 <- runModel.getDesignSubs(x)

    # Loop over each outcome
    for (i in 1:nrcovs) {

      # Get the outcome
      rcovi <- rcovs[i]
      tmp   <- as.numeric(newmodeldata$gdta[[rcovi]])  
      y     <- tmp[subOrder]
      
      # Determine the subset to use from the response
      subset <- runModel.getResponseSubs(y, family)

      # Get the final subset of subjects to use 
      subset <- subset0 & subset
      nsubs  <- sum(subset)
 
      if (nsubs >= minNsubs) {
        fit  <- try(runModel.callGLM(x[subset, , drop=FALSE], y[subset], 
                    famFun), silent=TRUE)
      } else {
        fit  <- NULL
      } 
   
      # Get the results to save
      tmp <- runModel.tidyGLM(fit, ccovNames, defObj, dmatCols0)

      # Save results
      k              <- k + 1
      k1             <- k1 + 1
      coef           <- tmp$coef.stats
      k2             <- k + nrow(coef) - 1
      vec            <- k:k2
      runRows[k1]    <- k
      conv[k1]       <- tmp$converged
      rname[vec]     <- rcovi
      cname[vec]     <- ccovj
      coefMat[vec, ] <- coef
      runVec[vec]    <- k1
      fitMat[k1,]    <- tmp$fit.stats
      msgVec[k1]     <- tmp$msg
      adjVec[k1]     <- tmp$adj
      k              <- k2
    }
  }

  ret1 <- data.frame(1:k1, rname[runRows], cname[runRows], conv, fitMat, 
                     msgVec, adjVec, stringsAsFactors=FALSE)
  colnames(ret1) <- c("run", "outcomespec", "exposurespec", "converged",
                     names(defObj$fit.stats), "message", "adjvars")
  ret2 <- data.frame(runVec, rname, cname, coefMat, stringsAsFactors=FALSE)
  colnames(ret2) <- c("run", "outcomespec", "exposurespec", 
                      names(defObj$coef.stats))

  # Subset if needed
  if (k < N) ret2 <- ret2[1:k, , drop=FALSE]

  list(fit=ret1, coef=ret2)

} # END: runModel.runAllMetabs

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

  valid <- c("pcorr", "glm")
  obj   <- runModel.check.str(obj, valid, "model") 
  
  obj

} # END: runModel.check.model

runModel.check.family <- function(obj) {

  valid <- c("binomial", "gaussian", "Gamma", "inverse.gaussian",
             "poisson", "quasi", "quasibinomial", "quasipoisson")
  obj   <- runModel.check.str(obj, valid, "family") 

  obj

} # END: runModel.check.family

# Get the valid link functions for a family. From the R doc on "family"
runModel.get.links <- function(family) {

  if (family == "gaussian") {
    ret <- c("identity", "log", "inverse")
  } else if (family == "binomial") {
    ret <- c("logit", "probit", "cauchit", "log", "cloglog")
  } else if (family == "Gamma") {
    ret <- c("inverse", "identity", "log")
  } else if (family == "poisson") {
    ret <- c("log", "identity", "sqrt")
  } else if (family == "inverse.gaussian") {
    ret <- c("1/mu^2", "inverse", "identity", "log")
  } else {
    ret <- c("logit", "probit", "cloglog", "identity", "inverse", 
             "log", "1/mu^2", "sqrt")
  }

  ret

} # END: runModel.get.links

# Get the canonical link function for a family
runModel.get.canonical <- function(family) {

  # family mustbe checked before this function is called

  if (family == "gaussian") {
    ret <- "identity"
  } else if (family == "binomial") {
    ret <- "logit"
  } else if (family == "Gamma") {
    ret <- "inverse"
  } else if (family == "poisson") {
    ret <- "log"
  } else if (family == "inverse.gaussian") {
    ret <- "1/mu^2"
  } else if (family == "quasi") {
    ret <- "identity"
  } else if (family == "quasibinomial") {
    ret <- "logit"
  } else if (family == "quasipoisson") {
    ret <- "log"
  } else {
    ret <- "identity"
  }

  ret

} # END: runModel.get.canonical

runModel.check.link <- function(obj, family) {

  # family must be checked before this function is called

  if (isString(obj) && (nchar(trimws(obj)) < 1)) {
    obj <- runModel.get.canonical(family) 
  }
  valid <- runModel.get.links(family)
  obj   <- runModel.check.str(obj, valid, "link") 

  obj

} # END: runModel.check.link

runModel.getFamilyFun <- function(family, link) {

  str <- paste(family, "(link='", link, "')", sep="")
  ret <- eval(parse(text=str))

  ret

} # END: runModel.getFamilyFun


runModel.designMat <- function(data, vars) {

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

runModel.checkFor1Val <- function(data, vars, nvals=2) {

  n   <- length(vars)
  ret <- rep(FALSE, n)
  for (i in 1:n) {
    vec <- data[, vars[i], drop=TRUE]
    tmp <- !is.na(vec)
    if (length(unique(vec[tmp])) < nvals) ret[i] <- TRUE
  }
  
  ret

} # END: runModel.checkFor1Val

runModel.addRemVars <- function(obj, vars, type, reason) {

  mat <- cbind(vars, type, reason)
  colnames(mat) <- c("Variable", "Type", "Reason")
  if (length(obj)) {
    obj <- rbind(obj, mat)
  } else {
    obj <- mat
  }

  obj

} # END: runModel.addRemVars

runModel.checkModelDesign <- function (modeldata, op=NULL) {

  if (is.null(modeldata)) stop("Please make sure that modeldata is defined")
  op <- default.list(op, 
                     c("min.nsubjects", "colNamePrefix", "rowNamePrefix", 
                       "min.n.unique.vals"), 
                     list(25, "...x", "r",
                          2))

  errObj <- data.frame()
  attr(errObj,"ptime") <- "Processing time: 0 sec"
  nunq <- op$min.n.unique.vals

  # Object for variables removed
  rem.obj <- NULL

  acovs  <- modeldata[["acovs", exact=TRUE]]
  nacovs <- length(acovs) 
  ccovs  <- modeldata$ccovs
  sFlag  <- !is.null(modeldata[["scovs", exact=TRUE]])
  rcovs  <- modeldata$rcovs

  # Set rownames to match subjects later
  gdta   <- modeldata$gdta
  rownames(gdta) <- paste(op$rowNamePrefix, 1:nrow(gdta), sep="")  

  # Remove adjustment vars that have to few non-missing unique values
  rem   <- runModel.checkFor1Val(gdta, acovs, nvals=nunq)
  if (any(rem)) {
    rem.obj <- runModel.addRemVars(rem.obj, acovs[rem], "adjvars", "too few unique non-missing values")
    acovs   <- acovs[!rem]
  }

  # Get the design matrix of adjusted variables and intercept
  dmat  <- runModel.designMat(gdta, acovs) 

  # Check that there is at least a minimum number of subjects
  if (nrow(dmat) < op$min.nsubjects){
    if (sFlag) return(errObj)
    stop(paste(modeldata$modlabel," has less than ", op$min.nsubjects, " observations and will not be run.", sep=""))
  }

  # Remove linearly dependent cols
  rem <- caret::findLinearCombos(dmat)$remove
  if (length(rem)) {
    tmp     <- colnames(dmat)
    rem.obj <- runModel.addRemVars(rem.obj, tmp[rem], "adjvars", "linearly dependent")
    dmat    <- dmat[, -rem, drop=FALSE]
  }

  # Change column names of the design matrix to prevent names colliding later
  dmatCols <- colnames(dmat)
  colnames(dmat)  <- paste(op$colNamePrefix, 0:(ncol(dmat)-1), sep="")
  names(dmatCols) <- colnames(dmat)

  # If subjects were removed, then update gdta
  if (nrow(dmat) < nrow(gdta)) {
    tmp  <- rownames(gdta) %in%  rownames(dmat)
    gdta <- gdta[tmp, , drop=FALSE]
  }  

  # Remove exposures that have too few non-missing unique value
  rem   <- runModel.checkFor1Val(gdta, ccovs, nvals=nunq)
  if (any(rem)) {
    rem.obj <- runModel.addRemVars(rem.obj, ccovs[rem], "colvars", "too few unique non-missing values")
    ccovs   <- ccovs[!rem]
  }
  if (!length(ccovs)) {
    if (sFlag) return(errObj)
    stop(paste(modeldata$modlabel," has all exposure variables with too few unique values.", sep=""))
  }

  # Remove outcomes that have too few non-missing unique value
  rem   <- runModel.checkFor1Val(gdta, rcovs, nvals=nunq)
  if (any(rem)) {
    rem.obj <- runModel.addRemVars(rem.obj, rcovs[rem], "rowvars", "too few unique non-missing values")
    rcovs   <- rcovs[!rem]
  }
  if (!length(rcovs)) {
    if (sFlag) return(errObj)
    stop(paste(modeldata$modlabel," has all outcome variables with too few unique values.", sep=""))
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
  modeldata$gdta.nrow            <- nrow(gdta)
  modeldata$variables.removed    <- rem.obj

  modeldata

} # END: runModel.checkModelDesign


# Function to check that an object is a string
isString <- function(obj) {

  if ((length(obj) == 1) && is.character(obj)) {
    ret <- TRUE
  } else {
    ret <- FALSE
  }

  ret

} # END: isString

# Function to order columns in a matrix or data frame
orderVars <- function(data, order) {

  # data     matrix or data frame with column names
  # order    Character vector

  if (ncol(data) == 1) return(data)
  cnames <- colnames(data)
  temp   <- order %in% cnames
  order  <- order[temp]
  temp   <- !(cnames %in% order)
  if (any(temp)) order <- c(order, cnames[temp])
    
  data <- data[, order, drop=FALSE]
  data

} # END: orderVars

# Function to assign a default value to an element in a list
default.list <- function(inList, names, default, error=NULL,
                         checkList=NULL) {

  # inList      List
  # names       Vector of names of items in inList
  # default     List of default values to assign if a name is not found
  #             The order of default must be the same as in names.
  # error       Vector of TRUE/FALSE if it is an error not to have the
  #             name in the list. 
  #             The default is NULL
  # checkList   List of valid values for each name.
  #             Use NA to skip a list element.
  #             The default is NULL

  n1 <- length(names)
  n2 <- length(default)
  if (n1 != n2) stop("ERROR: in calling default.list")

  if (is.null(error)) {
    error <- rep(0, times=n1)
  } else if (n1 != length(error)) {
    stop("ERROR: in calling default.list")
  }

  if (!is.null(checkList)) {
    if (n1 != length(checkList)) stop("ERROR: in calling default.list")
    checkFlag <- 1
  } else {
    checkFlag <- 0
  } 

  if (is.null(inList)) inList <- list()

  listNames <- names(inList)
  for (i in 1:n1) {
    if (!(names[i] %in% listNames)) {
      if (!error[i]) {
        inList[[names[i]]] <- default[[i]]
      } else {
        temp <- paste("ERROR: the name ", names[i], " was not found", sep="")
        stop(temp)
      }
    } else if (checkFlag) {
      temp <- checkList[[i]]
      if (!all(is.na(temp))) {
        if (!all(inList[[names[i]]] %in% checkList[[i]])) {
          temp <- paste("ERROR: the name '", names[i], 
                      "' has an invalid value", sep="")
          stop(temp)
        }
      }
    }
  }

  inList

} # END: default.list

