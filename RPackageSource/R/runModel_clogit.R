#' A list of 2:
#' \itemize{
#' \item{\code{method}}{ One of: "exact", "approximate", "efron", "breslow".
#'             The default is "exact".}
#' \item{\code{weights}}{ A variable name to specify weights. The default is NULL.}
#' }
#'
#' @name clogit.options
#' @title options list for clogit
#' 
#' @examples 
#' model.options <- list(method="efron", weights="weightVarInData")
NULL

runModel.getDefaultClogitOptions <- function() {

  ret   <- list(method="exact", weights=NULL)
  ops.c <- c("method", "weights")

  list(default=ret, ops.character=ops.c, ops.logical=NULL)

} # END: runModel.getDefaultClogitOptions

runModel.convertClogitOp <- function(opName, opValue) {

  ret <- opValue
  ret

} # END: runModel.convertClogitOp

runModel.parseAndCheckClogitOps <- function(str, modeldata) {

  tmp   <- runModel.getDefaultClogitOptions() 
  def   <- tmp$default
  valid <- names(def)

  if (!length(str)) return(def)
  if (!isString(str)) stop("str must be a string")

  str    <- setupOpStr(str)
  if (!nchar(str)) return(def)

  strVec <- parseStr(str, sep=getOpStrSep()) 
  n      <- length(strVec)
  
  ret    <- list()
  # Loop over each element of strVec, each element should be
  #   of the form "op=value" 
  eq <- getOpStrEq()
  for (i in 1:n) {
    tmp   <- getOptionNameAndValue(strVec[i], sep=eq)
    name  <- tmp$name
    value <- tmp$value
    if (!(name %in% valid)) stop(paste("ERROR: ", strVec[i], " is not valid", sep=""))
    if (!length(value)) stop(paste("ERROR: ", strVec[i], " is not valid", sep=""))
    
    # value must be converted to the correct type
    value <- runModel.convertClogitOp(name, value)

    ret[[name]] <- value
  }

  # Check the values
  ret <- try(runModel.checkClogitOpList(ret, modeldata, name=getModelOpsName()), silent=TRUE)
  if ("try-error" %in% class(ret)) {
    print(ret)
    msg <- paste("ERROR with options, make sure options are separated by a ", 
                 getOpStrSep(), sep="")
    stop(msg)
  }

  ret

} # END: runModel.parseAndCheckClogitOps

runModel.checkClogitOpList <- function(op, modeldata, name="clogit.options") {

  n   <- length(op)
  if (n && !is.list(op)) stop(paste("ERROR: ", name, " must be a list", sep=""))
  tmp <- runModel.getDefaultClogitOptions()
  
  def   <- tmp$default
  valid <- names(def)
  ops.c <- tmp$ops.character 

  # Check the names and values
  if (n) {
    checkOptionListNames(op, valid, name)
    nms <- names(op)

    if (length(modeldata)) {
      data   <- modeldata$gdta
      varMap <- modeldata$varMap
    } else {
      data   <- NULL
      varMap <- NULL
    }

    for (i in 1:n) {
      nm  <- nms[i]
      val <- op[[i]]
      if (is.null(val)) next
      
      if (nm %in% c("weights")) {
        tmp <- try(eval(parse(text=paste("checkClogitOp_", nm, "('", val, "', data, varMap)", sep=""))),
                   silent=TRUE)
      } else if (nm %in% ops.c) {
        tmp <- try(eval(parse(text=paste("checkClogitOp_", nm, "('", val, "')", sep=""))),
                   silent=TRUE)
      } else {
        tmp <- try(eval(parse(text=paste("checkClogitOp_", nm, "(", val, ")", sep=""))),
                   silent=TRUE)
      }
      if ("try-error" %in% class(tmp)) {
        print(tmp)
        stop(paste("ERROR: the option ", nm, getOpStrEq(), val, " is not valid", sep=""))
      }
    }
  }

  ret <- default.list(op, valid, def)

  ret

} # END: runModel.checkClogitOpList

checkClogitOp_method <- function(x) {
  valid <- c("efron", "breslow", "exact", "approximate")
  x   <- check.string(x, valid, "method") 
  x
}
checkClogitOp_weights <- function(x, data, varMap) {

  if (!isString(x)) stop("ERROR: weights must be a variable name")
  if (length(data)) {
    check.variableInData(x, "weights", data, varMap, numeric=1, positive=1)
  }
  x
}

runModel.defRetObj.clogit <- function(dmatCols0, op) {

  vec                  <- getEffectsGlmCoefNames() # Same column names as GLM
  coef.names           <- vec
  if (op[[getOutModSumOpName(), exact=TRUE]] == getOutModSumOpDefault()) {
    fit.names          <- getModelSummaryNobsName()
  } else {
    fit.names          <- getModelSummaryClogitFitNames()
  }
  coef.stats           <- matrix(data=NA, nrow=1, ncol=length(coef.names))
  colnames(coef.stats) <- coef.names
  fit.stats            <- rep(NA, length(fit.names))
  names(fit.stats)     <- fit.names
  adj                  <- runModel.getVarStr(dmatCols0[-1])
  wp                   <- NA
  names(wp)            <- ""

  list(converged=FALSE, coef.stats=coef.stats, fit.stats=fit.stats, 
       msg="", adj=adj, adj.rem="", wald.pvalue=wp)

} # END: runModel.defRetObj.clogit

runModel.callClogit <- function(x, y, op) {

  mop   <- op[[getModelOpsName()]]
  yvar  <- "...y"
  gvar  <- "...g"
  ...g  <- mop$group.vec
  
  # first column in design matrix will be for response
  x[, 1]      <- y
  cx          <- colnames(x)
  cx[1]       <- yvar
  colnames(x) <- cx
  form        <- as.formula(runModel.getFormulaStr(yvar, cx[-1], strata.var=gvar))

  # Do this to prevent warnings with no weights and exact method
  if (!mop$weightsFlag) {
    fit <- clogit(form, data=as.data.frame(x), method=mop$method)
  } else {
    fit <- clogit(form, data=as.data.frame(x), weights=mop$weights.vec, method=mop$method)
  }
  
  fit

} # END: runModel.callClogit

runModel.tidyClogit <- function(nsubs, fit, exposure, expVars, defObj, modeldata, op) {

  nv        <- getModelSummaryNobsName()
  n         <- length(fit)
  dmatCols0 <- modeldata$designMatCols  
  if ("try-error" %in% class(fit)) {
    ret                   <- defObj
    ret$msg               <- runModel.getErrorMsg(fit)
    ret$fit.stats[nv]     <- nsubs
  } else if (!n || isString(fit)) {
    ret                   <- defObj
    ret$fit.stats[nv]     <- nsubs
    if (n) {
      ret$msg <- fit
    } else {
      ret$msg <- runModel.getUnknownErrorStr()
    }    
  } else {
    if (op[[getOutModSumOpName(), exact=TRUE]] == getOutModSumOpDefault()) {
      obj2 <- nsubs
    } else {
      obj2 <- as.numeric(glance(fit))
    }
    sfit <- summary(fit)
    obj  <- sfit$coefficients

    # Get the matrix of coefficients
    tmp     <- runModel.getCoefsClogit(obj, defObj, expVars, op)
    obj1    <- tmp$obj1
    msg     <- ""

    vec     <- fit$coefficients
    nms     <- names(vec)
    # We only want the non-exposure adjustments
    nms0    <- names(dmatCols0)
    tmp0    <- nms %in% nms0
    tmp     <- is.finite(vec) & tmp0
    rem     <- !(nms0 %in% nms) 
    rem[1]  <- FALSE # Ignore intercept (response) col
    adj     <- runModel.getAdjVarStr(nms[tmp], dmatCols0)
    adj.rem <- runModel.getAdjVarStr(nms0[rem], dmatCols0)

    # Wald p-value
    wald.p <- runModel.getWaldPvalues(exposure, expVars, modeldata[["acovs.new.list", exact=TRUE]], 
                                      fit, sfit=sfit)
    ret  <- list(converged=1, coef.stats=obj1, fit.stats=obj2, 
                 msg=msg, adj=adj, adj.rem=adj.rem, 
                 wald.pvalue=wald.p)  
  } 

  ret

} # END: runModel.tidyClogit

runModel.getCoefsClogit <- function(coefMat, defObj, expVars, op) {

  outeff  <- op[[getOutEffectsOpName(), exact=TRUE]]
  defFlag <- outeff == getOutEffectsOpDefault()
  msg     <- ""

  # Get the rows and columns we need and convert to a named vector
  terms <- rownames(coefMat)
  if (defFlag) {
    tmp <- terms %in% expVars
  } else {
    tmp <- rep(TRUE, length(terms)) 
  }
  m     <- sum(tmp)
  if (m) {    
    # Get correct columns
    cols   <- c(1, 3, 4, 5) # coef se(coef) z Pr(>|z|)
    terms  <- terms[tmp]
    obj    <- as.matrix(coefMat[tmp, cols, drop=FALSE]) 
    obj1   <- cbind(terms, obj) 
  } else {
    # No exposure names found in fitted object, return default 
    obj1 <- defObj$coef.stats
    msg  <- runModel.getExpNotEstimated()
  }

  list(obj1=obj1, msg=msg)
}


