#' A list of 6:
#' \itemize{
#' \item{\code{ties}}{ One of: "efron", "breslow", "exact".
#'             The default is "efron".}
#' \item{\code{robust}}{ TRUE or FALSE for computing a robust covariance matrix).
#'                     The default is FALSE.}
#' \item{\code{weights}}{ A variable name to specify weights. The default is NULL.}
#' \item{\code{singular.ok}}{ See \code{\link[survival]{coxph}}. The default is TRUE.}
#' \item{\code{Surv.type}}{ See the \code{type} option in \code{\link[survival]{Surv}}. The default is NULL.}
#' }
#'
#' @name coxph.options
#' @title options list for coxph
#' 
#' @examples 
#' model.options <- list(robust=TRUE, weights="weightVarInData")
NULL

runModel.getDefaultCoxphOptions <- function() {

  ret <- list(ties="efron", robust=FALSE, weights=NULL,  
              singular.ok=TRUE, Surv.type=NULL)
  ops.c    <- c("ties", "weights", "Surv.type")
  ops.log  <- c("singular.ok", "robust")

  list(default=ret, ops.character=ops.c, ops.logical=ops.log)

} # END: runModel.getDefaultCoxphOptions

runModel.convertCoxphOp <- function(opName, opValue) {

  ret <- opValue
  if (opName %in% c("singular.ok", "robust")) {
    ret <- getLogicalValueFromStr(opValue)
  }

  ret

} # END: runModel.convertCoxphOp

runModel.parseAndCheckCoxphOps <- function(str, modeldata) {

  tmp   <- runModel.getDefaultCoxphOptions() 
  def   <- tmp$default
  valid <- names(def)

  if (!length(str)) return(def)
  if (!isString(str)) stop(msg_arg_notString("str"))

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
    if (!(name %in% valid)) stop(msg_arg_opNotValid(strVec[i]))
    if (!length(value)) stop(msg_arg_opNotValid(strVec[i]))
    
    # value must be converted to the correct type
    value <- runModel.convertCoxphOp(name, value)

    ret[[name]] <- value
  }

  # Check the values
  ret <- try(runModel.checkCoxphOpList(ret, modeldata, name=getModelOpsName()), silent=TRUE)
  if ("try-error" %in% class(ret)) {
    print(ret)
    msg <- msg_arg_opsError()
    stop(msg)
  }

  ret

} # END: runModel.parseAndCheckCoxphOps

runModel.checkCoxphOpList <- function(op, modeldata, name="coxph.options") {

  n   <- length(op)
  if (n && !is.list(op)) stop(msg_arg_notList(name))
  tmp <- runModel.getDefaultCoxphOptions()
  
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
      if (nm == "link") {
        next
      } else if (nm %in% c("weights")) {
        tmp <- try(eval(parse(text=paste("checkCoxphOp_", nm, "('", val, "', data, varMap)", sep=""))),
                   silent=TRUE)
      } else if (nm %in% ops.c) {
        tmp <- try(eval(parse(text=paste("checkCoxphOp_", nm, "('", val, "')", sep=""))),
                   silent=TRUE)
      } else {
        tmp <- try(eval(parse(text=paste("checkCoxphOp_", nm, "(", val, ")", sep=""))),
                   silent=TRUE)
      }
      if ("try-error" %in% class(tmp)) {
        print(tmp)
        stop(msg_arg_opEqValNotValid(c(nm, val)))
      }
    }
  }

  ret <- default.list(op, valid, def)

  ret

} # END: runModel.checkCoxphOpList

checkCoxphOp_singular.ok <- function(x) {
  check.logical(x, "singular.ok") 
}
checkCoxphOp_robust <- function(x) {
  check.logical(x, "robust") 
}
checkCoxphOp_ties <- function(x) {
  valid <- c("efron", "breslow", "exact")
  x   <- check.string(x, valid, "ties") 
  x
}
checkCoxphOp_Surv.type <- function(x) {
  valid <- c('right', 'left', 'interval', 'counting', 'interval2', 'mstate')
  x   <- check.string(x, valid, "Surv.type") 
  x
}
checkCoxphOp_weights <- function(x, data, varMap) {

  if (!isString(x)) stop(msg_arg_colNotValid("weights"))
  if (length(data)) {
    check.variableInData(x, "weights", data, varMap, numeric=1, positive=1)
  }
  x
}

runModel.defRetObj.coxph <- function(dmatCols0, op) {

  vec                  <- getEffectsGlmCoefNames() # Same column names as GLM
  coef.names           <- vec
  if (op[[getOutModSumOpName(), exact=TRUE]] == getOutModSumOpDefault()) {
    fit.names          <- getModelSummaryNobsName()
  } else {
    fit.names          <- getModelSummaryCoxphFitNames()
  }
  coef.stats           <- matrix(data=NA, nrow=1, ncol=length(coef.names))
  colnames(coef.stats) <- coef.names
  fit.stats            <- rep(NA, length(fit.names))
  names(fit.stats)     <- fit.names
  adj                  <- runModel.getVarStr(dmatCols0[-1])
  wp                   <- NA
  names(wp)            <- ""

  list(converged=FALSE, coef.stats=coef.stats, fit.stats=fit.stats, 
       msg="", adj=adj, adj.rem="", wald.pvalue=wp, cov.str="")

} # END: runModel.defRetObj.coxph

runModel.callCoxph <- function(x, y, op) {

  mop   <- op[[getModelOpsName()]]
  yvar  <- "...y"
  t1var <- "...t1"
  ntime <- mop$n.time.vars
  ...t1 <- mop$time1.vec
  if (ntime > 1) {
   t2var <- "...t2" 
   ...t2 <- mop$time2.vec
  } else {
   t2var <- NULL
  }

  # first column in design matrix will be for response
  x[, 1]      <- y
  cx          <- colnames(x)
  cx[1]       <- yvar
  colnames(x) <- cx
  form        <- as.formula(runModel.getFormulaStr(yvar, cx[-1], time1.var=t1var, time2.var=t2var, 
                                                   type=mop[["Surv.type", exact=TRUE]]))
  fit         <- coxph(form, data=as.data.frame(x), ties=mop$ties, 
                 weights=mop[["weights.vec", exact=TRUE]],
                 singular.ok=mop$singular.ok, robust=mop$robust)
  
  fit

} # END: runModel.callCoxph

runModel.tidyCoxph <- function(nsubs, fit, exposure, expVars, defObj, modeldata, op, expRef) {

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
    tmp     <- runModel.getCoefsCoxph(obj, defObj, expVars, op)
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
    parmslist <- NULL
    if (!op$out.eff.exp) parmslist <- modeldata[["acovs.new.list", exact=TRUE]]
    wald.p <- runModel.getWaldPvalues(exposure, expVars, parmslist, fit, sfit=sfit)

    # Get covariance string
    cov.str <- runModel.getUpperTriCovStr(fit, sfit, expVars, expRef)

    ret  <- list(converged=1, coef.stats=obj1, fit.stats=obj2, 
                 msg=msg, adj=adj, adj.rem=adj.rem, 
                 wald.pvalue=wald.p, cov.str=cov.str)  
  } 

  ret

} # END: runModel.tidyGLM

runModel.getCoefsCoxph <- function(coefMat, defObj, expVars, op) {

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
    mop <- op[[getModelOpsName()]]
    if (mop$robust) {
      cols <- c(1, 4, 5, 6) # coef robust_se z Pr(>|z|)
    } else {
      cols <- c(1, 3, 4, 5) # coef se(coef) z Pr(>|z|)
    }
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


