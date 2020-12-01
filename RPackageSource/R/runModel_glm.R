#' A list of 6:
#' \itemize{
#' \item{\code{family}}{ One of: "binomial", "gaussian", "Gamma", "inverse.gaussian",
#'             "poisson", "quasi", "quasibinomial", "quasipoisson".
#'             The default is "gaussian".}
#' \item{\code{link}}{ NULL or a string for the link function to use (see \code{\link[stats]{family}}).
#'                     The default is to use the canonical link for \code{family}.}
#' \item{\code{weights}}{ A variable name to specify weights. The default is NULL.}
#' \item{\code{offset}}{ A variable name to specify an offset. The default is NULL.}
#' \item{\code{control}}{ See \code{\link[stats]{glm}}.
#'                      The default is \code{\link[stats]{glm.control}}.}
#' \item{\code{singular.ok}}{ See \code{\link[stats]{glm}}. The default is TRUE.}
#' }
#'
#' @name glm.options
#' @title options list for glm
#' 
#' @examples 
#' model.options <- list(family="binomial", weights="weightVarInData")
NULL

runModel.getDefaultGlmOptions <- function() {

  ret <- list(family="gaussian", link="", weights=NULL, 
              offset=NULL, control=glm.control(), 
              singular.ok=TRUE)
  ops.c    <- c("family", "link", "weights", "offset")
  ops.list <- "control"
  ops.log  <- "singular.ok"

  list(default=ret, ops.character=ops.c, ops.list=ops.list, ops.logical=ops.log)

} # END: runModel.getDefaultGlmOptions

runModel.convertGlmOp <- function(opName, opValue) {

  ret <- opValue
  if (opName %in% c("control")) {
    ret <- eval(parse(text=opValue))
  } else if (opName %in% c("tol")) {
    ret <- as.numeric(opValue)
  } else if (opName %in% c("singular.ok")) {
    ret <- getLogicalValueFromStr(opValue)
  }

  ret

} # END: runModel.convertGlmOp

runModel.parseAndCheckGlmOps <- function(str, model, modeldata) {

  if (model == getGlmModelName()) {
    tmp   <- runModel.getDefaultGlmOptions()
  } else if (model == getLmModelName()) {
    tmp   <- runModel.getDefaultLmOptions()
  } else {
    stop("INTERNAL CODING ERROR in runModel.parseAndCheckGlmOps")
  }
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
    value <- runModel.convertGlmOp(name, value)

    ret[[name]] <- value
  }

  # Check the values
  ret <- try(runModel.checkGlmOpList(ret, modeldata, name=getModelOpsName()), silent=TRUE)
  if ("try-error" %in% class(ret)) {
    print(ret)
    msg <- paste("ERROR with options, make sure options are separated by a ", 
                 getOpStrSep(), sep="")
    stop(msg)
  }

  ret

} # END: runModel.parseAndCheckGlmOps

runModel.checkGlmOpList <- function(op, modeldata, name="glm.options", model="glm") {

  n       <- length(op)
  if (n && !is.list(op)) stop(paste("ERROR: ", name, " must be a list", sep=""))
  if (model == "glm") {
    tmp <- runModel.getDefaultGlmOptions()
  } else {
    tmp <- runModel.getDefaultLmOptions()
  }
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
      } else if (nm %in% c("weights", "offset")) {
        tmp <- try(eval(parse(text=paste("checkGlmOp_", nm, "('", val, "', data, varMap)", sep=""))),
                   silent=TRUE)
      } else if (nm == "control") {
        tmp <- try(checkGlmOp_control(val), silent=TRUE)
      } else if (nm %in% ops.c) {
        tmp <- try(eval(parse(text=paste("checkGlmOp_", nm, "('", val, "')", sep=""))),
                   silent=TRUE)
      } else {
        tmp <- try(eval(parse(text=paste("checkGlmOp_", nm, "(", val, ")", sep=""))),
                   silent=TRUE)
      }
      if ("try-error" %in% class(tmp)) {
        print(tmp)
        stop(paste("ERROR: the option ", nm, getOpStrEq(), val, " is not valid", sep=""))
      }
    }
  }

  ret <- default.list(op, valid, def)

  # Check the link
  if (model == "glm") {
    link <- trimws(ret[["link", exact=TRUE]])
    if (!length(link) || (nchar(link) < 1)) link <- runModel.get.canonical(ret$family) 
    checkGlmOp_link(link, ret$family)
    ret$link <- link
  }

  ret

} # END: runModel.checkGlmOpList

checkGlmOp_family <- function(x) {
  runModel.check.family(x)
}
checkGlmOp_link <- function(x, family) {
  runModel.check.link(x, family)
}
checkGlmOp_control <- function(x) {
  valid <- c("epsilon", "maxit", "trace")
  x     <- check.list(x, "control", valid)
  x     <- default.list(x, valid, list(1e-8, 25, FALSE))
  x     <- glm.control(epsilon=x$epsilon, maxit=x$maxit, trace=x$trace)
  x
}
checkGlmOp_singular.ok <- function(x) {
  check.logical(x, "singular.ok") 
}
checkGlmOp_weights <- function(x, data, varMap) {

  if (!isString(x)) stop("ERROR: weights must be a variable name")
  if (length(data)) {
    check.variableInData(x, "weights", data, varMap, numeric=1, positive=1)
  }
  x
}
checkGlmOp_offset <- function(x, data, varMap) {

  if (!isString(x)) stop("ERROR: offset must be a variable name")
  if (length(data)) {
    check.variableInData(x, "offset", data, varMap, numeric=1, positive=0)
  }
  x
}
checkGlmOp_tol <- function(x) {
  check.range(x, "tol", 0, Inf)
}


runModel.get.families <- function() {

  c("binomial", "gaussian", "Gamma", "inverse.gaussian",
    "poisson", "quasi", "quasibinomial", "quasipoisson")

} # END: runModel.get.families

runModel.check.family <- function(obj) {

  valid <- runModel.get.families()
  obj   <- check.string(obj, valid, "family") 

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

  valid <- runModel.get.links(family)
  obj   <- check.string(obj, valid, "link") 

  obj

} # END: runModel.check.link

runModel.getFamilyFun <- function(family, link) {

  str <- paste(family, "(link='", link, "')", sep="")
  ret <- eval(parse(text=str))

  ret

} # END: runModel.getFamilyFun

runModel.defRetObj.glm <- function(dmatCols0) {

  vec               <- c("term", "estimate", "std.error", "statistic", "p.value")
  coef.names        <- vec
  fit.names         <- c("null.deviance", "df.null", "logLik", "AIC",
                         "BIC", "deviance", "df.residual", "nobs")
  coef.stats        <- matrix(data=NA, nrow=1, ncol=length(coef.names))
  colnames(coef.stats) <- coef.names
  fit.stats         <- rep(NA, length(fit.names))
  names(fit.stats)  <- fit.names
  adj               <- runModel.getVarStr(dmatCols0[-1])

  list(converged=FALSE, coef.stats=coef.stats, fit.stats=fit.stats, 
       msg="", adj=adj, adj.rem="", wald.pvalue=NA)

} # END: runModel.defRetObj.glm

runModel.callGLM <- function(x, y, op) {

  # The model.options list will contain the variable names for the 
  #  weights and offset, but we need the actual vector of values
  #  that is stored in op
  mop <- op[[getModelOpsName()]]
  fit <- glm.fit(x, y, family=mop$famFun, 
                 weights=mop[["weights.vec", exact=TRUE]],
                 offset=mop[["offset.vec", exact=TRUE]],
                 control=mop[["control", exact=TRUE]],
                 singular.ok=mop[["singular.ok", exact=TRUE]])
  class(fit) <- c("glm", "lm")
  
  fit

} # END: runModel.callGLM

runModel.tidyGLM <- function(nsubs, fit, expVars, defObj, dmatCols0) {

  n <- length(fit)
  if ("try-error" %in% class(fit)) {
    ret                   <- defObj
    ret$msg               <- runModel.getErrorMsg(fit)
    ret$fit.stats["nobs"] <- nsubs
  } else if (!n || isString(fit)) {
    ret                   <- defObj
    ret$fit.stats["nobs"] <- nsubs
    if (n) {
      ret$msg <- fit
    } else {
      ret$msg <- runModel.getUnknownErrorStr()
    }    
  } else {
    obj2 <- as.numeric(glance(fit))
    conv <- fit$converged
    if (length(conv) != 1) conv <- FALSE
    sfit <- summary(fit)
    obj  <- sfit$coefficients
    msg  <- ""

    # Get the rows and columns we need and convert to a named vector
    terms <- rownames(obj)
    tmp   <- terms %in% expVars
    m     <- sum(tmp)
    if (m) {    
      terms  <- terms[tmp]
      obj    <- as.matrix(obj[tmp, , drop=FALSE]) 
      obj1   <- cbind(terms, obj) 
    } else {
      # No exposure names found in fitted object, return default 
      obj1 <- defObj$coef.stats
      msg  <- "exposure could not be estimated"
    }
    vec     <- fit$coefficients
    nms     <- names(vec)
    # We only want the non-intercept, non-exposure adjustments
    nms0    <- names(dmatCols0)
    tmp0    <- nms %in% nms0
    tmp     <- is.finite(vec) & tmp0
    tmp[1]  <- FALSE
    rem     <- !(nms0 %in% nms) 
    rem[1]  <- FALSE
    adj     <- runModel.getAdjVarStr(nms[tmp], dmatCols0)
    adj.rem <- runModel.getAdjVarStr(nms0[rem], dmatCols0)

    # Wald p-value
    wald.p <- runModel.getWaldTest(fit, expVars, sfit=sfit)$pvalue 

    ret  <- list(converged=conv, coef.stats=obj1, fit.stats=obj2, 
                 msg=msg, adj=adj, adj.rem=adj.rem, 
                 wald.pvalue=wald.p)  
  } 
  ret

} # END: runModel.tidyGLM




