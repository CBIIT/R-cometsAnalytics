runModel.getGlmOptions <- function() {

  valid <- c("family", "weights", "start", "etastart", 
             "mustart", "offset", "control", 
             "singular.ok")

  default <- list(gaussian(), NULL, NULL, NULL,
                  NULL, NULL, glm.control(), 
                  TRUE)

  list(valid=valid, default=default)

} # END: runModel.getGlmOptionNames

runModel.checkGlmOptions <- function(op) {

  # op should include only the valid options 
  ret <- list()
  tmp <- runModel.getGlmOptions()
  val <- tmp$valid
  def <- tmp$default
  for (i in 1:length(val)) {
    nm  <- val[i]
    tmp <- op[[nm, exact=TRUE]] 
    if (is.null(tmp)) tmp <- def[[i]]
    ret[[nm]] <- tmp 
  }

  ret

} # END: runModel.checkGlmOptions

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

runModel.defRetObj.glm <- function(dmatCols0) {

  vec               <- c("term", "estimate", "std.error", "statistic", "p.value")
  coef.names        <- vec
  fit.names         <- c("null.deviance", "df.null", "logLik", "AIC",
                         "BIC", "deviance", "df.residual", "nobs")
  coef.stats        <- matrix(data=NA, nrow=1, ncol=length(coef.names))
  names(coef.stats) <- coef.names
  fit.stats         <- rep(NA, length(fit.names))
  names(fit.stats)  <- fit.names
  adj               <- runModel.getVarStr(dmatCols0[-1])

  list(converged=FALSE, coef.stats=coef.stats, fit.stats=fit.stats, 
       msg="", adj=adj, adj.rem="")

} # END: runModel.defRetObj.glm


runModel.callGLM <- function(x, y, famFun) {

  fit <- glm.fit(x, y, family=famFun)
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
      ret$msg <- "unknown error"
    }    
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
    vec     <- fit$coefficients
    nms     <- names(vec)
    # We only want the non-intercept, non-exposure adjustments
    nms0    <- names(dmatCols0)
    tmp0    <- nms %in% nms0
    tmp     <- is.finite(vec) & tmp0
    tmp[1]  <- FALSE

    rem     <- !(nms0 %in% nms) 
    adj     <- runModel.getAdjVarStr(nms[tmp], dmatCols0)
    adj.rem <- runModel.getAdjVarStr(nms0[rem], dmatCols0)

    ret  <- list(converged=conv, coef.stats=obj1, fit.stats=obj2, 
                 msg=msg, adj=adj, adj.rem=adj.rem)  
  } 
  ret

} # END: runModel.tidyGLM
