#' Main function for running a model
#' @param modeldata list from function getModelData
#' @param metabdata metabolite data list
#' @param cohort cohort label (e.g DPP, NCI, Shanghai)
#' @param model string specifiying the model, "pcorr" or "glm"
#' @param family string for the family when model="glm"
#' @param link string for the link function when model="glm". The default will be to use the canonical link.
#' @param ... additional arguments for glm
#'
#' @return data frame with each row representing the correlation for each combination of outcomes and exposures represented as specified in the
#' model (*spec), label (*lab), and universal id (*_uid)
#' with additional columns for n, pvalue, method of model specification (Interactive or Batch), universal id for outcomes (outcome_uid) and exposures (exposure_uid)
#' name of the cohort, adjustment (adjvars) and stratification (stratavar,strata)  variables. Attribute of dataframe includes ptime for processing time of model
#' run.
#'
runModel <- function(modeldata, metabdata, cohort="", 
                     model="pcorr", family="gaussian", link="", ...) {

  model <- runModel.check.model(model)
  if (model == "pcorr") {
    ret <- runCorr(modeldata, metabdata, cohort=cohort)
  } else {
    family <- runModel.check.family(family)
    link   <- runModel.check.link(link, family)
    famFun <- runModel.getFamilyFun(family, link)

  }

  ret

} # END: runModel 

# Function to check an argument 
runModel.check.str <- function(obj, valid, parm) {

  # obj:   A character string (length 1)
  # valid: Character vector of valid values
  # parm:  The name of the argument being checked

  errFlag <- 0
 
  # Check for errors
  if (!isString(obj)) errFlag <- 1 
  obj <- removeWhiteSpace(obj)
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

  if (isString(obj) && (nchar(removeWhiteSpace(obj)) < 1) {
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

# Function to remove leading/trailing white space
removeWhiteSpace <- function(str, leading=1, trailing=1) {

  if ((leading) && (trailing)) {
    ret <- gsub("^\\s+|\\s+$", "", str, perl=TRUE)
  } else if (leading) {
    ret <- gsub("^\\s+", "", str, perl=TRUE)
  } else if (trailing) {
    ret <- gsub("\\s+$", "", str, perl=TRUE)
  } else {
    ret <- str
  }

  ret

} # END: removeWhiteSpace

# Function to check that an object is a string
isString <- function(obj) {

  if ((length(obj) == 1) && is.character(obj)) {
    ret <- TRUE
  } else {
    ret <- FALSE
  }

  ret

} # END: isString
