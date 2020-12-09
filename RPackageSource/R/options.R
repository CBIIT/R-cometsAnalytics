#' A list of 8:
#' \itemize{
#' \item{\code{check.cor.cutoff}}{ Cutoff value to remove highly correlated columns in the
#'                         design matrix. The default value is 0.97.}
#' \item{\code{check.cor.method}}{ Correlation method to remove highly correlated columns 
#'                      in the design matrix. It must be one of 
#'                   "spearman", "pearson", "kendall".
#'                         The default value is "spearman".}
#' \item{\code{check.design}}{ TRUE or FALSE to check the design matrix for linearly
#'                     dependent columns, highly correlated columns, or for 
#'                     being ill-conditioned whenever it is updated.
#'                     The default is TRUE.}
#' \item{\code{check.illCond}}{ TRUE or FALSE to check for an ill-conditioned design matrix.
#'                      The default is TRUE.}
#' \item{\code{check.nsubjects}}{ Minimum number of subjects. The default is 25.}
#' \item{\code{max.nstrata}}{ The maximum number of strata for a stratified analysis.
#'                    The default is 10.}
#' \item{\code{model}}{ String for the model function. Currently, it must be one of
#'                      "correlation", "lm", or "glm". The default is "correlation".} 
#' \item{\code{model.options}}{ List of options specific for the model. 
#'        See \code{\link{correlation.options}}, \code{\link{glm.options}},
#'        and \code{\link{lm.options}} for options specific to
#' \code{model="correlation", "lm", "glm"} respectively.        
#'                      The default is NULL.} 
#' }
#'
#' @name options
#' @title options list
#' @details Before any analysis is performed, an initial design matrix is
#'          constructed using the above options as follows: \cr
#'          1. Adjustment variables with less than two distinct non-missing
#'             values, or with less than \code{check.nsubjects} non-missing
#'             values are removed. \cr
#'          2. The design matrix is created from the remaining adjustment variables
#'             and any linearly dependent columns are removed. \cr
#'          3. If \code{check.cor.cutoff > 0}, then highly correlated columns
#'             are removed by computing the correlation matrix 
#'             \code{cor_matrix = cor(design_matrix, method=check.cor.method)}. \cr
#'          4. If \code{check.illCond} is TRUE, then the function
#'          \code{subselect::trim.matrix} is applied 
#'             to the correlation matrix to determine if additional
#'             columns will be removed. \cr \cr
#' If \code{check.design} is TRUE, then steps 2-4 are repeated each time an exposure
#' variable is added to the design matrix or when rows of the design matrix are removed
#' due to missing values in an outcome variable.
#' 
#' @examples 
#' # Logistic regression with all default options
#' model.op <- list(family="binomial")
#' op <- list(model="glm", model.options=model.op)
#' # Compute Pearson correlations requiring at least 100 subjects
#' model.op <- list(method="pearson")
#' op <- list(model="correlation", check.nsubjects=100, model.options=model.op)
NULL

# Called from runModel
runModel.checkOptions <- function(op, modeldata) {

  if (!length(op)) op <- list()
  if (!is.list(op)) stop("op must be a list")
   
  valid <- getAllOptionNames()

  # Check for valid names in the list
  checkOptionListNames(op, valid, "op") 
  opnames <- names(op)

  # Check global options 
  valid <- getValidGlobalOps()$valid
  op2   <- list()
  if (length(opnames)) {
    tmp   <- opnames %in% valid
    if (any(tmp)) op2 <- op[opnames[tmp]]
  }
  tmp <- checkGlobalOpList(op2, name="options")
  for (nm in names(tmp)) op[[nm]] <- tmp[[nm]]

  # Check model options
  op <- checkModelOptions(op, modeldata)

  # Turn off this option
  op$check.nearZeroVar.freqCut <- 0

  # Set other objects needed later
  op$colNamePrefix       <- "...x"
  op$rowNamePrefix       <- "r"
  op$check.n.unique.vals <- 2

  op

} # END: runModel.checkOptions

convertModelOptions <- function(opnames, opvalues, model) {

  n      <- length(opnames)
  if (n != length(opvalues)) stop("INTERNAL CODING ERROR in convertModelOptions")
  tmp    <- getDefaultModelOptions(model)
  ops.c  <- tmp[["ops.character", exact=TRUE]]
  ops.l  <- tmp[["ops.list", exact=TRUE]]
  ops.TF <- tmp[["ops.logical", exact=TRUE]]
  ret    <- list()
  for (i in 1:n) {
    nm  <- opnames[i]
    val <- opvalues[i]
    if (nm %in% ops.l) {
      val <- eval(parse(text=val))
    } else if (nm %in% ops.TF) {
      val <- getLogicalValueFromStr(val) 
    } else if (!(nm %in% ops.c)) {
      val <- as.numeric(val) 
    }
    ret[[nm]] <- val
  }

  ret

} # END: convertModelOptions

getDefaultModelOptions <- function(model) {

  if (model == getCorrModelName()) {
    ret <- runModel.getDefaultPcorrOptions()
  } else if (model == getGlmModelName()) {
    ret <- runModel.getDefaultGlmOptions()
  } else if (model == getLmModelName()) {
    ret <- runModel.getDefaultLmOptions()
  } else {
    stop("INTERNAL CODING ERROR in getDefaultModelOptions")
  }

  ret

} # END: getDefaultModelOptions

checkModelOptions <- function(op, modeldata) {

  op$pcorrFlag <- 0
  op$glmFlag   <- 0
  op$lmFlag    <- 0
  model        <- op$model
  corrName     <- getCorrModelName()
  nm           <- getModelOpsName()
  mop          <- op[[nm, exact=TRUE]]
  
  if (model == corrName) {
    op$pcorrFlag <- 1
    mop          <- runModel.checkPcorrOpList(mop, name="model.options")
    mop$family   <- getNoFamilyValue()
  } else if (model == getGlmModelName()) {
    op$glmFlag <- 1
    mop        <- runModel.checkGlmOpList(mop, modeldata, name=nm, model=model)
    mop$famFun <- runModel.getFamilyFun(mop$family, mop$link) 
  } else if (model == getLmModelName()) {
    op$lmFlag  <- 1
    mop        <- runModel.checkLmOpList(mop, modeldata, name=nm)
    mop$family <- getNoFamilyValue()
  } else {
    stop("INTERNAL CODING ERROR in checkModelOptions")
  }

  # Determine if weights or offset was specified
  mop$weightsFlag <- 0
  mop$offsetFlag  <- 0
  if (op$glmFlag || op$lmFlag) {
    if (length(mop[["weights", exact=TRUE]])) mop$weightsFlag <- 1
    if (length(mop[["offset", exact=TRUE]]))  mop$offsetFlag  <- 1
  }

  op[[nm]] <- mop

  op

} # END: checkModelOptions

getAllOptionNames <- function() {

  tmp   <- getValidGlobalOps()$valid
  valid <- c(tmp, getModelOpsName())

  valid

} # END: getAllOptionNames

getValidGlobalOps <- function() {
  ops.char <- c("model", "check.cor.method")
  ops.num  <- c("check.cor.cutoff", "check.nsubjects", "max.nstrata", "DEBUG")
  ops.log  <- c("check.illCond", "check.design")
  default  <- list(check.cor.method="spearman", check.illCond=TRUE, 
                   check.cor.cutoff=0.97, check.nsubjects=25, 
                   check.design=TRUE, max.nstrata=10,
                   model=getCorrModelName(), DEBUG=0)
  valid    <- names(default)
  list(ops.character=ops.char, ops.numeric=ops.num, ops.logical=ops.log,
       valid=valid, default=default)

} # END: getValidGlobalOps 

# Parse and check global options from options sheet. Return a list of options.
checkGlobalOpsFromCharVecs <- function(opnames, opvalues) {

  tmp    <- getValidGlobalOps()
  def    <- tmp$default
  ops.n  <- tmp$ops.numeric
  ops.l  <- tmp$ops.logical
  valid  <- names(def)

  n <- length(opnames)
  if (!n) return(def)

  opnames  <- trimws(opnames)
  opvalues <- trimws(opvalues)
  
  ret <- list()
  # Loop over each element
  eq <- getOpStrEq()
  for (i in 1:n) {
    name  <- opnames[i]
    value <- opvalues[i]
    if (!(name %in% valid)) stop(paste("ERROR: ", name, " is not a valid option", sep=""))
    if (!nchar(value)) stop(paste("ERROR: ", name, "=", value, " is not valid", sep=""))
    
    # value must be converted to the correct type
    if (name %in% ops.n) {
      value <- as.numeric(value)
    } else if (name %in% ops.l) {
      # Logical value
      value <- getLogicalValueFromStr(value) 
    }

    ret[[name]] <- value
  }

  # Check the values
  ret <- try(checkGlobalOpList(ret, name="options"), silent=TRUE)
  
  ret

} # END: checkGlobalOpsFromCharVecs


checkOptionListNames <- function(op, valid, name) {

    if (!length(op)) return(NULL)

    # Names cannot be ""
    nms <- trimws(names(op))
    tmp <- nchar(nms) < 1
    if (any(tmp)) {
      print(op)
      stop(paste("ERROR: the above ", name, " list is not valid", sep="")) 
    }
    tmp <- !(nms %in% valid)
    if (any(tmp)) {
      err <- paste(nms[tmp], collapse=",", sep="")
      if (length(err) > 1) {
        stop(paste("ERROR: ", err, " are not valid option names for ", name, sep=""))
      } else {
        stop(paste("ERROR: ", err, " is not a valid option name for ", name, sep=""))
      }
    }  

    NULL

} # END: checkOptionListNames

checkRequiredListNames <- function(x, req, name) {

  if (!length(x)) stop(paste0(name, " has length 0"))
  if (!is.list(x)) stop(paste0(name, " must be a list"))

  tmp  <- !(req %in% names(x))
  miss <- req[tmp]
  if (length(miss)) {
    tmp <- paste0(miss, collapse=", ")
    msg <- paste0("ERROR: the objects ", tmp, " are not in ", name)
    stop(msg)  
  }
  
  NULL

} # END: checkRequiredListNames

checkGlobalOpList <- function(op, name="options") {

  n       <- length(op)
  if (n && !is.list(op)) stop(paste("ERROR: ", name, " must be a list", sep=""))
  tmp     <- getValidGlobalOps()
  default <- tmp$default
  valid   <- names(default)
  ops.n   <- c(tmp$ops.numeric, tmp$ops.logical)

  # Check the names and values
  if (n) {
    checkOptionListNames(op, valid, name)
    nms <- names(op) 

    for (i in 1:n) {
      nm  <- nms[i]
      val <- op[[i]]
      if (nm %in% ops.n) {
        tmp <- try(eval(parse(text=paste("checkOp_", nm, "(", val, ")", sep=""))),
                   silent=TRUE)
      } else {
        tmp <- try(eval(parse(text=paste("checkOp_", nm, "('", val, "')", sep=""))),
            silent=TRUE)
      }
      if ("try-error" %in% class(tmp)) {
        print(tmp)
        stop(paste("ERROR: the option ", nm, getOpStrEq(), val, " is not valid", sep=""))
      }
    }
  }
  
  ret <- default.list(op, valid, default)

  ret

} # END: checkGlobalOpList

setupOpStr <- function(str) {

  ret <- trimws(str)
  ret <- gsub('"', "", ret, fixed=TRUE)
  ret <- gsub("'", "", ret, fixed=TRUE)
  ret <- gsub('\"', "", ret, fixed=TRUE)
  ret <- gsub("\'", "", ret, fixed=TRUE)
  ret

} # END: setupOpStr

checkOp_check.cor.method <- function(str, name="check.cor.method") {

  if (!isString(str)) stop("INTERNAL CODING ERROR in checkOp_check.cor.method")
  valid <- c("spearman", "pearson", "kendall")
  str   <- tolower(str)
  str   <- check.string(str, valid, name)
  str

} 

runModel.check.model <- function(obj) {

  valid <- getValidModelNames()
  obj   <- check.string(obj, valid, "model") 
  
  obj

} # END: runModel.check.model

# Function to check an argument 
check.string <- function(obj, valid, parm) {

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

} # END: check.string


check.logical <- function(x, name) {

  err <- 0
  n   <- length(x)
  if (!n || (n > 1)) {
    err <- 1
  } else {
    if ((x != 0) && (x != 1)) err <- 1
  }
  if (err) {
    msg <- paste("ERROR: ", name, " must be TRUE or FALSE", sep="")
    stop(msg)
  }

  x

} # END: check.logical

check.range <- function(x, name, lower, upper) {

  err <- 0
  n   <- length(x)
  if (!n || (n > 1) || !is.finite(x)) {
    err <- 1
  } else {
    if ((x < lower) || (x > upper)) err <- 1
  }
  if (err) {
    infFlag <- is.finite(upper)
    if (!infFlag) {
      msg <- paste("ERROR: ", name, " must be between ", 
                   lower, " and ", upper, sep="")
    } else {
      msg <- paste("ERROR: ", name, " must be >= ", lower, sep="")
    }
    stop(msg)
  }

  x

} # END: check.range

check.list <- function(x, name, valid) {

  if (!is.list(x)) stop(paste("ERROR: ", name, " must be a list", sep=""))
  ret <- checkOptionListNames(x, valid, name) 
  ret 

} # END: check.list

check.variableInData <- function(x, name, data, varMap, numeric=1, positive=0) {

  if (!isString(x)) stop(paste("ERROR: ", name, " must be a variable in the data", sep=""))
  vec <- NULL
  if (x %in% colnames(data)) vec <- data[, x, drop=TRUE]
  if (!length(vec)) {
    new <- (names(varMap))[varMap %in% x]  
    if (new %in% colnames(data)) vec <- data[, new, drop=TRUE]
  }
  if (!length(vec)) stop(paste("ERROR: ", name, " must be a variable in the data", sep=""))
  if (numeric) {
    if (!is.numeric(vec)) stop(paste("ERROR: ", name, " must be a numeric variable in the data", sep=""))
  }
  if (positive) {
    tmp <- vec < 0
    tmp[is.na(tmp)] <- FALSE
    if (any(tmp)) stop(paste("ERROR: ", name, " cannot have negative values", sep=""))
  }
  x

} # END: check.variableInData


checkOp_check.illCond <- function(x) {
  check.logical(x, "check.illCond") 
} 
checkOp_check.design <- function(x) {
  check.logical(x, "check.design") 
} 
checkOp_check.cor.cutoff <- function(x) {
  check.range(x, "check.cor.cutoff", 0, 1)
}
checkOp_check.nsubjects <- function(x) {
  check.range(x, "check.nsubjects", 2, Inf)
}
checkOp_max.nstrata <- function(x) {
  check.range(x, "max.nstrata", 1, Inf)
}
checkOp_DEBUG <- function(x) {
  check.logical(x, "DEBUG") 
} 
checkOp_model <- function(x) {
  runModel.check.model(x)
} 


####################################################################
# Old code to be removed:
# Parse and check global options from global options string. Return a list of options.
parseAndCheckGlobalOps.OLD <- function(str) {

  tmp    <- getValidGlobalOps()
  def    <- tmp$default
  ops.n  <- tmp$ops.numeric
  valid  <- names(def)

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
    if (name %in% ops.n) value <- as.numeric(value)

    ret[[name]] <- value
  }

  # Check the values
  ret <- try(checkGlobalOpList(ret, name="options"), silent=TRUE)
  if ("try-error" %in% class(ret)) {
    print(ret)
    msg <- paste("ERROR with options, make sure options are separated by a ", 
                 getOpStrSep(), sep="")
    stop(msg)
  }

  ret

} # END: parseAndCheckGlobalOps.OLD

getOptionNameAndValue <- function(str, sep="=") {

  tmp <- parseStr(str, sep=sep)
  if (length(tmp) < 2) {
    msg <- paste("ERROR: the option ", str, " is not valid", sep="")
    stop(msg)
  }
  name  <- tmp[1]
  value <- getOpValueFromVec(tmp[-1], sep=sep)

  list(name=name, value=value)

} # END: getOptionNameAndValue

getOpValueFromVec <- function(strVec, sep="=") {

  paste(strVec, collapse=sep, sep="")

} # END: getOpValueFromVec
