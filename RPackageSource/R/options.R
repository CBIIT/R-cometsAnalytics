#' A list of 12:
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
#'                      "correlation", "lm", "glm", "coxph", or "clogit". The default is "correlation".} 
#' \item{\code{model.options}}{ List of options specific for the model. 
#'        See \code{\link{correlation.options}}, \code{\link{glm.options}},
#'        \code{\link{lm.options}}, \code{\link{coxph.options}} and \code{\link{clogit.options}} for options specific to \cr
#' \code{model="correlation", "lm", "glm", "coxph", "clogit"} respectively.        
#'                      The default is NULL.} 
#' \item{\code{output.ci_alpha}}{ Confidence interval level for estimated from glm models. This
#'                         option must be a number >= 0 and < 1, where 0 is for not creating confidence intervals.
#'                         The default value is 0.95.}
#' \item{\code{output.Effects}}{ A string to define the terms output in the returned \code{\link{Effects}}
#'                            and \code{\link{ModelSummary}} data frames. Currently, it must be "exposure" or "all".
#'                           If set to "all", then summary statistics for the exposure
#'                            and adjustment variables will be output. Otherwise, only
#'                            summary statistics for the exposure will be output.
#'                            This option is ignored with \code{model = "correlation"}.
#'                            The default is "exposure".}
#' \item{\code{output.exp_parms}}{ TRUE, FALSE or NULL to exponentiate glm parameter estimates. 
#'                         Standard errors are obtained from the delta method. 
#'                         The default is NULL, so that estimates from glm models with 
#'                         family="binomial" will be exponentiated, and not otherwise.}
#' \item{\code{output.metab.cols}}{ Character vector of column names in the \code{METABOLITES}
#'                             sheet to be output in the \code{\link{ModelSummary}} and \code{\link{Effects}}
#'                             data frames. Metabolite ids are matched first using the 
#'                             \code{outcomespec} column and then using the \code{exposurespec} column.  
#'                         The default is "metabolite_name".}
#' \item{\code{output.ModelSummary}}{ A string to defines the columns output in the returned 
#'                            \code{\link{ModelSummary}} data frame. Currently, it must be "anova" or "all".
#'                            This option is ignored with \code{model = "correlation"}.
#'                            The default is "anova".}
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

  # Output options are ignored for correlation analysis
  if (op$pcorrFlag) {
    op[[getOutEffectsOpName()]] <- getOutEffectsOpDefault()
    op[[getOutModSumOpName()]]  <- getOutModSumOpDefault()
  }

  # The option getExpParmsOpName() might be NULL, so set to TRUE or FALSE based on the model
  nm  <- getExpParmsOpName()
  val <- op[[nm, exact=TRUE]]
  if (!length(val)) {
    val <- FALSE
    if (op$glmFlag && (op$model.options$family == "binomial")) val <- TRUE
    op[[nm]] <- val
  } 

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
    } else if (nm %in% ops.c) {
      val <- trimws(val)
      if (!nchar(val)) val <- NULL
      if (!length(val)) val <- NULL 
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
  } else if (model == getCoxphModelName()) {
    ret <- runModel.getDefaultCoxphOptions()
  } else if (model == getClogitModelName()) {
    ret <- runModel.getDefaultClogitOptions()
  } else {
    stop("INTERNAL CODING ERROR in getDefaultModelOptions")
  }

  ret

} # END: getDefaultModelOptions

checkModelOptions <- function(op, modeldata) {

  op$pcorrFlag  <- 0
  op$glmFlag    <- 0
  op$lmFlag     <- 0
  op$coxphFlag  <- 0
  op$clogitFlag <- 0
  model         <- op$model
  corrName      <- getCorrModelName()
  nm            <- getModelOpsName()
  mop           <- op[[nm, exact=TRUE]]
  
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
  } else if (model == getCoxphModelName()) {
    op$coxphFlag <- 1
    mop          <- runModel.checkCoxphOpList(mop, modeldata, name=nm)
    mop$family   <- "binomial"
  } else if (model == getClogitModelName()) {
    op$clogitFlag <- 1
    mop           <- runModel.checkClogitOpList(mop, modeldata, name=nm)
    mop$family    <- "binomial"
  } else {
    stop("INTERNAL CODING ERROR in checkModelOptions")
  }

  # Determine if weights or offset was specified
  mop$weightsFlag <- 0
  mop$offsetFlag  <- 0
  mop$timeFlag    <- 0
  mop$groupFlag   <- 0
  if (op$glmFlag || op$lmFlag || op$coxphFlag || op$clogitFlag) {
    if (length(mop[["weights", exact=TRUE]])) mop$weightsFlag <- 1
  }
  if (op$glmFlag || op$lmFlag) {
    if (length(mop[["offset", exact=TRUE]]))  mop$offsetFlag  <- 1
  }
  if (op$coxphFlag) {
    # Add info for time vars
    timecov <- modeldata[["timecov", exact=TRUE]]
    len     <- length(timecov)
    if (!len) stop(paste0("ERROR: no time variables specified for ", model, " model."))
    if (len > 2) stop(paste0("ERROR: only one or two time variables can be specified for ", model, " model."))
    mop$n.time.vars <- len
    mop$time1.var   <- timecov[1]
    if (len > 1) mop$time2.var <- timecov[2]
    mop$timeFlag <- 1
  }
  if (op$clogitFlag) {
    # Add info for group
    groupcov <- modeldata[["groupcov", exact=TRUE]]
    len      <- length(groupcov)
    if (!len) stop(paste0("ERROR: no group variable specified for ", model, " model."))
    if (len > 1) stop(paste0("ERROR: only one group variable can be specified for ", model, " model."))
    mop$group.var <- groupcov
    mop$groupFlag <- 1
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

  out.eff    <- getOutEffectsOpName()
  out.modSum <- getOutModSumOpName()
  add.ci     <- getAddCiOpName()
  exp.parms  <- getExpParmsOpName() 
  out.metabs <- getAddMetabColsOpName()

  ops.char    <- c("model", "check.cor.method", out.eff, out.modSum)
  ops.charVec <- c(out.metabs)
  ops.num     <- c("check.cor.cutoff", "check.nsubjects", "max.nstrata", 
                   add.ci, exp.parms, 
                   "DEBUG", "DONOTRUN")
  ops.log  <- c("check.illCond", "check.design")
  default  <- list(check.cor.method="spearman", check.illCond=TRUE, 
                   check.cor.cutoff=0.97, check.nsubjects=25, 
                   check.design=TRUE, max.nstrata=10,
                   model=getCorrModelName(), 
                   DEBUG=0, DONOTRUN=0)
  default[[out.eff]]    <- getOutEffectsOpDefault()
  default[[out.modSum]] <- getOutModSumOpDefault()
  default[[add.ci]]     <- getAddCiOpDefault()
  default[[out.metabs]] <- getAddMetabColsDefault()

  # Be careful with exp.parms option, as it can be NULL
  defval <- getExpParmsOpDefault() 
  if (!length(defval)) {
    nms            <- names(default)
    default        <- c(default, list(NULL))
    names(default) <- c(nms, exp.parms)  
  } else {
    default[[exp.parms]] <- defval 
  }
  valid <- names(default)

  list(ops.character=ops.char, ops.numeric=ops.num, ops.logical=ops.log,
       valid=valid, default=default, ops.charVec=ops.charVec)

} # END: getValidGlobalOps 

getCharVecFromStr <- function(str, sep) {

  ret <- strsplit(str, sep, fixed=TRUE)
  ret <- unlist(ret)
  ret

} # END: getCharVecFromStr

# Parse and check global options from options sheet. Return a list of options.
checkGlobalOpsFromCharVecs <- function(opnames, opvalues) {

  tmp    <- getValidGlobalOps()
  def    <- tmp$default
  ops.n  <- tmp$ops.numeric
  ops.l  <- tmp$ops.logical
  ops.cv <- tmp$ops.charVec
  valid  <- names(def)

  n <- length(opnames)
  if (!n) return(def)

  opnames  <- trimws(opnames)
  opvalues <- trimws(opvalues)
  sep      <- getAddMetabColsSep()

  ret <- list()
  # Loop over each element
  eq <- getOpStrEq()
  for (i in 1:n) {
    name  <- opnames[i]
    value <- opvalues[i]
    if (!(name %in% valid)) stop(paste("ERROR: ", name, " is not a valid option", sep=""))
    if (!nchar(value)) {
      # An error was originally thrown, but perhaps set to NULL so it will be assigned the default value
      #stop(paste("ERROR: ", name, "=", value, " is not valid", sep=""))
      value <- NULL
    } else {  
      # value must be converted to the correct type
      if (name %in% ops.n) {
        value <- as.numeric(value)
      } else if (name %in% ops.l) {
        # Logical value
        value <- getLogicalValueFromStr(value) 
      } else if (name %in% ops.cv) {
        # Character vector
        value <- getCharVecFromStr(value, sep) 
      }
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
  ops.cv  <- tmp$ops.charVec

  # Special cases
  spec1 <- getAddMetabColsOpName() 

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
      } else if (nm == spec1) {
        tmp     <- try(checkOp_output.metab.cols(val), silent=TRUE)
        op[[i]] <- tmp
      } else {
        tmp <- try(eval(parse(text=paste("checkOp_", nm, "('", val, "')", sep=""))),
            silent=TRUE)
      }
      if ("try-error" %in% class(tmp)) {
        print(tmp)
        if (length(val) > 1) val <- paste0(val, collapse=",")
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

checkOp_output.Effects <- function(str, name=NULL) {

  if (!isString(str)) stop("INTERNAL CODING ERROR in checkOp_output.Effects")
  valid <- tolower(getOutEffectsOpVals())
  str   <- tolower(str)
  str   <- check.string(str, valid, getOutEffectsOpName())
  str

} 

checkOp_output.ModelSummary <- function(str, name=NULL) {

  if (!isString(str)) stop("INTERNAL CODING ERROR in checkOp_output.ModelSummary")
  valid <- tolower(getOutModSumOpVals())
  str   <- tolower(str)
  str   <- check.string(str, valid, getOutModSumOpName())
  str

} 

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

check.range <- function(x, name, lower, upper, upper.inc=TRUE) {

  err <- 0
  n   <- length(x)
  op1 <- ">= "
  op2 <- "<= "
  if (!n || (n > 1) || !is.finite(x)) {
    err <- 1
  } else {
    if ((x < lower) || (x > upper)) err <- 1
    if (!upper.inc && (x == upper)) {
      err <- 1
      op2 <- "< "
    }
  }
  if (err) {
    if (is.finite(upper)) {
      msg <- paste("ERROR: ", name, " must be ", op1,  
                   lower, " and ", op2, upper, sep="")
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

check.varnameVec <- function(x, name, min.len=0, tolower=1, returnOnMiss=NULL) {

  len <- length(x)
  if (!len) {
    if (min.len) stop(paste0("ERROR: length(", name, ") = 0"))
    return(NULL) 
  }
  if (!is.character(x)) stop(paste0("ERROR: ", name, " must be a character vector"))
  if (!is.vector(x)) stop(paste0("ERROR: ", name, " must be a character vector"))
  x <- trimws(x)
  if (tolower) x <- tolower(x)
  tmp <- nchar(x) > 0
  x   <- x[tmp]
  if (!length(x)) return(returnOnMiss)
  x

}

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
checkOp_DONOTRUN <- function(x) {
  check.logical(x, "DONOTRUN") 
}
checkOp_model <- function(x) {
  runModel.check.model(x)
} 
checkOp_output.exp_parms <- function(x) {

  if (!length(x)) {
    ret <- NULL
  } else {
    ret <- check.logical(x, "output.exp_parms") 
  }
  ret
} 
checkOp_output.ci_alpha <- function(x) {
  check.range(x, "output.ci_alpha", 0, 1, upper.inc=FALSE)
}
checkOp_output.metab.cols <- function(x) {

  ret <- check.varnameVec(x, getAddMetabColsOpName(), min.len=0, tolower=1, returnOnMiss="")
  ret
}



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
