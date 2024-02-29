#' A list of 19:
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
#' \item{\code{max.npairwise}}{ The maximum number of metabolites to process the 
#'                             "all pairwise correlations" model.
#'                    The default is 1000.}
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
#' \item{\code{output.exp_parms}}{ TRUE, FALSE or NULL to exponentiate parameter estimates. 
#'                         Standard errors are obtained from the delta method. 
#'                         The default is NULL, so that estimates from logistic regression and survival models
#'                         will be exponentiated, and not otherwise.}
#' \item{\code{output.metab.cols}}{ Character vector of column names in the \code{METABOLITES}
#'                             sheet to be output in the \code{\link{ModelSummary}} and \code{\link{Effects}}
#'                             data frames. Metabolite ids are matched first using the 
#'                             \code{outcomespec} column and then using the \code{exposurespec} column.  
#'                         The default is "metabolite_name".}
#' \item{\code{output.ModelSummary}}{ A string to defines the columns output in the returned 
#'                            \code{\link{ModelSummary}} data frame. Currently, it must be "anova" or "all".
#'                            This option is ignored with \code{model = "correlation"}.
#'                            The default is "anova".}
#' \item{\code{output.type}}{ "rda" or "xlsx" to define the type of output file(s) when \code{\link{runAllModels}} is called.\cr
#'                            See \code{output.common.cols} and \code{output.merge}. \cr
#'                            The default is "xlsx".}
#' \item{\code{method.adjPvalue}}{ Method for adjusted p-values. It must be one of:
#'    "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none".
#'   Adjusted p-values are only computed for models with a single outcome variable or
#'   a single exposure variable.
#'                            The default is 'fdr'.}
#' \item{\code{chemEnrich}}{ 0 or 1 to run a chemical class enrichment (0=no, 1=yes) using RaMP.
#'                            The default is 0.}
#' \item{\code{chemEnrich.adjPvalue}}{ The BH-adjusted p-value cutoff to select metabolites for 
#'                             chemical class enrichment.
#'                            The default is 0.05.}
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
  if (!is.list(op)) stop(msg_arg_notList("op")) 
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
    if (op$coxphFlag || op$clogitFlag) val <- TRUE
    op[[nm]] <- val
  } 

  # Flag set for efficiency in the model tidy functions
  op$out.eff.exp <- op[[getOutEffectsOpName()]] == getOutEffectsOpDefault()

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
    if (!len) stop(msg_mod_19(model))
    if (len > 2) stop(msg_mod_20(model))
    mop$n.time.vars <- len
    mop$time1.var   <- timecov[1]
    if (len > 1) mop$time2.var <- timecov[2]
    mop$timeFlag <- 1
  }
  if (op$clogitFlag) {
    # Add info for group
    groupcov <- modeldata[["groupcov", exact=TRUE]]
    len      <- length(groupcov)
    if (!len) stop(msg_mod_21(model))
    if (len > 1) stop(msg_mod_22(model))
    mop$group.var <- groupcov
    mop$groupFlag <- 1
  }

  op[[nm]] <- mop

  op

} # END: checkModelOptions

getAllOptionNames <- function() {

  tmp   <- getValidGlobalOps()$valid
  valid <- c(tmp, getModelOpsName(), getRampOpName())

  valid

} # END: getAllOptionNames

getValidGlobalOps <- function(meta=0) {

  if (meta) return(getValidGlobalMetaOps())

  out.eff    <- getOutEffectsOpName()
  out.modSum <- getOutModSumOpName()
  add.ci     <- getAddCiOpName()
  exp.parms  <- getExpParmsOpName() 
  out.metabs <- getAddMetabColsOpName()
  out.type   <- getOutTypeOpName()
  out.common <- getOutCommonColsOpName()
  out.merge  <- getOutMergeOpName()
  chemEnrich <- getRampCallChemEnrichOpName()
  cE.pval    <- getRampPvalOpName()
  allpair    <- getMaxNpairwiseOpName()
  methadj    <- getOpMethodAdjPvalue()
  #miss.m     <- getMissMetabOpName()
  #miss.d     <- getMissDataOpName()

  ops.char    <- c("model", "check.cor.method", out.eff, out.modSum,
                   out.type, out.merge, methadj)
  ops.charVec <- c(out.metabs)
  ops.num     <- c("check.cor.cutoff", "check.nsubjects", "max.nstrata", 
                   add.ci, exp.parms, cE.pval, allpair, 
                   "DEBUG", "DONOTRUN")
  ops.log  <- c("check.illCond", "check.design", out.common, chemEnrich)
  default  <- list(check.cor.method="spearman", check.illCond=TRUE, 
                   check.cor.cutoff=0.97, check.nsubjects=25, 
                   check.design=TRUE, max.nstrata=10,
                   model=getCorrModelName(), 
                   DEBUG=0, DONOTRUN=0)
  default[[out.eff]]    <- getOutEffectsOpDefault()
  default[[out.modSum]] <- getOutModSumOpDefault()
  default[[add.ci]]     <- getAddCiOpDefault()
  default[[out.metabs]] <- getAddMetabColsDefault()
  default[[out.type]]   <- getOutTypeOpDefault()
  default[[out.common]] <- getOutCommonColsOpDefault()
  default[[out.merge]]  <- getOutMergeOpDefault()
  default[[chemEnrich]] <- getRampCallChemEnrichOpDefault()
  default[[cE.pval]]    <- getRampPvalOpDefault()
  default[[allpair]]    <- getMaxNpairwiseOpDefault()
  default[[methadj]]    <- getOpMethodAdjPvalueDefault()
  
  # Be careful with options that have NULL as the default value
  default <- addNamedValueToList(default, exp.parms, getExpParmsOpDefault())
  #default <- addNamedValueToList(default, miss.m,    getMissMetabOpDefault())
  #default <- addNamedValueToList(default, miss.d,    getMissDataOpDefault())
  valid   <- names(default)

  list(ops.character=ops.char, ops.numeric=ops.num, ops.logical=ops.log,
       valid=valid, default=default, ops.charVec=ops.charVec)

} # END: getValidGlobalOps 

getCharVecFromStr <- function(str, sep, remWhiteSpace=1) {

  ret <- strsplit(str, sep, fixed=TRUE)
  ret <- unlist(ret)
  if (remWhiteSpace) ret <- trimws(ret)
  ret

} # END: getCharVecFromStr

# Parse and check global options from options sheet. Return a list of options.
checkGlobalOpsFromCharVecs <- function(opnames, opvalues, meta=0) {

  tmp      <- getValidGlobalOps(meta=meta)
  def      <- tmp$default
  ops.n    <- tmp$ops.numeric
  ops.l    <- tmp$ops.logical
  ops.cv   <- tmp$ops.charVec
  valid    <- names(def)
  n <- length(opnames)
  if (!n) return(def)

  opnames  <- trimws(opnames)
  opvalues <- trimws(opvalues)
  sep      <- getAddMetabColsSep()

  # Special case
  nm    <- metaOp_strataToExcludeFromHetTest()
  sp1   <- c(nm, paste0(nm, ".", 1:999))
  flag1 <- 0
  if (meta) valid <- c(valid, sp1)

  ret <- list()
  # Loop over each element
  eq <- getOpStrEq()
  for (i in 1:n) {
    name  <- opnames[i]
    value <- opvalues[i]

    if (!(name %in% valid)) stop(msg_mod_23(name))
    if (!nchar(value)) {
      # An error was originally thrown, but perhaps set to NULL so it will be assigned the default value
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
      } else if (name %in% sp1) {
        value <- meta_excStratOpStr2List(value, name)
        flag1 <- 1
      }
    }

    #ret[[name]] <- value
    ret <- addNamedValueToList(ret, name, value)
  }

  # Special case
  if (flag1) ret <- meta_excStratOp_setList(ret)

  # Check the values
  ret <- try(checkGlobalOpList(ret, name="options", meta=meta), silent=TRUE)

  ret

} # END: checkGlobalOpsFromCharVecs

checkGlobalOpList <- function(op, name="options", meta=0) {

  n       <- length(op)
  if (n && !is.list(op)) stop(msg_arg_notList(name))
  tmp     <- getValidGlobalOps(meta=meta)
  default <- tmp$default
  valid   <- names(default)
  ops.n   <- c(tmp[["ops.numeric", exact=TRUE]], tmp[["ops.logical", exact=TRUE]])
  ops.cv  <- tmp[["ops.charVec", exact=TRUE]]
  ops.lst <- tmp[["ops.list", exact=TRUE]]
  if (meta) {
    FSTR <- "checkMetaOp_"
  } else {
    FSTR <- "checkOp_"
  }

  # Special cases
  spec1 <- getAddMetabColsOpName() 
  spec2 <- metaOp_cohorts.include()
  spec3 <- metaOp_cohorts.exclude()
  spec4 <- metaOp_strataToExcludeFromHetTest()

  # Check the names and values
  if (n) {
    checkOptionListNames(op, valid, name)
    nms <- names(op) 

    for (i in 1:n) {
      nm  <- nms[i]
      val <- op[[i]]
      if (!length(val)) next

      if (nm %in% ops.n) {
        tmp <- try(eval(parse(text=paste(FSTR, nm, "(", val, ")", sep=""))),
                   silent=TRUE)
      } else if (nm == spec1) {
        tmp     <- try(checkOp_output.metab.cols(val), silent=TRUE)
        op[[i]] <- tmp
      } else if (nm == spec2) {
        tmp     <- try(checkMetaOp_cohorts.include(val), silent=TRUE)
        op[[i]] <- tmp
      } else if (nm == spec3) {
        tmp     <- try(checkMetaOp_cohorts.exclude(val), silent=TRUE)
        op[[i]] <- tmp
      } else if (nm == spec4) {
        tmp     <- try(checkMetaOp_strata.exclude.het.test(val), silent=TRUE)
        op[[i]] <- tmp
      } else if (nm %in% ops.cv) {
        tmp     <- try(checkOp_CHARVEC(val, nm), silent=TRUE)
        op[[i]] <- tmp
      } else {
        tmp <- try(eval(parse(text=paste(FSTR, nm, "('", val, "')", sep=""))),
            silent=TRUE)
      }
      if ("try-error" %in% class(tmp)) {
        print(tmp)
        if (length(val) > 1) val <- paste0(val, collapse=",")
        stop(msg_arg_opEqValNotValid(c(nm, val)))
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

checkOp_method.adjPvalue <- function(obj) {

  valid <- c("holm", "hochberg", "hommel", "bonferroni", 
             "BH", "BY", "fdr", "none")
  obj   <- check.string(obj, valid, getOpMethodAdjPvalue()) 
  
  obj

}

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

check.variableInData <- function(x, name, data, varMap, numeric=1, positive=0) {

  if (!isString(x)) stop(msg_arg_colNotValid(name))
  vec <- NULL
  if (x %in% colnames(data)) vec <- data[, x, drop=TRUE]
  if (!length(vec)) {
    new <- (names(varMap))[varMap %in% x]  
    if (new %in% colnames(data)) vec <- data[, new, drop=TRUE]
  }
  if (!length(vec)) stop(msg_arg_colNotValid(name))
  if (numeric) {
    if (!is.numeric(vec)) stop(msg_arg_colNumNotValid(name))
  }
  if (positive) {
    tmp <- vec < 0
    tmp[is.na(tmp)] <- FALSE
    if (any(tmp)) stop(msg_arg_colNegNotValid(name))
  }
  x

} # END: check.variableInData

check.varnameVec <- function(x, name, min.len=0, tolower=1, returnOnMiss=NULL) {

  len <- length(x)
  if (!len) {
    if (min.len) stop(msg_arg_len0(name))
    return(NULL) 
  }
  if (!is.character(x)) stop(msg_arg_notCharVec(name))
  if (!is.vector(x)) stop(msg_arg_notCharVec(name))
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
checkOp_output.common.cols <- function(x) {

  if (!length(x)) {
    ret <- getOutCommonColsOpDefault()
  } else {
    ret <- check.logical(x, getOutCommonColsOpName()) 
  }
  ret
} 
checkOp_output.type <- function(x) {
  if (!length(x)) {
    ret <- getOutTypeOpDefault()
  } else {
    ret <- check.string(x, getOutTypeOpVals(), getOutTypeOpName())
  }
  ret
}
checkOp_output.merge <- function(x) {
  if (!length(x)) {
    ret <- getOutMergeOpDefault()
  } else {
    ret <- check.string(x, getOutMergeOpVals(), getOutMergeOpName())
  }
  ret
}
checkOp_chemEnrich <- function(x) {

  if (!length(x)) {
    ret <- getRampCallChemEnrichOpDefault()
  } else {
    ret <- check.logical(x, getRampCallChemEnrichOpName()) 
  }
  ret
} 
checkOp_chemEnrich.adjPvalue <- function(x) {
 if (!length(x)) {
    ret <- getRampPvalOpDefault()
  } else {
    ret <- check.range(x, "chemEnrich.adjPvalue", 0, 1)
  }
  ret
} 
checkOp_max.npairwise <- function(x) {
  check.range(x, "max.npairwise", 2, Inf)
}

checkOp_CHARVEC <- function(x, nm) {

  if (!length(x)) return(NULL)
  check.vector(x, nm, min.len=0, len=0)
  x
}

checkMetaFiles <- function(filevec, name="filevec", valid.ext=NULL) {

  filevec <- unique(checkFiles(filevec, name=name))
  if (is.null(valid.ext)) valid.ext <- getMetaValidExt()

  # Check extensions
  dotExt <- paste0(".", valid.ext)
  ok     <- checkFileExtensions(filevec, checkForExt=dotExt)
  if (!all(ok)) {
    print(filevec[!ok])
    stop(msg_meta_33())
  }
  filevec
}

checkMetaFilesFolders <- function(x, name="filesFolders") {

  x     <- checkFiles(x, name=name)
  tmp   <- dir.exists(x)
  dirs  <- x[tmp]  
  files <- x[!tmp]
  if (length(files)) {
    # Check extensions
    dotExt <- paste0(".", getMetaValidExt())
    ok     <- checkFileExtensions(files, checkForExt=dotExt)
    if (!all(ok)) {
      print(files[!ok])
      stop(msg_meta_32())
    }
  }
  x
}

getOptionNameAndValue <- function(str, sep="=") {

  tmp <- parseStr(str, sep=sep)
  if (length(tmp) < 2) {
    msg <- msg_arg_opNotValid(str)
    stop(msg)
  }
  name  <- tmp[1]
  value <- getOpValueFromVec(tmp[-1], sep=sep)

  list(name=name, value=value)

} # END: getOptionNameAndValue

getOpValueFromVec <- function(strVec, sep="=") {

  paste(strVec, collapse=sep, sep="")

} # END: getOpValueFromVec
