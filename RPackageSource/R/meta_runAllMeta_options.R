#' @name meta_opfile
#' @title Options file for meta-analyses
#' @description An Excel file containing models and options for the \code{\link{runAllMeta}} function
#' @details
#' The file can contain sheets \bold{META_MODELS} and \bold{META_TYPES}. 
#' Each sheet is optional. The \bold{META_MODELS} sheet should have a column
#' called \code{MODEL} containing the models that the user wants to run 
#' meta-analyses for. 
#' If the \bold{MODELS} sheet is not given then meta-analyses for all models
#'  will be run.
#' This sheet can also have an optional column called \code{META_TYPE} that
#' links to the \code{META_TYPE} column in the \bold{META_TYPES} sheet to define
#' the options for each meta-analysis model.
#' The \bold{META_TYPES} sheet should contain columns \code{META_TYPE},  \code{OPTION},
#' and \code{VALUE}. 
#' See the example file in /inst/extdata/cometsMetaInput.xlsx.
#' If the \bold{META_TYPES} sheet is not specified, then the default values for all
#'  meta-analysis options will be used.
# The \bold{META_TRANSFORM} sheet allows users to transform files of results so that
# a meta-analysis can be run. For example a file can be transformed to change the
# reference category of a categorical exposure variable, or to change the the
# category levels of a stratification or exposure variable so that the levels are 
# consistent with other files of results. This sheet should have columns \code{FILE},
# \code{OPTION}, and \code{VALUE}. The \code{FILE} column has the full name or base name
# of the file to be transformed. For the complete list of transformation options,
# see \code{\link{file.list}}.
#' 
NULL

meta_setOptions <- function(op, model) {

  tmp   <- getValidGlobalMetaOps()
  valid <- tmp$valid
  ret   <- tmp$default
  tmp   <- valid %in% names(op)
  vv    <- valid[tmp]
  if (length(vv)) {
    for (v in vv) ret[[v]] <- op[[v, exact=TRUE]]
  }
  gop <- getModelTypeOptionsFromSheet(op, model)
  if (length(gop)) {
    nms <- names(gop)
    for (nm in nms) ret[[nm]] <- gop[[nm, exact=TRUE]]
  }

  ret[["MODEL"]] <- model

  ret
}

getModelTypeOptionsFromSheet <- function(op, model) {

  DEBUG <- op$DEBUG
  if (DEBUG) cat("Begin: getModelTypeOptionsFromSheet\n")

  modelTypes <- op[["modelTypes", exact=TRUE]]
  if (!length(modelTypes)) return(NULL)
  opTable   <- op[["modelTypes.DF", exact=TRUE]]
  if (!length(opTable)) return(NULL)
  models     <- op[["models", exact=TRUE]]
  if (!length(models)) stop("INTERNAL CODING ERROR 1")

  tmp   <- meta_normModelStr(models) %in% meta_normModelStr(model)
  if (sum(tmp) != 1) stop("INTERNAL CODING ERROR 2")
  mtype <- modelTypes[tmp]
  if (mtype %in% c(NA, "")) return(NULL)

  typeCol   <- tolower(getMetaOpFileModelTypeCol())
  opNameCol <- tolower(getOptionNameCol())
  opValCol  <- tolower(getOptionValueCol())
  tmp       <- opTable[, typeCol] %in% mtype
  opTable   <- opTable[tmp, , drop=FALSE]
  if (!nrow(opTable)) return(NULL)

  opTable   <- unique(opTable[, c(opNameCol, opValCol), drop=FALSE])
  opnames   <- opTable[, opNameCol, drop=TRUE]
  opvalues  <- opTable[, opValCol, drop=TRUE]

  # Remove ones with no name and no value
  tmp <- (nchar(opnames) < 1) & (nchar(opvalues) < 1)
  if (any(tmp)) {
    opnames  <- opnames[!tmp]
    opvalues <- opvalues[!tmp]
  }
  if (!length(opnames)) return(NULL)
 
  # Check for missing names
  if (any(nchar(opnames) < 1)) {
    stop(msg_metaop_1())
  }

  # Check for duplicate names. metaOp_strataToExcludeFromHetTest is a special case
  tmp0 <- opnames %in% metaOp_strataToExcludeFromHetTest()
  tmp  <- (duplicated(opnames)) & !tmp0
  if (any(tmp)) {
    str <- paste(opnames[tmp], collapse=", ", sep="")
    msg <- msg_metaop_2(str)
    stop(msg)
  }

  # Special case: metaOp_strataToExcludeFromHetTest
  opnames0 <- opnames
  m        <- sum(tmp0) 
  if (m > 1) {
    # Temporarily rename option to prevent overwriting the option later
    opnames[tmp0] <- paste0(opnames[tmp0], ".", 1:m)
  }

  # Get the global options
  ret   <- NULL
  valid <- getValidGlobalMetaOps()$valid
  tmp   <- (opnames %in% valid) | tmp0
  if (any(tmp)) {
    g.opnames  <- opnames[tmp]
    g.opvalues <- opvalues[tmp]
    lst        <- checkGlobalOpsFromCharVecs(g.opnames, g.opvalues, meta=1)
    if ("try-error" %in% class(lst)) stop(lst)
    # Get the options we need
    tmp <- opnames0 %in% valid
    nms <- unique(opnames0[tmp])
    if (length(nms)) {
      ret <- lst[nms]
    } else {
      if (DEBUG) {
        print("POSSIBLE ERROR:")
        print(cbind(g.opnames, g.opvalues))
      } 
      ret <- list()
    }
  }

  if (DEBUG) cat("End: getModelTypeOptionsFromSheet\n")
  ret
} 

meta_readAndSetGlobalOps <- function(opFile) {

  if (is.null(opFile)) return(meta_check_op(NULL))
  op  <- meta_readOpFile(opFile)
  gop <- NULL

  # Get GLOBAL options
  opTable <- op[["modelTypes.DF", exact=TRUE]]
  if (length(opTable)) {
    gop <- getGlobalOptionsFromSheet(opTable, meta=1)
    nms <- names(gop)
    len <- length(nms)
    if (len) {
      for (nm in nms) op[[nm]] <- gop[[nm]]
    }
  }
  if (!length(op[["DEBUG", exact=TRUE]])) op$DEBUG <- 0

  op
}

meta_readOpFile <- function(opFile) {

  ret         <- list()

  # If there is a transform sheet, read in and convert to a named list.
  # The names in the list will be the file base names.
  ret$transform <- meta_readTransformSheet(opFile) 

  sheets      <- try(readxl::excel_sheets(opFile))
  modelsSheet <- getMetaModelsSheetName()
  nm          <- tolower(getMetaOpFileModelsCol())
  nmt         <- tolower(getMetaOpFileModelTypeCol())
  if (toupper(modelsSheet) %in% toupper(sheets)) {
    x <- readExcelSheet(opFile, modelsSheet, sheets, optional=1) 
    if (!nonEmptyDf(x)) return(ret)
    cx <- tolower(colnames(x))
    colnames(x) <- cx 
    models      <- NULL
    modelTypes  <- NULL
    if (tolower(nm) %in% cx) {
      models <- trimws(unlist(x[, nm, drop=TRUE]))
      if (tolower(nmt) %in% cx) modelTypes <- unlist(x[, nmt, drop=TRUE])
      if (is.null(modelTypes)) modelTypes <- rep("", length(models))
      tmp        <- nchar(models) > 0
      models     <- models[tmp]
      modelTypes <- modelTypes[tmp]
    } 
    if (!length(models)) {
      models     <- NULL
      modelTypes <- NULL
    } 
    ret$models     <- models
    ret$modelTypes <- modelTypes
  }

  # Read in the options sheet 
  typeSheet <- getMetaModelTypeSheetName()
  x         <- NULL
  if (toupper(typeSheet) %in% toupper(sheets)) {
    x    <- readExcelSheet(opFile, typeSheet, sheets, optional=1) 
    cols <- c(nmt, getOptionNameCol(), getOptionValueCol())
    if (!nonEmptyDfHasCols(x, cols, allcols=1, ignoreCase=1)) x <- NULL
    if (length(x)) {
      x           <- x[, cols, drop=FALSE]
      cx          <- tolower(colnames(x))
      colnames(x) <- cx
      # Remove white space, set missing values to ""
      for (i in 1:ncol(x)) {
        x[, i] <- trimws(x[, i])
        tmp    <- is.na(x[, i])
        if (any(tmp)) x[tmp, i] <- ""
      }
      # For missing ids, set to previous id
      len    <- nchar(x[, nmt, drop=TRUE])
      tmp    <- len < 1
      tmp[1] <- FALSE # cannot be first row
      if (any(tmp)) {
        rows <- (1:nrow(x))[tmp]
        for (row in rows) {
          x[row, nmt] <- x[row-1, nmt, drop=TRUE]
        }
      } 
    } 
  }
  ret$modelTypes.DF <- x

  ret
}

meta_check_opFile <- function(x, nm="opfile") {

  if (is.null(x)) return(NULL)
  if (!isString(x)) stop(msg_metaop_3(nm))
  if (!isExcelFile(x)) stop(msg_metaop_3(nm))
  x <- checkFiles(x, name=nm)
  sheets <- try(readxl::excel_sheets(x))
  if ("try-error" %in% class(sheets)) {
    stop(msg_metaop_4(nm))
  }

  x
}

meta_excStratOpStr2List <- function(str, name) {

  vec <- unlist(strsplit(str, metaOp_stratHet.argEq(), fixed=TRUE))
  vec <- trimws(vec)
  tmp <- nchar(vec) > 0
  vec <- vec[tmp]

  if (length(vec) != 2) stop(msg_metaop_5(c(name, str)))

  var        <- tolower(trimws(vec[1]))
  values     <- parseStr(vec[2], sep=metaOp_stratHet.vecSep())
  if (!length(values)) stop(msg_metaop_5(c(name, str)))
  ret        <- list()
  ret[[var]] <- values

  ret
}

meta_excStratOp_setList <- function(oplist) {

  nms <- names(oplist)
  if (!length(nms)) return(oplist)
  nm  <- metaOp_strataToExcludeFromHetTest()
  sp1 <- c(nm, paste0(nm, ".", 1:999)) 
  tmp <- nms %in% sp1
  sp1 <- nms[tmp]
  m   <- length(sp1)
  if (!m) return(oplist)
  lst <- list()

  for (nm2 in sp1) {
    obj <- oplist[[nm2, exact=TRUE]]
    if (is.null(obj)) next
    var           <- names(obj)
    val           <- obj[[1]]
    lst[[var]]    <- val
    oplist[[nm2]] <- NULL
  }
  if (!length(oplist)) oplist <- list()
  oplist[[nm]] <- lst
  oplist
}

##########################################################
# Function to read the "transform file" sheet into a list
##########################################################

meta_readTransformSheet <- function(f) {

  # Sheet with cols file, option, value  
  # A blank file field will assume it is for the previous non-blank file field


  if (!length(f)) return(NULL)  
  
  sheet <- getMetaTransSheetName()
  x     <- readExcelSheet(f, sheet, NULL, stopOnError=1, optional=1)
  if (!length(x)) return(NULL)

  # x should have correct columns
  fcol   <- tolower(getMetaTransFileCol())
  opcol  <- tolower(getMetaTransOptionCol())
  valcol <- tolower(getMetaTransValueCol())
  cols   <- c(fcol, opcol, valcol)
  if (!nonEmptyDfHasCols(x, cols, allcols=1, ignoreCase=1)) {
    warning(paste0("Sheet ", sheet, " is ignored"))
    return(NULL)
  }
  colnames(x) <- tolower(colnames(x))

  for (col in cols) {
    vec <- trimws(x[, col])
    tmp <- is.na(vec)
    if (any(tmp)) vec[tmp] <- ""
    tmp <- nchar(vec) < 1
    if (any(tmp)) vec[tmp] <- ""
    vec      <- gsub("'", "", vec, fixed=TRUE)
    vec      <- gsub('"', "", vec, fixed=TRUE)
    x[, col] <- vec
  }

  # Walk through file names and set missing to previous non-missing
  x[, fcol] <- setMissValToPrev(x[, fcol, drop=TRUE], miss="")

  # Remove rows with no file, option or value
  tmp <- (x[, fcol, drop=TRUE] %in% "")  | 
         (x[, opcol, drop=TRUE] %in% "") | 
         (x[, valcol, drop=TRUE] %in% "")
  x   <- x[!tmp, , drop=FALSE]
  if (!nrow(x)) stop(paste0("ERROR: all rows from sheet ", sheet, " have beed removed")) 

  filevec <- x[, fcol, drop=TRUE]
  opvec   <- x[, opcol, drop=TRUE]
  valvec  <- x[, valcol, drop=TRUE]
  rm(x); gc()
  
  # Get options for each unique file
  filevec <- basename(filevec)
  ufiles  <- unique(filevec)
  nfiles  <- length(ufiles)
  ret     <- list()

  for (i in 1:nfiles) {
    file <- ufiles[i]
    tmp  <- filevec %in% file
    lst  <- meta_readTransParseFileOps(opvec[tmp], valvec[tmp])
    if (length(lst)) ret[[file]] <- lst
  }
  if (!length(ret)) ret <- NULL

  ret
}

meta_readTransParseFileOps <- function(opvec, valvec) {

  # opvec, valvec  character vector of the option names and values
  n <- length(opvec)
  if (n != length(valvec)) stop("INTERNAL CODING ERROR 1")
  if (!n) return(NULL)

  # Numeric options
  ops.numeric <- c(dfToC_nobs())

  # special cases
  sp1 <- dfToC_change.col.values()
  sp2 <- dfToC_where()
  sep <- getWhereSep() 

  ret        <- list()
  changeList <- list()
  for (i in 1:n) {
    nm <- opvec[i]
    if (nm == sp1) {
      tmp <- meta_readTransParseChangeCol(valvec[i])
      if (length(tmp)) changeList[[length(changeList) + 1]] <- tmp
    } else if (nm == sp2) {
      ret[[nm]] <- parseStr(valvec[i], sep=sep)
    } else if (nm %in% ops.numeric) {
      ret[[nm]] <- as.numeric(valvec[i])
    } else {
      ret[[nm]] <- trimws(valvec[i])
    }
  }
  if (length(changeList)) ret[[sp1]] <- changeList
  ret
}

meta_readTransParseChangeCol <- function(str) {

  col  <- dfToC_change.col()
  old  <- dfToC_change.old()
  new  <- dfToC_change.new()
  sep  <- dfToC_change.sep()              
  vsep <- dfToC_change.vec.sep()
  eq   <- dfToC_change.argEq()
  nm   <- dfToC_change.col.values()
  str2 <- getQuotedVarStr(str)
  
  vec  <- parseStr(str, sep=sep)
  n    <- length(vec)
  if (n != 3) stop(paste0("ERROR: ", nm, " = ", str2, " is not valid"))
  valid <- c(col, old, new)
  ret   <- list()
  for (i in 1:n) {
    v2 <- parseStr(vec[i], sep=eq)
    if (length(v2) != 2) stop(paste0("ERROR: ", nm, " = ", str2, " is not valid"))
    arg  <- v2[1]
    vals <- v2[2]
    if (!(arg %in% valid)) stop(paste0("ERROR: ", nm, " = ", str2, " is not valid"))
    if (!(arg %in% col)) vals <- parseStr(vals, sep=vsep)
    ret[[arg]] <- vals   
  }
  tmp <- try(dfToComets.check_changeValues(ret, name=nm))
  if ("try-error" %in% class(tmp)) {
    stop(paste0("ERROR: ", nm, " = ", str2, " is not valid"))
  }
  ret
}

checkMetaOp_min.n.cohort <- function(x) {

  if (!length(x)) {
    ret <- metaOp_minNcohortDefault()
  } else {
    check.range(x, metaOp_minNcohortName(), 1, Inf)
    ret <- x
  }
  ret
}

checkMetaOp_min.nsub.cohort <- function(x) {

  if (!length(x)) {
    ret <- metaOp_cohortMinSubsDefault()
  } else {
    check.range(x, metaOp_cohortMinSubs(), 1, Inf)
    ret <- x
  }
  ret
}

checkMetaOp_min.nsub.total <- function(x) {

  if (!length(x)) {
    ret <- metaOp_totalMinSubsDefault()
  } else {
    check.range(x, metaOp_totalMinSubs(), 1, Inf)
    ret <- x
  }
  ret
}

checkMetaOp_cohorts.include <- function(x) {

  if (!length(x)) {
    ret <- NULL
  } else {
    ret <- x
  }
  ret
}

checkMetaOp_cohorts.exclude <- function(x) {

  if (!length(x)) {
    ret <- NULL
  } else {
    ret <- x
  }
  ret
}

checkMetaOp_output.type <- function(x) {
  checkOp_output.type(x)
}

checkMetaOp_strata.exclude.het.test <- function(x) {

  nm <- metaOp_strataToExcludeFromHetTest()
  if (length(x)) {
    check.list(x, nm, NULL)
  } else {
    x <- NULL
  }
  x
}

checkMetaOp_dups.method <- function(x) {

 x <- check.number(x, metaOp_dups.methodValues(), metaOp_dups.method())
 x  
}

checkMetaOp_stopOnFileError <- function(x) {

 x <- check.logical(x, metaOp_stopOnFileError())
 x  
}

checkMetaOp_oneModelCheck <- function(x) {

 x <- check.logical(x, metaOp_oneModelCheck())
 x  
}

checkMetaOp_add.cohort.names <- function(x) {

 x <- check.logical(x, metaOp_addCohortNamesDefault())
 x  
}

checkMetaOp_add.cohort.cols <- function(x) {

  if (!length(x)) {
    ret <- NULL
  } else {
    ret <- x
  }
  ret
}


checkMetaOp_DEBUG <- function(x) {
  checkOp_DEBUG(x)
}
checkMetaOp_MODEL <- function(x) {
  x
}
