#' @name meta_opfile
#' @title Options file for meta-analyses
#' @description An Excel file containing models and options for the runAllMeta function
#' @details
#' The file should contain sheets \bold{MODELS} and \bold{MODEL_TYPES}. 
#' Each sheet is optional. The \bold{MODELS} sheet should have a column
#' called \code{MODEL} containing the models for meta-analysis. 
#' This list of models can be a subset of models defined by the input files.
#' This sheet can also have a optional column called \code{MODEL_TYPE} that
#' links to the \bold{MODEL_TYPES} sheet defining the options for each model.
#' See the example file in /inst/extdata/cometsMetaInput.xlsx.
#'
NULL

meta_setOptions <- function(op, model) {

  ret   <- list()
  tmp   <- getValidGlobalMetaOps()
  valid <- tmp$valid
  tmp   <- valid %in% names(op)
  if (any(tmp)) ret <- op[valid[tmp]]

  gop <- getModelTypeOptionsFromSheet(op, model)
  if (length(gop)) {
    nms <- names(gop)
    for (nm in nms) ret[[nm]] <- gop[[nm]]
  }

  ret[[metaOp_models()]] <- model
  ret
}

getModelTypeOptionsFromSheet <- function(op, model) {

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
    stop(paste0("ERROR: missing option names in ", getMetaModelTypeSheetName(), " sheet"))
  }

  # Check for duplicate names. metaOp_strataToExcludeFromHetTest is a special case
  tmp0 <- opnames %in% metaOp_strataToExcludeFromHetTest()
  tmp  <- (duplicated(opnames)) & !tmp0
  if (any(tmp)) {
    str <- paste(opnames[tmp], collapse=", ", sep="")
    msg <- paste0("ERROR: the options ", str, " appear more than once in the ",
                  getMetaModelTypeSheetName(), " sheet")
    stop(msg)
  }

  # Special case: metaOp_strataToExcludeFromHetTest
  m <- sum(tmp0) 
  if (m > 1) {
    # Temporarily rename option to prevent overwriting the option later
    opnames[tmp0] <- paste0(opnames[tmp0], ".", 1:m)
  }

  # Get the global options
  ret <- NULL
  tmp <- opnames %in% getValidGlobalMetaOps()$valid
  if (any(tmp)) {
    g.opnames  <- opnames[tmp]
    g.opvalues <- opvalues[tmp]
    tmp        <- checkGlobalOpsFromCharVecs(g.opnames, g.opvalues, meta=1)
    if ("try-error" %in% class(tmp)) stop(tmp)
    ret <- tmp[g.opnames]
  }
  
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

  op
}

meta_readOpFile <- function(opFile) {

  ret         <- list()
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
  if (!isString(x)) stop(paste0("ERROR: ", nm, " must be an Excel file"))
  if (!isExcelFile(x)) stop(paste0("ERROR: ", nm, " must be an Excel file"))
  x <- checkFiles(x, name=nm)
  sheets <- try(readxl::excel_sheets(x))
  if ("try-error" %in% class(sheets)) {
    stop(paste0("ERROR: check that ", nm, " is an Excel file"))
  }

  x
}

meta_excStratOpStr2List <- function(str, name) {

  vec <- strsplit(str, ":", fixed=TRUE)
  vec <- trimws(vec)
  tmp <- nchar(vec) > 0
  vec <- vec[tmp]
  if (length(vec) != 2) stop(paste0(name, " = ", str, " is not correctly specified"))
  var        <- tolower(trimws(vec[1]))
  values     <- strsplit(vec[2], ",", fixed=TRUE)
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

checkMetaOp_min.n.cohort <- function(x) {

  if (!length(x)) {
    ret <- metaOp_minNcohortDefault()
  } else {
    check.range(x, metaOp_minNcohortName(), 2, Inf)
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

checkMetaOp_DEBUG <- function(x) {
  checkOp_DEBUG(x)
}

