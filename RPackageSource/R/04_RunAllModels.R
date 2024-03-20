#' This function allows users to run all models that are provided in the "Models" sheet of
#' the input Excel file.
#' @param readData list from \code{\link{readCOMETSinput}}
#' @param cohortLabel cohort label (e.g. DPP, NCI, Shanghai)
#' @param writeTofile TRUE/FALSE (whether or not to write results for each model into
#' separate xlsx files). Files are written to current directory. Default is TRUE.
#' 
#' @return A list of return objects from \code{\link{runModel}} or \code{\link{runCorr}}.
#'       The \code{ith} element in this list is the output from 
#'        the \code{ith} model run.
#'
#' @examples
#' \dontrun{
#' dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' allmodeloutput <- runAllModels(exmetabdata)
#' }
#' @export

runAllModels <- function(readData, cohortLabel="", writeTofile=TRUE) {

  mymodels  <- readData$mods$model
  mrgStrs   <- rep("", length(mymodels))
  op        <- runAllModels.getOptions(readData) 
  writeEach <- op[[getOutMergeOpName()]] == getOutMergeOpNone()
  mergeFlag <- runAllModels.getMergeFlag(op) && writeTofile
  results   <- list()

  for (j in 1:length(mymodels)) {
    i       <- mymodels[j]
    errFlag <- 0
    cat(paste0("Running ",i, "\n"))
    mymod <- try(getModelData(readData,modlabel=i))
    if (!("try-error" %in% class(mymod))) {
      myobj <- try(run1Model(mymod, readData, cohortLabel=cohortLabel))
      if ("try-error" %in% class(mymod)) errFlag <- 1
    } else {
      myobj   <- mymod
      errFlag <- 1 
    }
    if (mergeFlag) mrgStrs[j] <- runAllModels.getMergeStr(readData, mymod, j, op) 
    if (errFlag) {
      msg <- getErrorMsgFromTryError(myobj, addToEnd=NULL)
      msg <- paste0("ERROR: model ", i, " failed with message ", msg, "\n")
      cat(msg)
    }
    if (errFlag || !isValidReturnObj(myobj)) { 
      myobj   <- getResListFromError(myobj, i)
      errFlag <- 0
    } 
    if (writeTofile && !errFlag && writeEach) {
      writeObjectToFile(myobj, cohortLabel, i, op)
    } else {
      results[[i]] <- myobj
    }
  }
  if (writeTofile && !writeEach) {
    mergeModelsAndOutput(results, mymodels, mrgStrs, cohortLabel, op)
  }

  return(results)
}

# Function to call runCorr or runModel
run1Model <- function(mymod, readData, cohortLabel="") {

  flag <- mymod[[getOldCorrModelName(), exact=TRUE]]
  if (is.null(flag)) flag <- FALSE
  if (flag) {
    ret <- runCorr(mymod, readData, cohort=cohortLabel)
  } else {
    ret <- runModel(mymod, readData, cohortLabel=cohortLabel, op=NULL,
                    writeTofile=FALSE)
  }
  ret

} # END: run1Model

writeObjectToFile <- function(modelResults, cohortLabel, model, op, dir=NULL) {

  out.type <- op[[getOutTypeOpName(), exact=TRUE]] 
  rdaFlag  <- out.type == getOutTypeOpRda()
  fname    <- getOutFileName(cohortLabel, model, out.type)
  if (!is.null(dir)) fname <- paste0(dir, fname)
  if (rdaFlag) {
    save(modelResults, file=fname)
  } else {
    OutputListToExcel(fname, modelResults)
  }
  NULL

} # END:writeObjectToFile  

getModelFuncFromData <- function(readData, model.index) {

  mods <- readData$mods[model.index, , drop=FALSE]
  ret  <- try(getAllOptionsForModel(mods, readData, only.modelFunction=1), silent=FALSE)
  if ("try-error" %in% class(ret)) ret <- "_UNKNOWN_ERROR_"

  ret 

} # END: getModelFuncFromData

# Function to determine if return object is valid
isValidReturnObj <- function(obj) {

  ret <- FALSE
  cls <- class(obj)
  tmp <- cls %in% c(class_runModel(), class_runCorr())
  if (any(tmp)) ret <- TRUE
  ret

} # END: isValidReturnObj

# Function to create a runModel return list from an error
getResListFromError <- function(obj, model) {

  cls <- class(obj)
  
  if ("try-error" %in% class(obj)) {
    msg <- getErrorMsgFromTryError(obj, addToEnd=NULL)
  } else if (isString(obj)) {
    msg <- obj
  } else {
    msg <- "Unknown error"
  }
  msg <- removeEOL(msg)
  c1  <- runModel.getWarningCol()
  c2  <- runModel.getObjectCol()
  c3  <- runModel.getMessageCol()
  tmp <- list()
  tmp[[c1]]  <- "ERROR"
  tmp[[c2]]  <- model
  tmp[[c3]]  <- msg
  err        <- runmodel.addWarning(NULL, tmp)
  err        <- as.data.frame(err, stringsAsFactors=FALSE)
  ret        <- list()
  nm         <- runModel.getWarningsListName()
  ret[[nm]]  <- err
  class(ret) <- class_runModel()
  ret

} # END: getResListFromError 

# Function to get output file extension
getOutExtension <- function(out.type) {

  if (out.type == getOutTypeOpRda()) {
    ret <- ".rda"
  } else {
    ret <- ".xlsx"
  }
  ret

} # END: getOutExtension

# Function to create output file name
getOutFileName <- function(cohortLabel, model, out.type) {

  if (!length(cohortLabel)) cohortLabel <- "NA"
  cohortLabel <- trimws(cohortLabel)
  if (!nchar(cohortLabel)) cohortLabel <- "NA"
  if (!length(model)) model <- "NA"
  model <- trimws(model)
  if (!nchar(model)) model <- "NA"

  # Have file names in the form model__cohort__date, so it can easily be parsed
  sep <- getOutfileCohortSep()

  # Normalize strings
  str.model <- normOutFileStr(model)
  str.label <- normOutFileStr(cohortLabel)
  fname     <- paste0(str.model, sep, str.label, sep, Sys.Date(), getOutExtension(out.type))
  fname

} # END: getOutFileName

normOutFileStr <- function(str) {

  # Replace non-alphanumeric chars with a blank space
  searchStr <- "[^[:alnum:]._-]"
  ret       <- gsub(searchStr, " ", str)

  # Compress multiple blanks
  ret <- gsub("\\s+", " ", trimws(ret)) 

  # Replace blank with other char
  ret <- gsub(" ", getOutfileSpCharSep(), ret) 

  ret
}

combine2Lists <- function(l1, l2, op) {

  ret             <- list()
  common          <- op[[getOutCommonColsOpName(), exact=TRUE]]
  doNotRemoveCols <- op$doNotRemoveCols
  for (nm in getAllRetSheetNames()) {
    df1 <- l1[[nm,exact=TRUE]]
    df2 <- l2[[nm,exact=TRUE]]
    if (common) {
      tmp  <- df.rbind.common(df1, df2, doNotRemoveCols=doNotRemoveCols) 
    } else {
      tmp  <- df.rbind.all(df1, df2) 
    }
    ret[[nm]] <- tmp
  }
  ret

} # END: combine2Lists

combineListAndDF <- function(lst, df, op) {

  ret             <- lst
  common          <- op[[getOutCommonColsOpName(), exact=TRUE]]
  doNotRemoveCols <- op$doNotRemoveCols
  ef.nm           <- getEffectsName()
  x               <- lst[[ef.nm,exact=TRUE]]

  if (common) {
    tmp  <- df.rbind.common(x, df, doNotRemoveCols=doNotRemoveCols) 
  } else {
    tmp  <- df.rbind.all(x, df) 
  }
  ret[[ef.nm]] <- tmp
  
  ret

} # END: combineListAndDF

combine2DF <- function(df1, df2, op) {

  ret             <- NULL
  common          <- op[[getOutCommonColsOpName(), exact=TRUE]]
  doNotRemoveCols <- op$doNotRemoveCols
  if (common) {
    ret  <- df.rbind.common(df1, df2, doNotRemoveCols=doNotRemoveCols) 
  } else {
    ret  <- df.rbind.all(df1, df2) 
  }
  
  ret

} # END: combine2DF

combine2ModelObj <- function(obj1, obj2, op) {

  DEBUG  <- op[["DEBUG", exact=TRUE]]
  if (is.null(DEBUG)) DEBUG <- 0
  common <- op[[getOutCommonColsOpName(), exact=TRUE]]
  if (is.null(common)) stop("INTERNAL CODING ERROR with op$common")
  doNotRemoveCols <- op[["doNotRemoveCols", exact=TRUE]]
  if (is.null(doNotRemoveCols)) stop("INTERNAL CODING ERROR with op$doNotRemoveCols")

  ret          <- NULL
  n1           <- length(obj1)
  n2           <- length(obj2)
  cls1         <- class(obj1)
  cls2         <- class(obj2)
  cls.runCorr  <- class_runCorr()
  cls.runModel <- class_runModel()

  if ((cls.runModel %in% cls1) && (cls.runModel %in% cls2)) {
    if (DEBUG) print("Combine 2 lists")
    ret        <- combine2Lists(obj1, obj2, op)
    class(ret) <- cls1
  } else if (cls.runModel %in% cls1) {
    if (DEBUG) print("Combine list and df")
    ret        <- combineListAndDF(obj1, obj2, op)
    class(ret) <- cls1
  } else if (cls.runModel %in% cls2) {
    if (DEBUG) print("Combine df and list")
    ret        <- combineListAndDF(obj2, obj1, op)
    class(ret) <- cls2
  } else {
    if (DEBUG) print("Combine 2 df")
    ret        <- combine2DF(obj1, obj2, op)
    class(ret) <- cls1
  }
  ret 

} # END: combine2ModelObj

getModelFunctionName <- function(runModelObj) {

  # runModelObj could be a list or just a data frame (runCorr), or a try-error
  ret <- NULL
  n   <- length(runModelObj)
  if (!n) return(ret)
  if (nonEmptyDf(runModelObj)) return(getOldCorrModelName())
  if (is.list(runModelObj)) {
    nm  <- getInfoTableDfName()
    x   <- runModelObj[[nm, exact=TRUE]]
    ret <- getInfoTableValue(x, getOpModelName(), ifNotFound=NULL)
  }

  ret

} # END: getModelFunctionName

getAllModelFunctions <- function(reslist) {

  n <- length(reslist)
  if (!n) return(NULL)
  if (!is.list(reslist)) return(NULL)
  ret <- NULL
  for (i in 1:n) {
    tmp <- getModelFunctionName(reslist[[i]]) 
    if (length(tmp)) ret <- unique(c(ret, tmp))
  }
  ret
  
} # END: getAllModelFunctions

updateModelDF <- function(df, model, modelNumber) {

  if (!is.data.frame(df)) stop("INTERNAL CODING ERROR 1 in updateModelDF")

  # Add columns for model and model number
  mv  <- getModelSummaryModelCol()
  mnv <- getModelSummaryModelNumCol()
  cx  <- colnames(df)
  if (nrow(df)) {
    if (!(mv %in% cx)) df[, mv]   <- model
    if (!(mnv %in% cx)) df[, mnv] <- modelNumber
  } 
  df

} # END: updateModelDF

updateModelObj <- function(obj, model, modelNumber) {

  ret <- obj

  # Add columns for model and model number
  cls <- class(obj)
  if (any(cls %in% class_runCorr())) {
    ret <- updateModelDF(ret, model, modelNumber)
  } else if (any(cls %in% class_runModel())) {
    nms <- getAllRetSheetNames() 
    for (nm in nms) {
      tmp <- ret[[nm, exact=TRUE]]
      if (!is.null(tmp)) ret[[nm]] <- updateModelDF(tmp, model, modelNumber)
    }
  } else {
    stop("INTERNAL CODING ERROR 1 in updateModelObj")
  }

  ret

} # END: updateModelObj

mergeByModelFunction <- function(reslist, modelStrings, modelFunctions, func, op) {

  if (!isString(func)) stop("INTERNAL CODING ERROR 1 in mergeByModelFunction")
  n <- length(reslist)
  if (!n) return(NULL)
  if (n != length(modelStrings)) stop("INTERNAL CODING ERROR 2 in mergeByModelFunction")
  if (n != length(modelFunctions)) stop("INTERNAL CODING ERROR 3 in mergeByModelFunction")
  if (!is.list(reslist)) return(NULL)
  tmp  <- modelFunctions %in% func
  ids  <- (1:n)[tmp]
  nids <- length(ids)
  if (!nids) return(NULL)
  id  <- ids[1]
  ret <- reslist[[id]]
  ret <- updateModelObj(ret, modelStrings[id], id)
  if (nids < 2) return(ret)

  for (i in 2:nids) {
    id  <- ids[i]
    tmp <- updateModelObj(reslist[[id]], modelStrings[id], id)
    ret <- combine2ModelObj(ret, tmp, op)
  }
  ret

} # END: mergeByModelFunction 

mergeAllModels <- function(reslist, modelStrings, op) {

  n <- length(reslist)
  if (!n) return(NULL)
  if (n != length(modelStrings)) stop("INTERNAL CODING ERROR 1 in mergeByModelFunction")
  if (!is.list(reslist)) return(NULL)
  ids <- 1:n
  id  <- ids[1]
  ret <- reslist[[id]]
  ret <- updateModelObj(ret, modelStrings[id], id)
  if (n < 2) return(ret)

  for (i in 2:n) {
    id  <- ids[i]
    tmp <- updateModelObj(reslist[[id]], modelStrings[id], id)
    ret <- combine2ModelObj(ret, tmp, op)
  }
  ret

} # END: mergeAllModels

mergeModelsAndOutput <- function(reslist, modelStrings, mrgStrs, cohortLabel, op) {

  if (!runAllModels.getMergeFlag(op)) return(NULL)
  mrgStrs <- tolower(trimws(mrgStrs))
  merge   <- op[[getOutMergeOpName(), exact=TRUE]]

  if (merge == getOutMergeOpAll()) {
    # Merge all results together
    tmp <- mergeAllModels(reslist, modelStrings, op) 
    tmp <- runAllModels.orderObjVars(tmp)
    writeObjectToFile(tmp, cohortLabel, getOutMergeAllStr(), op)
  } else {
    # Merge models by (function or modelspec) and output 
    for (str in unique(mrgStrs)) {
      tmp <- mergeByModelFunction(reslist, modelStrings, mrgStrs, str, op)
      tmp <- runAllModels.orderObjVars(tmp)
      writeObjectToFile(tmp, cohortLabel, str, op)
    }
  }
 
  NULL

} # END: mergeModelsAndOutput

runAllModels.getOptions <- function(readData) {

  op      <- list()
  valid   <- c(getOutTypeOpName(), getOutCommonColsOpName(), getOutMergeOpName()) 
  default <- list(getOutTypeOpDefault(), getOutCommonColsOpDefault(), getOutMergeOpNone())
  keep    <- c(getEffectsEstSeName(), runModel.getStrataColName(), runModel.getStrataNumColName())
  ret     <- list(doNotRemoveCols=keep)
  opTable <- readData[[getMetabDataOpsName(), exact=TRUE]]
  op      <- list()
  if (length(opTable)) op <- getGlobalOptionsFromSheet(opTable)
  if ("try-error" %in% class(op)) stop(op)
  if (!length(op)) op <- list()
  op      <- default.list(op, valid, default)
  for (nm in valid) ret[[nm]] <- op[[nm]]
  
  # Turn off output.merge option
  ret[[getOutMergeOpName()]] <- getOutMergeOpNone()

  ret

} # END: runAllModels.getOptions

runAllModels.getMergeFlag <- function(op) {

  ret <- op[[getOutMergeOpName()]] != getOutMergeOpNone()
  if (!length(ret)) stop("INTERNAL CODING ERROR in runAllModels.getMergeFlag")
  ret

} # END: runAllModels.getMergeFlag

runAllModels.getMergeStr <- function(readData, mymod, modelIndex, op) {

  ret <- ""
  val <- op[[getOutMergeOpName(), exact=TRUE]]
  if (!length(val)) stop("INTERNAL CODING ERROR in runAllModels.getMergeStr")

  if (val == getOutMergeOpByModelFunc()) {
    if (!("try-error" %in% class(mymod))) ret <- mymod$options$model
    #if (ret == "") ret <- getModelFuncFromData(readData, modelIndex)
  } else {
    ret <- readData$mods[modelIndex, getModelOptionsIdCol(), drop=TRUE]
  }
  ret

} # END: runAllModels.getMergeStr

runAllModels.orderDfVars <- function(x) {

  if (!length(x)) return(x)
  cx <- try(colnames(x), silent=TRUE)
  if ("try-error" %in% class(cx)) return(x)
  if (!length(cx)) return(x)
  m1v <- getModelSummaryModelCol()
  m2v <- getModelSummaryModelNumCol()
  s1v <- runModel.getStrataColName()
  s2v <- runModel.getStrataNumColName()
  
  # Let the final columns be s1v, s2v, m2v, m1v
  vv  <- c(s1v, s2v, m2v, m1v)
  tmp <- vv %in% cx
  vv  <- vv[tmp]
  if (!length(vv)) return(x)
  tmp <- !(cx %in% vv)
  cx2 <- c(cx[tmp], vv)
  if ((length(cx) == length(cx2)) && !all(cx == cx2)) {
    x <- x[, cx2, drop=FALSE]
  }

  x

} # END: runAllModels.orderDfVars

runAllModels.orderObjVars <- function(obj) {

  cls <- class(obj)
  if (class_runModel() %in% cls) {
    tsheet <- getTable1DfName()
    sheets <- getAllRetSheetNames()
    for (sheet in sheets) {
      x <- obj[[sheet, exact=TRUE]]
      x <- runAllModels.orderDfVars(x) 
      if ((sheet == tsheet) && nonEmptyDf(x)) {
        cols <- getTable1ColNames()$cols
        x    <- orderVars(x, cols)
      }
      obj[[sheet]] <- x
    }
  } else if (class_runCorr() %in% cls) {
    obj <- runAllModels.orderDfVars(obj) 
  }
  obj

} # END: runAllModels.orderObjVars