getInfoTableValue <- function(infoTable, name, ifNotFound=NULL, check.len=1) {

  ret <- ifNotFound
  if (nonEmptyDf(infoTable)) {
    cx   <- colnames(infoTable)
    nmv  <- getInfoTableNameCol()
    valv <- getInfoTableValueCol()
    if (!all(c(nmv, valv) %in% cx)) stop("INTERNAL CODING ERROR 1")

    tmp  <- infoTable[, nmv, drop=TRUE] %in% name 
    n    <- sum(tmp)
    if (!n) return(ifNotFound)
    ret <- unique(infoTable[tmp, valv, drop=TRUE])  
    if (check.len && (check.len != length(ret))) stop("INTERNAL CODING ERROR 2")
  }
  ret

}

setInfoTableValue <- function(infoTable, name, value) {

  ret <- infoTable
  if (nonEmptyDf(infoTable)) {
    cx   <- colnames(infoTable)
    nmv  <- getInfoTableNameCol()
    valv <- getInfoTableValueCol()
    if (!all(c(nmv, valv) %in% cx)) stop("INTERNAL CODING ERROR 1")

    tmp  <- infoTable[, nmv, drop=TRUE] %in% name 
    n    <- sum(tmp)
    if (!n) return(ret)
    ret[tmp, valv] <- value  
  }
  ret

}


infoTable_normNames <- function(infoTable) {

  if (nonEmptyDf(infoTable)) {
    cx   <- colnames(infoTable)
    nmv  <- getInfoTableNameCol()
    valv <- getInfoTableValueCol()
    if (!all(c(nmv, valv) %in% cx)) stop("INTERNAL CODING ERROR 1")

    tmp  <- infoTable[, nmv, drop=TRUE] %in% getInfoTableModelNmName()
    if (any(tmp)) infoTable[tmp, valv] <- meta_normModelStr(infoTable[tmp, valv, drop=TRUE])
    tmp  <- infoTable[, nmv, drop=TRUE] %in% getInfoTableCohortName()
    if (any(tmp)) infoTable[tmp, valv] <- meta_normCohortStr(infoTable[tmp, valv, drop=TRUE])
  }
  infoTable

}

updateInfoTableVecs <- function(nms, op, namevec, valuevec, namestr) {

  if (length(nms)) {
    excvec <- c(metaOp_cohorts.merge(), metaOp_cohorts.indep(), metaOp_addCohortCols(),
                metaOp_cohorts.include(), metaOp_cohorts.exclude())
    for (nm in nms) {
      tmp <- op[[nm, exact=TRUE]]
      len <- length(tmp)
      if (!len) tmp <- "NULL"
      len <- length(tmp)
      if ((len == 1) && (is.character(tmp) || is.numeric(tmp) || is.logical(tmp))) {
        namevec  <- c(namevec, paste0(namestr, nm))
        valuevec <- c(valuevec, as.character(tmp))
      } else if (len && (nm %in% excvec)){
        namevec  <- c(namevec, paste0(namestr, nm))
        valuevec <- c(valuevec, paste(as.character(tmp), collapse=", "))
      }
    }
  }

  list(namevec=namevec, valuevec=valuevec)
}

getInfoTableDF <- function(modeldata, metabdata, op) {

  si        <- sessionInfo()
  mydate    <- as.character(Sys.time())
  rversion  <- si$R.version$version.string
  if (!length(rversion)) rversion <- ""
  os        <- si$running
  if (!length(os)) os <- ""
  cversion  <- si$otherPkgs$RcometsAnalytics$Version
  if (!length(cversion)) cversion <- getVersionNumber()
  inputf    <- metabdata[[getInputFileOpsName(), exact=TRUE]]
  if (!length(inputf)) inputf <- ""
  modelName <- modeldata$modlabel
  if (!length(modelName)) modelName <- ""
  modelspec <- modeldata$modelspec
  if (!length(modelspec)) modelspec <- ""
  cohort    <- op$cohort
  if (!length(cohort)) cohort <- ""

  namevec   <- c("date", "cohort", 
                 "RcometsAnalytics version", "R version", "operating system",
                 "input file", "model name", "run type")
  valuevec  <- c(mydate, cohort, 
                 cversion, rversion, os,
                 inputf, modelName, modelspec) 
  if (length(namevec) != length(valuevec)) stop("INTERNAL CODING ERROR 1")

  global    <- getValidGlobalOps()$valid
  exc       <- c("DEBUG", "DONOTRUN", getModelOpsName(), getOutCommonColsOpName())
  tmp       <- !(global %in% exc)
  global    <- global[tmp]
  tmp       <- updateInfoTableVecs(global, op, namevec, valuevec, "op$")
  namevec   <- tmp$namevec
  valuevec  <- tmp$valuevec
  if (length(namevec) != length(valuevec)) stop("INTERNAL CODING ERROR 2")

  mop      <- op[[getModelOpsName(), exact=TRUE]]
  inc      <- c("method", "family", "link", "weights", "offset", 
                "ties", "robust", "Surv.type")
  nms       <- names(mop)
  tmp       <- nms %in% inc
  nms       <- nms[tmp]
  tmp       <- updateInfoTableVecs(nms, mop, namevec, valuevec, "model.options$")
  namevec   <- tmp$namevec
  valuevec  <- tmp$valuevec
  if (length(namevec) != length(valuevec)) stop("INTERNAL CODING ERROR 3")

  # Add info for meta-analyses
  yv        <- modeldata[["rcovs", exact=TRUE]]
  ev        <- modeldata[["ccovs", exact=TRUE]]
  sv        <- modeldata[["scovs", exact=TRUE]]
  if (length(yv) != 1) yv <- getInfoTable2plusVars()
  if (length(ev) != 1) ev <- getInfoTable2plusVars()
  if (length(sv) > 1) sv <- paste0(sv, collapse=runModel.getVarSep())
  if (!length(sv)) sv <- ""
  nms       <- c(getInfoTableOutcomeName(), getInfoTableExposureName(), getInfoTableStrataName())
  vals      <- c(yv, ev, sv)
  namevec   <- c(namevec, nms)
  valuevec  <- c(valuevec, vals)
  if (length(namevec) != length(valuevec)) stop("INTERNAL CODING ERROR 4")

  # Add exposure reference level
  exprefs <- modeldata[["exposurerefs", exact=TRUE]]
  n       <- length(exprefs)
  if (!n) {
    exprefs <- ""
  } else if (n > 1) {
    exprefs <- getInfoTable2plusVars()
  }
  namevec  <- c(namevec, getInfoTableExpRefName())
  valuevec <- c(valuevec, exprefs)
  if (length(namevec) != length(valuevec)) stop("INTERNAL CODING ERROR 5")

  ret <- data.frame(name=namevec, value=valuevec, stringsAsFactors=FALSE)
  ret
}

getInfoTableDF.meta <- function(op) {

  si        <- sessionInfo()
  mydate    <- as.character(Sys.time())
  rversion  <- si$R.version$version.string
  if (!length(rversion)) rversion <- ""
  os        <- si$running
  if (!length(os)) os <- ""
  cversion  <- si$otherPkgs$RcometsAnalytics$Version
  if (!length(cversion)) cversion <- getVersionNumber()
  cohorts  <- op$cohorts
  ncohorts <- length(cohorts)
  if (!ncohorts) cohorts <- ""
  cohorts <- paste0(cohorts, collapse=", ")
  modelName <- op[["MODEL", exact=TRUE]]
  if (length(modelName) > 1) modelName <- modelName[1]
  if (!length(modelName)) modelName <- ""

  namevec   <- c("date", 
                 "RcometsAnalytics version", "R version", "operating system",
                 "model name", "cohorts")
  valuevec  <- c(mydate, 
                 cversion, rversion, os,
                 modelName, cohorts) 

  # Add file names
  files.orig <- op[["files.orig", exact=TRUE]]
  nfiles     <- length(files.orig)
  if (nfiles) {
    namevec  <- c(namevec,  paste0("file", 1:nfiles))
    valuevec <- c(valuevec, files.orig) 
  }

  # Get meta options
  tmp       <- meta_check_op(NULL)
  global    <- names(tmp)
  exc       <- c("DEBUG", "MODEL", metaOp_dups.method(), metaOp_oneModelCheck())
  tmp       <- !(global %in% exc)
  global    <- global[tmp]
  tmp       <- updateInfoTableVecs(global, op, namevec, valuevec, "op$")
  namevec   <- tmp$namevec
  valuevec  <- tmp$valuevec

  ret <- data.frame(name=namevec, value=valuevec, stringsAsFactors=FALSE)

  ret
}
