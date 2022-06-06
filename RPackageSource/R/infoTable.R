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

updateInfoTableVecs <- function(nms, op, namevec, valuevec, namestr) {

  if (length(nms)) {
    for (nm in nms) {
      tmp <- op[[nm, exact=TRUE]]
      len <- length(tmp)
      if (!len) tmp <- "NULL"
      len <- length(tmp)
      if ((len == 1) && (is.character(tmp) || is.numeric(tmp))) {
        namevec  <- c(namevec, paste0(namestr, nm))
        valuevec <- c(valuevec, as.character(tmp))
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

  global    <- getValidGlobalOps()$valid
  exc       <- c("DEBUG", "DONOTRUN", getModelOpsName(), getOutCommonColsOpName())
  tmp       <- !(global %in% exc)
  global    <- global[tmp]
  tmp       <- updateInfoTableVecs(global, op, namevec, valuevec, "op$")
  namevec   <- tmp$namevec
  valuevec  <- tmp$valuevec

  mop      <- op[[getModelOpsName(), exact=TRUE]]
  inc      <- c("method", "family", "link", "weights", "offset", 
                "ties", "robust", "Surv.type")
  nms       <- names(mop)
  tmp       <- nms %in% inc
  nms       <- nms[tmp]
  tmp       <- updateInfoTableVecs(nms, mop, namevec, valuevec, "model.options$")
  namevec   <- tmp$namevec
  valuevec  <- tmp$valuevec

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

  ret <- data.frame(name=namevec, value=valuevec, stringsAsFactors=FALSE)
  ret
}
