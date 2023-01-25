#' This function allows users to run a single meta-analysis 
#' @param filevec Character vector of files that contain
#'        the output files from \code{\link{runModel}} or \code{\link{runAllModels}}. 
#'        Valid file extensions are ".xlsx", and ".rda". 
#' @param op A list containing the options to use.
#'           See \code{\link{meta_options}}.
#'           The default is NULL.
#' @return List of data frames containing the results and information
#'
#' @export
runMeta <- function(filevec, op=NULL) {

  filevec     <- checkMetaFiles(filevec, valid.ext=c(getOutTypeOpRda(), getOutTypeOpExcel()))
  op          <- meta_check_op(op) 
  op$temp.dir <- meta_createTempDir(getwd(), nrand=6)
  ret         <- try(meta_main(filevec, NULL, op), silent=TRUE)
  meta_removeTempDir(op$temp.dir)
  ret
}

meta_main <- function(filevec, modelName, op) {

  DEBUG                <- op$DEBUG
  if (DEBUG) cat("Begin: meta_main\n")
  if (length(filevec) < op[[metaOp_minNcohortName()]]) {
    msg <- paste0("ERROR: number of input files less than ", metaOp_minNcohortName())
    stop(msg)
  }
  op$modelName         <- modelName
  op$modelNumber       <- meta_getModelNumber(modelName)
  tmp                  <- meta_initFileCheck(filevec, op)
  op                   <- tmp$op
  file.info.list       <- tmp$file.info.list

  tmp                  <- meta_loadFilesAndSetupData(file.info.list, op)
  data.list            <- tmp$data
  file.info.list       <- tmp$file.info.list
  op                   <- tmp$op
  ids                  <- tmp$ids
  nsubs                <- tmp$nsub
  op$cohorts           <- names(data.list)
  rm(tmp)
  gc()

  # Check for model consistency across the cohorts
  meta_consistency(file.info.list, op)

  # Determine if this is a correlation model
  modfunc <- getInfoTableValue(file.info.list[[1]]$info, getInfoTableModelFuncName(), ifNotFound=NULL, check.len=1) 
  if (is.null(modfunc)) stop("INTERNAL CODING ERROR: model function")
  op$corr.model <- (modfunc == getCorrModelName()) 

  # Determine if this is a stratified model
  ret <- getInfoTableValue(file.info.list[[1]]$info, runModel.getStrataNumColName(), ifNotFound=NULL, check.len=1) 
  if (is.null(ret)) stop("INTERNAL CODING ERROR: strata")
  if (is.na(ret)) ret <- ""
  op$strat.model <- nchar(ret) > 0

  ret <- meta_getFinalDataAndRun(ids, nsubs, data.list, op) 

  ret <- meta_setReturnObj(ret, op)
  if (DEBUG) cat("End: meta_main\n")  

  ret     
}

meta_setReturnObj <- function(df, op) {

  ret  <- list()
  wnm  <- runModel.getWarningsListName()
  wobj <- op[[wnm, exact=TRUE]]
  if (is.null(wobj)) wobj <- runModel.getEmptyErrorWarn() 
  if (op$strat.model) df <- meta_strat_het_test(df, op)
  if (op$corr.model) df <- meta_FisherZtoCorr(df, alpha=0.95)

  # Check min nbumber of cohorts and sample size
  df <- meta_setRetDf(df, op) 

  ord <- c(
           runModel.getStrataColName(), runModel.getStrataNumColName(),
           getModelSummaryOutUidCol(), getModelSummaryExpUidCol(),
           getEffectsTermName(), 
           getMetaNcohortCol(), getMetaNsubCol(),
           getMetaFixedPvalueCol(), getMetaFixedBetaCol(), getMetaFixedBetaSeCol(),
           getMetaFixedBetaLCol(), getMetaFixedBetaUCol(),
           getMetaRandomPvalueCol(), getMetaRandomBetaCol(), getMetaRandomBetaSeCol(),
           getMetaRandomBetaLCol(), getMetaRandomBetaUCol(),
           getMetaHetPvalueCol(), getMetaDirectionCol(), 
           paste0(getEffectsPvalueName(), ".", op$cohorts),
           getMetaMessageCol())
 
  ret[[metaRetListResultsTable()]] <- orderVars(df, ord)
  ret[[wnm]]                       <- wobj

  # Info table
  nm        <- getInfoTableDfName()
  ret[[nm]] <- getInfoTableDF.meta(op)
  
  ret

}

meta_setRetDf <- function(x, op) {

  nr    <- nrow(x)
  if (!nr) return(x)
  N     <- op[[metaOp_minNcohortName()]]
  nsub  <- op[[metaOp_totalMinSubs()]]
  Nv    <- getMetaNcohortCol()
  nsubv <- getMetaNsubCol()
  cx    <- colnames(x)
  keep  <- rep(TRUE, nr)
  if ((N > 0) && (Nv %in% cx)) {
    tmp <- x[, Nv, drop=TRUE] >= N
    tmp[is.na(tmp)] <- TRUE
    keep <- keep & tmp
  }
  if ((nsub > 0) && (nsubv %in% cx)) {
    tmp <- x[, nsubv, drop=TRUE] >= nsub
    tmp[is.na(tmp)] <- TRUE
    keep <- keep & tmp
  }
  if (!all(keep)) x <- x[keep, , drop=FALSE]
  x

}

meta_consistency <- function(file.info.list, op) {

  # Check model, outcomes, exposures
  meta_model_check(file.info.list, op)
  meta_outExp_check(file.info.list, op) 

  NULL
}

meta_model_check <- function(file.info.list, op) {

  n          <- length(file.info.list)
  modnm      <- getInfoTableModelFuncName()
  cohortnm   <- getInfoTableCohortName()
  modelFuncs <- rep("", n)
  miss       <- c("", NA, "NA")
  glmnm      <- getGlmModelName()
  famnm      <- getInfoTableFamilyName() 
  lmnm       <- getLmModelName()

  for (i in 1:n) {
    flist  <- file.info.list[[i]]
    info   <- flist$info
    cohort <- getInfoTableValue(info, cohortnm, ifNotFound="", check.len=1) 
    model  <- getInfoTableValue(info, modnm, ifNotFound="", check.len=1) 
    if (model %in% miss) {
      msg <- paste0("ERROR: missing model function for cohort ", cohort)
      stop(msg)
    }
    if (model == glmnm) {
      fam <- getInfoTableValue(info, famnm, ifNotFound="", check.len=1)
      if (fam == "gaussian") model <- lmnm  # Same as linear model
    }
    modelFuncs[i] <- model
  }
  umods <- unique(modelFuncs)
  if (length(umods) != 1) stop("ERROR: different model functions were run among the cohorts")
  NULL

}

meta_outExp_check <- function(file.info.list, op) {

  DEBUG      <- op$DEBUG
  if (DEBUG) cat("Begin: meta_outExp_check\n")

  n          <- length(file.info.list)
  cohortnm   <- getInfoTableCohortName()
  miss       <- c("", NA, "NA")
  ovec       <- rep("", n)
  evec       <- ovec
  ovecn      <- rep(0, n)
  evecn      <- ovecn
  outnm      <- getInfoTableOutcomeName()
  expnm      <- getInfoTableExposureName() 
  sep        <- runModel.getVarSep()
  symbol     <- getInfoTable2plusVars()
  for (i in 1:n) {
    flist    <- file.info.list[[i]]
    if (DEBUG) cat(paste0("  checking file ", flist$file, "\n"))
    info     <- flist$info
    cohort   <- getInfoTableValue(info, cohortnm, ifNotFound="", check.len=1) 
    outcome  <- getInfoTableValue(info, outnm,    ifNotFound="", check.len=1) 
    exposure <- getInfoTableValue(info, expnm,    ifNotFound="", check.len=1) 
    olen     <- length(unlist(strsplit(outcome, sep)))
    elen     <- length(unlist(strsplit(exposure, sep)))
    # Check for multiple outcomes and multiple exposures
    if ((outcome == symbol) && (exposure == symbol)) {
      stop(paste0("ERROR: cohort ", cohort, " contains multiple outcomes and multiple exposures"))
    } 
    ovec[i] <- outcome
    evec[i] <- exposure
  }
  # Check for different exposure variables
  vec <- unique(evec)
  if (length(vec) > 1) stop("ERROR: all cohorts must have the same exposure variable") 

  # Check for different outcome variables
  vec <- unique(ovec)
  if (length(vec) > 1) stop("ERROR: all cohorts must have the same outcome variable") 

  if (DEBUG) cat("End: meta_outExp_check\n")

  NULL

}

meta_getDirection <- function(data, beta.vars) {

  nr  <- nrow(data)
  ret <- NULL
  for (i in 1:length(beta.vars)) {
    vec <- rep("?", nr)
    x   <- as.numeric(unlist(data[, beta.vars[i], drop=TRUE]))
    tmp <- x >= 0
    tmp[is.na(tmp)] <- FALSE
    if (any(tmp)) vec[tmp] <- "+"
    tmp <- x < 0
    tmp[is.na(tmp)] <- FALSE
    if (any(tmp)) vec[tmp] <- "-"
    if (i == 1) {
      ret <- vec
    } else {
      ret <- paste0(ret, vec)
    }
  }
  ret
}

meta_subsetDfByModelNumber <- function(df, modelNumber) {

  if (nonEmptyDf(df) && !is.null(modelNumber)) {
    mcol <- getModelSummaryModelCol()
    if (mcol %in% colnames(df)) {
      modnums <- meta_getModelNumber(df[, mcol, drop=TRUE])
      tmp     <- modnums %in% modelNumber
      df      <- df[tmp, , drop=FALSE] 
    }
  }
  df
}

FisherZtoCorr <- function(z) {

  val <- exp(2*z)

  # Check for (positive) infinity
  tmp <- is.infinite(val) & (val > 0)
  tmp[is.na(tmp)] <- FALSE

  ret <- (val - 1)/(val + 1)
  if (any(tmp)) ret[tmp] <- 1

  ret
}

meta_FisherZtoCorr <- function(df, alpha=0.95) {

  z      <- qnorm((1-alpha)/2, lower.tail=FALSE)

  # Fixed effects
  betav        <- getMetaFixedBetaCol()
  sev          <- getMetaFixedBetaSeCol()
  betaLv       <- getMetaFixedBetaLCol()
  betaUv       <- getMetaFixedBetaUCol()
  beta         <- df[, betav, drop=TRUE]
  se           <- df[, sev, drop=TRUE]
  df[, betav]  <- FisherZtoCorr(beta)
  df[[sev]]    <- NULL
  df[, betaLv] <- FisherZtoCorr(beta - z*se)
  df[, betaUv] <- FisherZtoCorr(beta + z*se)

  # Random effects
  betav        <- getMetaRandomBetaCol()
  sev          <- getMetaRandomBetaSeCol()
  betaLv       <- getMetaRandomBetaLCol()
  betaUv       <- getMetaRandomBetaUCol()
  beta         <- df[, betav, drop=TRUE]
  se           <- df[, sev, drop=TRUE]
  df[, betav]  <- FisherZtoCorr(beta)
  df[[sev]]    <- NULL
  df[, betaLv] <- FisherZtoCorr(beta - z*se)
  df[, betaUv] <- FisherZtoCorr(beta + z*se)

  df
}

meta_corrToFisherZ <- function(x, f) {
 
  # x: effects data frame
  # f: results file name
  
  # Determine id a stratified run
  sv     <- runModel.getStrataNumColName()
  rv     <- getEffectsRunName()
  betav  <- getEffectsEstName()
  sev    <- getEffectsEstSeName()

  stratFlag <- sv %in% colnames(x)

  # Load model summary data frame
  y <- loadDataFrame(f, getModelSummaryName())
  
  # Match rows to get the number of observations for each 
  xids <- x[, rv, drop=TRUE]
  yids <- y[, rv, drop=TRUE]
  if (stratFlag) {
    xids <- paste0(xids, ":", x[, sv, drop=TRUE])
    yids <- paste0(yids, ":", y[, sv, drop=TRUE])
  }
  rows <- match(xids, yids)
  if (any(is.na(rows))) stop("ERROR with Effects/ModelSummary")
  nobs <- as.numeric(y[rows, getModelSummaryNobsName(), drop=TRUE])
  
  corr <- as.numeric(x[, betav, drop=TRUE])
 
  # Change if corr = -1, 1
  eps        <- getMetaEpsForCorr()
  tmp        <- corr > 1 - eps
  tmp[is.na(tmp)] <- FALSE
  if (any(tmp)) corr[tmp] <- 1 - eps
  tmp        <- corr < -1 + eps
  tmp[is.na(tmp)] <- FALSE
  if (any(tmp)) corr[tmp] <- -1 + eps

  # Fisher's Z
  x[, betav] <- 0.5*log((1 + corr)/(1 - corr))
  x[, sev]   <- sqrt(1/(nobs - 3)) 

  x
}

meta_loadEffects <- function(f, cols, numvars, modnum, infoTable) {

  sheet <- getEffectsName()

  # Load effects data frame
  x   <- loadDataFrame(f, sheet)

  # Get correct model if file contains more than one model
  x <- meta_subsetDfByModelNumber(x, modnum)
  if (!nonEmptyDf(x)) stop("INTERNAL CODING ERROR effects") 

  # For correlation models, transform and add standard error
  modfunc <- getInfoTableValue(infoTable, getInfoTableModelFuncName(), ifNotFound=NULL, check.len=1) 
  if (is.null(modfunc)) stop("INTERNAL CODING ERROR op$model")
  if (modfunc == getCorrModelName()) x <- meta_corrToFisherZ(x, f) 

  cx <- colnames(x)
  if (length(numvars)) {
    if (!all(numvars %in% cx)) stop("INTERNAL CODING ERROR numvars")
    for (v in numvars) x[, v] <- as.numeric(x[, v])
  }
  if (length(cols)) {
    tmp  <- cols %in% cx
    cols <- cols[tmp]
    if (!length(cols)) stop("INTERNAL CODING ERROR 1") 
    x  <- x[, cols, drop=FALSE]
  }

  x
}

meta_getIdNames <- function(x, strata.cols=TRUE) {

  sep <- getMetaIdNamesSep()
  cx  <- colnames(x)
  vv1 <- getMetaIdNamesUidCols()
  vv2 <- getMetaIdNamesStratCols()
  if (!all(vv1 %in% cx)) stop("INTERNAL CODING ERROR 1 in meta_getIdNames")
  ret <- paste(x[, vv1[1], drop=TRUE], x[, vv1[2], drop=TRUE], x[, vv1[3], drop=TRUE], sep=sep)
  if (strata.cols && all(vv2 %in% cx)) {
    ret <- paste(ret, x[, vv2[1], drop=TRUE], x[, vv2[2], drop=TRUE], sep=sep)
  }

  ret
}

meta_loadCohortData <- function(flist, op) {

  DEBUG <- op$DEBUG
  if (DEBUG) cat("Begin: meta_loadCohortData\n")
  idv      <- getEffectsRunName()
  cols     <- c(idv,
               runModel.getStrataColName(), runModel.getStrataNumColName(),
               getEffectsTermName(), 
               getEffectsEstName(), 
               getEffectsEstSeName(), 
               getEffectsPvalueName())
  numvars  <- c(getEffectsEstName(), getEffectsEstSeName(), getEffectsPvalueName())

  # Load Effects data frame
  x   <- meta_loadEffects(flist$file, cols, numvars, NULL, flist$info)

  # Add in sample size, uid columns from modelSummary data frame
  y   <- loadDataFrame(flist$file, getModelSummaryName())
  ssv <- getModelSummaryNobsName()
  yv  <- getModelSummaryOutUidCol()
  ev  <- getModelSummaryExpUidCol()
  tmp <- c(idv, ssv, yv, ev)
  if (!nonEmptyDfHasCols(y, tmp, allcols=1, ignoreCase=0)) {
    stop("ERROR with ModelSummary")
  }
  y    <- y[, tmp, drop=FALSE]
  rows <- match(x[, idv, drop=TRUE], y[, idv, drop=TRUE]) 
  if (any(is.na(rows))) stop("ERROR matching rows between Effects and ModelSummary data frames")
  x[, ssv] <- y[rows, ssv]
  x[, yv]  <- y[rows, yv]
  x[, ev]  <- y[rows, ev]

  rm(y, rows)
  gc()

  # Filter based on options
  x <- meta_filterCohortData(x, op)

  # Add id column
  x[, getMetaIdCol()] <- meta_getIdNames(x) 

  if (DEBUG) cat("End: meta_loadCohortData\n")

  x
}

meta_filterCohortData <- function(x, op) {

  DEBUG <- op$DEBUG
  if (DEBUG) cat("Begin: meta_filterCohortData\n")
  n    <- nrow(x)
  keep <- rep(TRUE, n)
  tmp  <- is.finite(x[, getEffectsEstName(), drop=TRUE]) &
          is.finite(x[, getEffectsEstSeName(), drop=TRUE]) &
          x[, getEffectsEstSeName(), drop=TRUE] > 0
  tmp[is.na(tmp)] <- FALSE
  keep <- keep & tmp

  minN  <- op[[metaOp_cohortMinSubs(), exact=TRUE]]
  if (minN) {
    tmp <- x[, getModelSummaryNobsName(), drop=TRUE] >= minN
    tmp[is.na(tmp)] <- FALSE
    keep <- keep & tmp 
  }
  if (!all(keep)) x <- x[keep, , drop=FALSE]
  if (DEBUG) cat(paste0(n-nrow(x), " rows removed\n"))

  if (DEBUG) cat("End: meta_filterCohortData\n")
  x
}

meta_updateIdCnts <- function(retlist, ids, nsub) {

  len <- length(ids)
  if (!len) return(retlist)
  tmp <- !is.finite(nsub)
  if (any(tmp)) nsub[tmp] <- 0
  if (!length(retlist)) {
    retlist <- list(ids=ids, ncohort=rep(1, len), nsub=nsub)
  } else {
    tmp <- ids %in% retlist$ids
    if (any(tmp)) {
      retlist$ncohort[tmp] <- retlist$ncohort[tmp] + 1
      retlist$nsub[tmp]    <- retlist$nsub[tmp] + nsub[tmp]
    }
    tmp <- !tmp
    m   <- sum(tmp)
    if (m) {
      retlist$ids     <- c(retlist$ids, ids[tmp])
      retlist$ncohort <- c(retlist$ncohort, rep(1, m))
      retlist$nsub    <- c(retlist$nsub, nsub[tmp])
    }
  }
  retlist
}

meta_getFinalSet <- function(idlist, op) {

  len <- length(idlist$ids)
  if (!len) stop("ERROR: all metabolites have been filtered out")
  ok    <- rep(TRUE, len)
  minN  <- op[[metaOp_totalMinSubs(), exact=TRUE]]
  if (minN) {
    ok <- ok & (idlist$nsub >= minN)
    ok[is.na(ok)] <- FALSE
  }
  minN  <- op[[metaOp_minNcohortName(), exact=TRUE]]
  if (minN) {
    ok <- ok & (idlist$ncohort >= minN)
    ok[is.na(ok)] <- FALSE
  }
  ret.ids  <- idlist$ids[ok]
  ret.nsub <- idlist$nsub[ok] 
  list(ids=ret.ids, nsub=ret.nsub)
}

meta_loadFilesAndSetupData <- function(file.info.list, op) {

  DEBUG    <- op$DEBUG
  if (DEBUG) cat("Begin: meta_loadFilesAndSetupData\n")
  wnm      <- runModel.getWarningsListName()
  wobj     <- op[[wnm, exact=TRUE]]
  save.mem <- op[[metaOp_save.mem(), exact=TRUE]]
  nfiles   <- length(file.info.list)
  all.ids  <- NULL
  idlist   <- NULL
  ret      <- list()
  ret.info <- list()
  msnv     <- getModelSummaryNobsName()
  temp.dir <- op$temp.dir
  cohorts  <- NULL

  for (i in 1:nfiles) {
    x      <- NULL
    flist  <- file.info.list[[i]]
    cohort <- flist$cohort
    info   <- flist$info
    if (DEBUG) cat(paste0("Processing cohort ", cohort, "\n"))
    x    <- meta_loadCohortData(flist, op)
    wobj <- runmodel.checkForError(x, warnStr="ERROR", objStr=flist$file, rem.obj=wobj)
    if ("try-error" %in% class(x)) {
      if (DEBUG) print(x)
      next
    }

    # Update counts
    idlist <- meta_updateIdCnts(idlist, meta_getIdNames(x), x[, msnv, drop=TRUE])

    temp.file <- paste0(temp.dir, "cohort_", i, ".rda")
    save(x, file=temp.file)
    x <- NULL
    gc()

    # File is OK
    ind             <- length(ret.info) + 1 
    flist$info      <- info
    ret.info[[ind]] <- flist 
    ret[[ind]]      <- temp.file
    cohorts         <- c(cohorts, cohort)  
  }
  if (!length(ret)) stop("ERROR loading all files")  

  # Get final set of metabs to analyze
  tmp <- meta_getFinalSet(idlist, op) 

  names(ret) <- cohorts
  op[[wnm]]  <- wobj

  if (DEBUG) cat("End: meta_loadFilesAndSetupData\n")
  list(data=ret, file.info.list=ret.info, op=op, ids=tmp$ids, nsub=tmp$nsub)
}

meta_unpasteIds <- function(ids) {

  sep <- getMetaIdNamesSep()
  vec <- unlist(strsplit(ids[1], sep))
  m   <- length(vec)
  ret <- parseDelimVec(ids, sep, ncol=m)
  vv1 <- getMetaIdNamesUidCols()
  vv2 <- getMetaIdNamesStratCols()
  cx  <- c(vv1, vv2)
  ret <- as.data.frame(ret, stringsASFactors=FALSE)
  colnames(ret) <- cx[1:ncol(ret)]
  ret
}

meta_getFinalDataAndRun <- function(ids, nsubs, data.list, op) {

  ndata   <- length(data.list)
  N       <- length(ids)
  beta    <- matrix(data=NA, nrow=N, ncol=ndata)
  se      <- beta
  p       <- beta
  betav   <- getEffectsEstName()
  sev     <- getEffectsEstSeName()
  pv      <- getEffectsPvalueName()
  idv     <- getMetaIdCol()
  ind     <- 0
  cohorts <- NULL
  nms     <- names(data.list)
  for (i in 1:ndata) {
    x     <- NULL
    gc()
    x     <- myread_rda(data.list[[i]])
    file.remove(data.list[[i]])
    rows  <- match(ids, x[, idv, drop=TRUE])
    tmp   <- !is.na(rows)
    rows  <- rows[tmp]
    if (!length(rows)) next
    ind            <- ind + 1
    beta[tmp, ind] <- x[rows, betav, drop=TRUE]
    se[tmp, ind]   <- x[rows, sev, drop=TRUE]
    p[tmp, ind]    <- x[rows, pv, drop=TRUE]
    cohorts        <- c(cohorts, nms[i])
  }
  rm(x, tmp, rows, nms)
  gc()
  if (ind < ndata) {
    beta <- beta[, 1:ind, drop=FALSE]
    se   <- se[, 1:ind, drop=FALSE]
    p    <- p[, 1:ind, drop=FALSE]
    gc()
  }

  # Call main function
  ret <- meta_core(beta, se)
  rm(beta, se)
  gc()

  # Set return data frame
  p             <- as.data.frame(p)
  colnames(p)   <- paste0(pv, ".", cohorts)
  ret           <- as.data.frame(ret, stringsAsFactors=FALSE)
  nsubv         <- getMetaNsubCol()
  ret[, nsubv]  <- nsubs
  ret           <- cbind(meta_unpasteIds(ids), ret, p)
  
  ret
}

meta_getModelNumber <- function(x, sep=NULL) {

  # For now, model number will be the model name. May change later.
  return(x)

  if (is.null(sep)) sep <- metaModelNameNumberSep()
  x    <- trimws(x)
  len  <- nchar(x)
  tmp  <- str_locate(x, sep)
  pos  <- tmp[, 1, drop=TRUE]
  miss <- is.na(pos)
  if (any(miss)) pos[miss] <- len[miss]
  pos <- pos - 1
  tmp <- pos < 0
  if (any(tmp)) pos[tmp] <- 1
  if (any(miss)) pos[miss] <- len[miss]
  ret <- substr(x, 1, pos) 
  ret
}

meta_getFileInfo <- function(f, normModelNames=1) {
 
  nm <- getEffectsName()
  x  <- loadDataFrame(f, nm)
  if (!nonEmptyDf(x)) stop(paste0("ERROR: ", nm, " table not found"))
  
  nm <- getInfoTableDfName()
  x  <- loadDataFrame(f, nm)
  if (!nonEmptyDf(x)) stop(paste0("ERROR: ", nm, " table not found"))

  if (normModelNames) x <- infoTable_normNames(x)

  cnm    <- getInfoTableCohortName()
  cohort <- getInfoTableValue(x, cnm, ifNotFound=NULL, check.len=1) 
  if (!length(cohort)) stop("ERROR: name of cohort not found")

  mnm <- getInfoTableModelNmName()
  modname <- getInfoTableValue(x, mnm, ifNotFound=NULL, check.len=0) 
  if (!length(modname)) stop("ERROR: name of model not found")
  modnumbers <- meta_getModelNumber(modname)
  nmodels    <- length(modnumbers)
  if (nmodels != length(unique(modnumbers))) stop("ERROR: model names are not unique")

  list(cohort=cohort, model.names=modname, model.numbers=modnumbers, info=x)

}

meta_initFileCheck <- function(filevec, op) {

  DEBUG    <- op$DEBUG
  if (DEBUG) cat("Begin: meta_initFileCheck\n")

  wrnm     <- runModel.getWarningsListName()
  wobj     <- op[[wrnm, exact=TRUE]]
  infolist <- list()
  n        <- length(filevec)
  ok       <- rep(TRUE, n)
  modnum   <- op[["modelNumber", exact=TRUE]]
  cohorts  <- NULL
  for (i in 1:n) {
    f   <- filevec[i]
    obj <- try(meta_getFileInfo(f), silent=TRUE)
    if ("try-error" %in% class(obj)) {
      print(obj)
      wobj  <- runmodel.checkForError(obj, warnStr="ERROR", objStr=f, rem.obj=wobj)
      ok[i] <- FALSE
    } else {
      # See if file contains correct model
      if (!is.null(modnum)) {
        tmp <- modnum %in% tolower(obj$model.numbers)
      } else {
        tmp <- TRUE
      }
      if (!tmp) {
        ok[i]      <- FALSE
        class(obj) <- "try-error"
        msg        <- "File does not contain the correct model"
        wobj       <- runmodel.checkForError(obj, warnStr="ERROR", objStr=f, rem.obj=wobj, msg=msg)
      } else {     
        obj$file          <- f
        len               <- length(infolist)
        infolist[[len+1]] <- obj
        cohorts           <- c(cohorts, obj$cohort)
      }
    }
  }
  m <- sum(ok)
  if (m < op[[metaOp_minNcohortName()]]) stop("ERROR: too few cohorts left after exclusions")
  if (any(duplicated(cohorts))) stop("ERROR: cohort names are not unique")
  op[[wrnm]] <- wobj

  if (DEBUG) cat("End: meta_initFileCheck\n")

  list(file.info.list=infolist, op=op)

}

meta_createTempDir <- function(out.dir, nrand=6) {

  tmp     <- sample(c(letters, as.character(0:9)), nrand)
  dir     <- paste0("tmp_", paste0(tmp, collapse=""))
  out.dir <- checkForSep(out.dir)
  ret     <- paste0(out.dir, dir)
  dir.create(ret)
  if (!dir.exists(ret)) stop(paste0("ERROR: directory ", ret, " not created"))
  ret
}

meta_removeTempDir <- function(dir) {

  if (!length(dir) || !nchar(dir)) return(NULL)
  if (dir.exists(dir)) {
    unlink(dir, recursive=TRUE, force=TRUE, expand=TRUE)
  }
  NULL

}

#' Main function that performs the meta-analysis calculations.
#' @param beta matrix or vector of betas. If a matrix, then the rows
#'             represent the metabolites and the columns represent
#'             the cohorts.
#' @param se matrix or vector of the standard errors for beta. This object
#'        must be in the same order and have the same dimension as beta.
#'            
#' @return List containing the results from fixed-effect and random-effect 
#'         meta-analyses along with Cochran's Q test for heterogeneity.
#'
#' @export
meta_calc <- function(beta, se) {

  meta_check_beta_se(beta, se)
  ret <- meta_core(beta, se)
  ret
}

meta_check_beta_se <- function(beta, se) {

  nm.beta <- "beta"
  nm.se   <- "se"
  len     <- NA
  dim     <- NA
  check_numVecMat(beta, nm.beta, len=NA, dim=NA)
  if (is.matrix(beta)) {
    dim <- dim(beta)
  } else {
    len <- length(beta)
  }
  check_numVecMat(se, nm.se, len=len, dim=dim)
  NULL
}

meta_core <- function(beta, se, only.ret.Q=FALSE) {

  if (is.vector(beta)) dim(beta) <- c(1, length(beta))
  if (is.vector(se)) dim(se) <- c(1, length(se))

  var       <- se*se
  w         <- 1/var
  miss      <- !is.finite(beta) | !is.finite(se) | (se <= 0)
  tmp       <- is.na(miss)
  miss[tmp] <- TRUE
  missFlag  <- any(miss)
  if (missFlag) {
    w[miss]    <- 0
    var[miss]  <- 0
    beta[miss] <- 0
  }
  ncohort  <- rowSums(!miss)
  rowSumsW <- rowSums(w)   

  # Fixed effects
  wgt      <- w/rowSumsW
  betaf    <- rowSums(wgt*beta)
  sef      <- sqrt(1/rowSumsW)
  wgt      <- NULL
  
  # Cochran's Q statistic
  tmp  <- beta - betaf
  q    <- rowSums(tmp*tmp*w)
  phet <- pchisq(q, df=ncohort-1, lower.tail=FALSE)
  
  if (only.ret.Q) {
    tmp <- ncohort %in% 0
    if (any(tmp)) phet[tmp]      <- NA
    ret                          <- list()
    ret[[getMetaHetPvalueCol()]] <- phet
    ret[[getMetaNcohortCol()]]   <- ncohort
    return(ret)
  }
  
  # Random effects
  tmp       <- pmax(0, (q-(ncohort-1))/(rowSumsW - (rowSums(w*w)/rowSumsW)))
  w         <- NULL
  rowSumsW  <- NULL
  gc()
  wgt       <- 1/(var + tmp)
  tmp       <- NULL
  if (missFlag) wgt[miss] <- 0
  miss      <- NULL
  rm(tmp, q)
  gc()
  rsumWrand <- rowSums(wgt)
  wgt       <- wgt/rsumWrand
  betar     <- rowSums(wgt*beta)
  ser       <- sqrt(1/rsumWrand)
  wgt       <- NULL
  rsumWrand <- NULL
  gc()
  
  # P-values
  pvaluef  <- 2*pnorm(abs(betaf/sef), lower.tail=FALSE)
  pvaluer  <- 2*pnorm(abs(betar/ser), lower.tail=FALSE)

  tmp <- ncohort %in% 0
  if (any(tmp)) {
    betaf[tmp]   <- NA
    sef[tmp]     <- NA
    pvaluef[tmp] <- NA
    betar[tmp]   <- NA
    ser[tmp]     <- NA
    pvaluer[tmp] <- NA
    phet[tmp]    <- NA
  }
  ret <- list()
  ret[[getMetaFixedBetaCol()]]    <- betaf
  ret[[getMetaFixedBetaSeCol()]]  <- sef
  ret[[getMetaFixedPvalueCol()]]  <- pvaluef
  ret[[getMetaRandomBetaCol()]]   <- betar
  ret[[getMetaRandomBetaSeCol()]] <- ser
  ret[[getMetaRandomPvalueCol()]] <- pvaluer
  ret[[getMetaHetPvalueCol()]]    <- phet
  ret[[getMetaNcohortCol()]]      <- ncohort

  ret  
}

meta_strat_het_test <- function(df, op) {

  svarCol <- runModel.getStrataColName()
  svalCol <- runModel.getStrataNumColName()
  tmp     <- c(svarCol, svalCol)
  if (!nonEmptyDfHasCols(df, tmp, allcols=1, ignoreCase=0)) return(df)
  
  ret     <- df # Return data frame with added columns

  # Remove any strata if needed
  svar <- unique(unlist(df[, svarCol, drop=TRUE]))
  if (length(svar) != 1) stop("INTERNAL CODING ERROR with strata")
  rem.list <- op[[metaOp_strataToExcludeFromHetTest(), exact=TRUE]]
  if (length(rem.list)) rem.list <- rem.list[[svar, exact=TRUE]]
  if (length(rem.list)) {
    tmp <- !(df[, svalCol, drop=TRUE] %in% rem.list)
    if (!all(tmp)) df <- df[tmp, , drop=FALSE]
  }

  # Get the ids for all rows
  ids     <- meta_getIdNames(df, strata.cols=FALSE)
  strata  <- unlist(df[, svalCol, drop=TRUE]) 
  ustrata <- unique(strata)
  uids    <- unique(ids)
  
  # Perform Cochran's Q test for both fixed and random effects
  bcol   <- getMetaFixedBetaCol()
  secol  <- getMetaFixedBetaSeCol()
  fixed  <- meta_strat_het_test_main(unlist(df[, bcol]), unlist(df[, secol]), 
                                     ids, uids, strata, ustrata)
  bcol   <- getMetaRandomBetaCol()
  secol  <- getMetaRandomBetaSeCol()
  random <- meta_strat_het_test_main(unlist(df[, bcol]), unlist(df[, secol]), 
                                     ids, uids, strata, ustrata)

  # Add columns to data frame
  ret <- meta_strat_het_test_add(ret, uids, fixed, random)

  ret
}

meta_strat_het_test_main <- function(beta, se, ids, uids, strata, ustrata) {

  N          <- length(uids)
  Ns         <- length(ustrata)
  bmat       <- matrix(data=NA, nrow=N, ncol=Ns)
  smat       <- bmat
  for (i in 1:Ns) {
    tmp     <- strata %in% ustrata[i]
    beta2   <- beta[tmp]
    se2     <- se[tmp]
    strata2 <- strata[tmp]
    rows    <- match(uids, ids[tmp])
    tmp     <- !is.na(rows)
    rows    <- rows[tmp]
    if (!length(rows)) next
    bmat[tmp, i] <- beta2[rows]
    smat[tmp, i] <- se2[rows]
  }
  ret <- meta_core(bmat, smat, only.ret.Q=TRUE)
  ret  
}

meta_strat_het_test_add <- function(ret, uids, fixed, random) {

  ids  <- meta_getIdNames(ret, strata.cols=FALSE)
  rows <- match(ids, uids)
  tmp  <- !is.na(rows)
  rows <- rows[tmp]
  if (!length(rows)) return(ret)

  hetpCol       <- getMetaHetPvalueCol() 
  ncohortCol    <- getMetaNcohortCol()  
  pv            <- getMetaStrataHetFixedPCol()
  dfv           <- getMetaStrataHetFixedDfCol()
  ret[, pv]     <- NA
  ret[, dfv]    <- NA
  ret[tmp, pv]  <- fixed[[hetpCol]][rows]
  ret[tmp, dfv] <- fixed[[ncohortCol]][rows] - 1
  pv            <- getMetaStrataHetRandomPCol()
  dfv           <- getMetaStrataHetRandomDfCol()
  ret[, pv]     <- NA
  ret[, dfv]    <- NA
  ret[tmp, pv]  <- random[[hetpCol]][rows]
  ret[tmp, dfv] <- random[[ncohortCol]][rows] - 1

  ret
} 

meta_relevel_terms_main <- function(df, var, from, to) {

  tv      <- tolower(getEffectsTermName())
  var     <- tolower(var)
  varfrom <- paste0(var, ".", from)
  varto   <- paste0(var, ".", to)
  vec     <- df[, tv, drop=TRUE]
  len     <- length(vec)
  ret     <- rep("", len)
  eff     <- getEffectsName()
  for (i in 1:length(varfrom)) {
    tmp <- vec %in% varfrom[i]
    if (!any(tmp)) {
      msg <- paste0(tv, "='", varfrom[i], "' not found in ", eff, " data frame")
      warning(msg)
    }
    ret[tmp] <- varto[i]
  }
  df[, tv] <- ret

  df
}
