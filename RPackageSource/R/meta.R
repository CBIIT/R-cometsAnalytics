# To do:
#   1. Check for model consistency across cohorts
#   2. Allow for multiple strata variables, re-map strata if necessary
#   3. Allow for more than one model per input file
#   4. Re-order columns in results table
#   5. Add additional options
#   6. allow for correlation analyses?

metaAnalysis <- function(filevec, modelName, op) {

  filevec <- checkFiles(filevec)
  op      <- meta_check_op(op) 
  ret     <- meta_main(filevec, modelName, op)
  ret
}

meta_main <- function(filevec, modelName, op) {

  DEBUG                <- op$DEBUG
  if (DEBUG) cat("Begin: meta_main\n")
  op$modelName         <- modelName
  op$modelNumber       <- meta_getModelNumber(modelName)
  tmp                  <- meta_initFileCheck(filevec, op)
  op                   <- tmp$op
  file.info.list       <- tmp$file.info.list
  tmp                  <- meta_loadFilesForAnalysis(file.info.list, op) 
  data.list            <- tmp$data
  file.info.list       <- tmp$file.info.list

  # Check for model consistency across the cohorts
  meta_consistency(file.info.list)

  cohorts              <- names(data.list)
  op$metabs            <- meta_getAllMetabs(data.list)
  op$merge.cohort.cols <- c(getEffectsEstName(), getEffectsEstSeName(), getEffectsPvalueName())
  op$merge.other.cols  <- c(getEffectsExposurespecName(), getEffectsTermName())
  op$beta.vars         <- paste0(getEffectsEstName(), ".", cohorts)
  op$se.vars           <- paste0(getEffectsEstSeName(), ".", cohorts)
  op$pvalue.vars       <- paste0(getEffectsPvalueName(), ".", cohorts)

  ret <- meta_strata_loop(data.list, file.info.list, op)
  ret <- meta_setReturnObj(ret, op)
  if (DEBUG) cat("End: meta_main\n")  

  ret     
}

meta_setReturnObj <- function(obj, op) {

  ret  <- list()
  op   <- obj$op
  wnm  <- runModel.getWarningsListName()
  wobj <- op[[wnm, exact=TRUE]]
  if (is.null(wobj)) wobj <- runModel.getEmptyErrorWarn() 
  ord <- c(getHarmMetabIdCol(), 
           runModel.getStrataColName(), runModel.getStrataNumColName(),
           getEffectsTermName(), 
           getEffectsPvalueName(), getEffectsEstName(), getEffectsEstSeName(),
           getMetaNcohortCol(), getMetaDirectionCol(),
           paste0(getEffectsPvalueName(), ".", op$cohorts),
           getMetaMessageCol())
 
  ret[[metaRetListResultsTable()]] <- orderVars(obj$data, ord)
  ret[[wnm]]                       <- wobj
  ret

}

meta_consistency <- function(file.info.list) {

  # Check model, outcomes, exposures
  meta_model_check(file.info.list)
  meta_outExp_check(file.info.list) 

  NULL
}

meta_model_check <- function(file.info.list) {

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

meta_outExp_check <- function(file.info.list) {

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


  NULL

}


meta_strata_loop <- function(data.list, file.info.list, op) {

  DEBUG <- op$DEBUG
  if (DEBUG) cat("Begin: meta_strata_loop\n")  

  # Check whether it is a stratified run
  tmp       <- meta_strata_check(file.info.list)
  stratFlag <- tmp$stratFlag
  svars     <- tmp$strata.var

  if (stratFlag) {
    # Get all possible strata
    strata        <- meta_getAllStrata(data.list) 
    nstrata       <- length(strata)
    op$strata.var <- svars
    if (DEBUG) cat(paste("nstrata = ", nstrata, " strata vars = ", svars, "\n"))
  } else {
    nstrata       <- 1
    if (DEBUG) cat("Unstratified\n")
  }
  op$stratFlag <- stratFlag
  all          <- NULL
  for (i in 1:nstrata) {
    if (DEBUG && stratFlag) cat(paste0("i=", i, " strata=", strata[i], "\n"))

    # Set the strata so that the merge function will get the correct data
    if (stratFlag) op$strata <- strata[i]
   
    # Loop over the exposure (more than 1 for cat exposure)
    tmp <- meta_term_loop(data.list, file.info.list, op)
    ret <- tmp$data
    op  <- tmp$op
    rm(tmp)
    gc()

    all  <- df.rbind.all(all, ret)
    rm(ret)
    gc() 
  }
  if (DEBUG) cat("End: meta_strata_loop\n") 
  list(data=all, op=op)
}

meta_getAllStrata <- function(data.list) {

  n   <- length(data.list) 
  sv  <- runModel.getStrataNumColName()  # Check for multiple variables
  all <- NULL
  for (i in 1:n) {
    x <- data.list[[i]]
    if (!(sv %in% colnames(x))) stop("INTERNAL CODING ERROR 1")
    strata <- unique(unlist(x[, sv, drop=TRUE]))    
    tmp    <- !(strata %in% all)
    if (any(tmp)) all <- c(all, strata[tmp])
  }
  all

}

meta_strata_check <- function(file.info.list) {

  n       <- length(file.info.list)
  snm     <- getInfoTableStrataName()
  strat   <- 0
  unstrat <- 0
  miss    <- c("", NA, "NA")
  svars   <- rep("", n)
  for (i in 1:n) {
    flist <- file.info.list[[i]]
    info  <- flist$info
    scov  <- getInfoTableValue(info, snm, ifNotFound=NULL, check.len=1)
    if (is.null(scov)) stop("INTERNAL CODING ERROR 1")
    if (scov %in% miss) {
      unstrat <- unstrat + 1
    } else {
      strat   <- strat + 1
    }
    svars[i] <- scov
  }
  if (strat && unstrat) stop("ERROR: cohorts contain both stratified and unstratified results")
  tmp   <- !(svars %in% miss)
  svars <- unique(svars[tmp])
  if (length(svars) > 1) stop("ERROR: cohorts contain different stratification variables")

  list(stratFlag=strat, strata.var=svars)

}

meta_getObjStr <- function(strata, term) {

  ret <- ""
  if (length(strata)) ret <- paste0("strata = ", strata)
  if (length(term)) ret <- paste0(ret, " term = ", term)
  ret <- trimws(ret)
  ret 
}

meta_term_loop <- function(data.list, file.info.list, op) {

  DEBUG <- op$DEBUG
  if (DEBUG) cat("Begin: meta_term_loop\n")  

  if (op$stratFlag) {
    strata <- op$strata
    svars  <- c(runModel.getStrataColName(), runModel.getStrataNumColName())
  } else {
    strata <- NULL
    svars  <- NULL 
  }
  wnm <- runModel.getWarningsListName()

  # Get all possible terms. (Dummy variables for categorical exposures)
  terms    <- meta_getAllTerms(data.list, file.info.list) 
  termFlag <- !is.null(terms)
  termv    <- getEffectsTermName()
  if (termFlag) {
    nterms <- length(terms)
    if (DEBUG) cat(paste0("nterms=", nterms, "\n"))
  } else {
    nterms <- 1
    termv  <- NULL
    if (DEBUG) cat("continuous exposure\n")
  }
  all  <- NULL
  COLS <- c(getHarmMetabIdCol(), svars, termv, op$pvalue.vars)  

  # Loop over the exposure (more than 1 for cat exposures)
  for (i in 1:nterms) {
    if (termFlag) {
      term <- terms[i]
      if (DEBUG) cat(paste0("i=", i, " term=", term, "\n"))
    } else {
      term <- NULL
    } 
    objStr    <- meta_getObjStr(strata, term)
    tmp       <- try(meta_mergeDatalist(data.list, op, term=term, strata=strata), silent=TRUE)
    op[[wnm]] <- runmodel.checkForError(tmp, warnStr="ERROR", objStr=objStr, rem.obj=op[[wnm, exact=TRUE]], msg=NULL) 
    if ("try-error" %in% class(tmp)) {
      if (DEBUG) print(tmp)
      next
    }
    data      <- tmp$data
    op        <- tmp$op
    rm(tmp)
    gc()

    if (!all(COLS %in% colnames(data))) stop("INTERNAL CODING ERROR with COLS")
    metaobj   <- meta_loop(data, op)
    ret       <- data[, COLS, drop=FALSE]
    v         <- getMetaDirectionCol()
    ret[, v]  <- meta_getDirection(data, op$beta.vars)
    nms       <- names(metaobj)
    for (nm in nms) ret[, nm] <- metaobj[[nm]]
    rm(data, metaobj)
    gc()

    all  <- df.rbind.all(all, ret)
    rm(ret)
    gc() 
  }
  if (DEBUG) cat("End: meta_term_loop\n")
  list(data=all, op=op)
}

meta_getAllTerms <- function(data.list, file.info.list) {

  termv   <- getEffectsTermName()
  especv  <- getEffectsExposurespecName()
  for (i in 1:length(data.list)) {
    x    <- data.list[[i]]
    tvec <- x[, termv, drop=TRUE]
    evec <- x[, especv, drop=TRUE]

    # Get the unique term values for a categorical exposure
    # If term = exposurespec for all rows, then exposure is continuous
    if (!all(tvec == evec)) {
      terms <- unique(tvec)
    } else {
      terms <- NULL 
    }
  }
  terms
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

meta_check_op <- function(op) {

  if (!length(op)) op <- list()
  valid <- c(metaOp_methodName(), metaOp_minNcohortName(), "DEBUG")
  def   <- list(metaOp_methodDefault(), metaOp_minNcohortDefault(), 0)
  check.list(op, "op", valid)
  op <- default.list(op, valid, def)
  op
}

meta_loop <- function(alldata, op) {

  DEBUG   <- op$DEBUG
  if (DEBUG) cat("Begin: meta_loop\n")
  betav   <- op$beta.vars
  sev     <- op$se.vars
  N       <- nrow(alldata)
  meta.p  <- meta.se <- meta.beta <- rep(NA, N)
  msg     <- rep("", N)
  ncohort <- rep(0, N)
  for (i in 1:N) {
    betavec      <- unlist(alldata[i, betav, drop=TRUE])
    sevec        <- unlist(alldata[i, sev, drop=TRUE])
    varvec       <- sevec*sevec
    tmp          <- meta_mixmeta(betavec, varvec, op)
    meta.p[i]    <- tmp$pvalue
    meta.beta[i] <- tmp$estimate
    meta.se[i]   <- tmp$se 
    msg[i]       <- tmp$msg
    ncohort[i]   <- tmp$n.cohort
  }
  ret <- list()
  ret[[getEffectsEstName()]]    <- meta.beta
  ret[[getEffectsEstSeName()]]  <- meta.se
  ret[[getEffectsPvalueName()]] <- meta.p
  ret[[getMetaNcohortCol()]]    <- ncohort
  ret[[getMetaMessageCol()]]    <- msg

  if (DEBUG) cat("End: meta_loop\n")
  ret
}

meta_mixmeta <- function(beta, variances, op) {

  ret             <- list(estimate=NA, se=NA, pvalue=NA, q.pvalue=NA, msg="", n.cohort=0)
  tmp             <- is.finite(beta) & (variances > 0)
  tmp[is.na(tmp)] <- FALSE
  beta            <- beta[tmp]
  variances       <- variances[tmp]
  ncohort         <- length(beta)
  ret$n.cohort    <- ncohort
  if (ncohort < op$min.n.cohort) {
    ret$msg <- "Too few cohorts"
    return(ret)
  } 

  obj <- try(mixmeta(beta, variances, method=op$method), silent=TRUE)
  if ("try-error" %in% class(obj)) {
   ret$msg <- getErrorMsgFromTryError(obj, addToEnd=NULL)
   return(ret)
  }
  conv <- obj[["converged", exact=TRUE]]
  if (is.null(conv)) conv <- TRUE
  if (!conv) {
    ret$msg <- "Did not converge"
    return(ret)
  }
  
  sumobj       <- summary(obj)
  coef         <- sumobj$coefficients
  ret$estimate <- coef[1, 1]
  ret$se       <- coef[1, 2]
  ret$pvalue   <- coef[1, 4]
  qstat        <- sumobj[["qstat", exact=TRUE]]
  if (!is.null(qstat)) ret$q.pvalue <- qstat$pvalue

  ret
}

meta_subsetDfByModelNumber <- function(df, modelNumber) {

  if (nonEmptyDf(df)) {
    mcol <- getModelSummaryModelCol()
    if (mcol %in% colnames(df)) {
      modnums <- meta_getModelNumber(df[, mcol, drop=TRUE])
      tmp     <- modnums %in% modelNumber
      df      <- df[tmp, , drop=FALSE] 
    }
  }
  df
}

meta_loadFilesForAnalysis <- function(file.info.list, op) {

  sheet   <- getEffectsName()
  #wnm     <- runModel.getWarningsListName()
  #wobj    <- op[[wnm, exact=TRUE]]
  cols    <- c(getHarmMetabIdCol(), 
               runModel.getStrataColName(), runModel.getStrataNumColName(),
               getEffectsExposurespecName(), getEffectsTermName(), 
               getEffectsEstName(), 
               getEffectsEstSeName(), 
               getEffectsPvalueName())
  numvars <- c(getEffectsEstName(), getEffectsEstSeName(), getEffectsPvalueName())
  modnum  <- op$modelNumber
  nfiles  <- length(file.info.list)
  ok      <- rep(TRUE, nfiles)
  ret     <- list()
  for (i in 1:nfiles) {
    x      <- NULL
    flist  <- file.info.list[[i]]
    f      <- flist$file
    cohort <- flist$cohort
    info   <- flist$info

    # Subset info by model number
    info <- meta_subsetDfByModelNumber(info, modnum)
    if (!nonEmptyDf(info)) stop("INTERNAL CODING ERROR info") 
    flist$info          <- info
    file.info.list[[i]] <- flist 
    
    # Load effects data frame
    x   <- loadDataFrame(f, sheet)

    # Get correct model if file contains more than one model
    x <- meta_subsetDfByModelNumber(x, modnum)
    if (!nonEmptyDf(info)) stop("INTERNAL CODING ERROR effects") 

    cx <- colnames(x)
    if (!all(numvars %in% cx)) stop("INTERNAL CODING ERROR numvars")
    for (v in numvars) x[, v] <- as.numeric(x[, v])
    tmp  <- cols %in% cx
    cols <- cols[tmp]
    if (!length(cols)) stop("INTERNAL CODING ERROR 1") 
    x  <- x[, cols, drop=FALSE]
    ret[[cohort]] <- x   
  }  
  list(data=ret, file.info.list=file.info.list)
}

meta_getAllMetabs <- function(datalist) {

  col <- getHarmMetabIdCol()
  ret <- NULL
  for (i in 1:length(datalist)) {
    x   <- datalist[[i]]
    x   <- unique(x[, col, drop=TRUE])
    tmp <- !(x %in% ret)
    if (any(tmp)) ret <- c(ret, x[tmp])
  }

  ret
}

meta_mergeDatalist <- function(datalist, op, term=NULL, strata=NULL) {

  DEBUG   <- op$DEBUG
  if (DEBUG) cat("Begin: meta_mergeDatalist\n")

  if (length(term) > 1) stop("INTERNAL CODING ERROR 0")
  if (length(strata) > 1) stop("INTERNAL CODING ERROR for strata")
  stratFlag     <- !is.null(strata)
  stratcol      <- runModel.getStrataNumColName()
  stratv        <- runModel.getStrataColName()
  termcol       <- getEffectsTermName()
  termFlag      <- !is.null(term)
  wnm           <- runModel.getWarningsListName()
  wobj          <- op[[wnm, exact=TRUE]]
  cohorts       <- names(datalist)
  metabv        <- getHarmMetabIdCol()
  ret           <- data.frame(metabs=op$metabs, stringsAsFactors=FALSE)
  colnames(ret) <- metabv
  cols          <- op$merge.cohort.cols
  ncols         <- length(cols) 
  if (stratFlag) {
    ret[, stratv]   <- op$strata.var
    ret[, stratcol] <- strata  
  }
  if (termFlag) ret[, termcol] <- term
  
  N <- 0
  for (i in 1:length(datalist)) {
    cohort  <- cohorts[i]
    newcols <- paste0(cols, ".", cohort)
    x       <- datalist[[i]]
    cx      <- colnames(x)
    if (!(metabv %in% cx)) stop("INTERNAL CODING ERROR 1")
    subset <- rep(TRUE, nrow(x))
    if (stratFlag) {
      if (!(stratcol %in% cx)) stop("INTERNAL CODING ERROR 2")
      subset <- x[, stratcol, drop=TRUE] %in% strata
      if (!any(subset)) {
        class(x) <- "try-error"
        msg      <- paste0("Strata ", strata, " not found ")
        wobj     <- runmodel.checkForError(x, warnStr="ERROR", objStr=cohort, rem.obj=wobj, msg=msg)
        if (DEBUG) cat(paste0("Strata ", strata, " not found for cohort ", cohort, "\n"))
        next
      }
    }
    if (termFlag) {
      if (!(termcol %in% cx)) stop("INTERNAL CODING ERROR 3")
      subset <- subset & (x[, termcol, drop=TRUE] %in% term)  
      if (!any(subset)) {
        class(x) <- "try-error"
        msg      <- paste0("Term ", term, " not found ")
        wobj     <- runmodel.checkForError(x, warnStr="ERROR", objStr=cohort, rem.obj=wobj, msg=msg)
        if (DEBUG) cat(paste0("Term ", term, " not found for cohort ", cohort, "\n"))
        next
      }
    } 
    x       <- x[subset, ,  drop=FALSE]
    rows    <- match(op$metabs, x[, metabv, drop=TRUE])
    tmp     <- !is.na(rows)
    rows    <- rows[tmp]
    if (!length(rows)) {
      class(x) <- "try-error"
      msg      <- paste0("No metabolites match (INTERNAL CODING ERROR?)")
      wobj     <- runmodel.checkForError(x, warnStr="ERROR", objStr=cohort, rem.obj=wobj, msg=msg)
      if (DEBUG) cat(paste0("No metabolites match for cohort ", cohort, "\n"))
      next
    }
    for (j in 1:ncols) {
      v           <- newcols[j]
      ret[, v]    <- NA
      ret[tmp, v] <- x[rows, cols[j], drop=TRUE]
    }
    # ok, update N
    N <- N + 1
  }
  op[[wnm]] <- wobj
  if (N < op[[metaOp_minNcohortName()]]) stop("Too few cohorts left for analysis")  

  if (DEBUG) cat("End: meta_mergeDatalist\n")
  list(data=ret, op=op)
}

meta_getModelNumber <- function(x, sep=NULL) {

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

meta_getFileInfo <- function(f) {
 
  nm <- getEffectsName()
  x  <- loadDataFrame(f, nm)
  if (!nonEmptyDf(x)) stop(paste0("ERROR: ", nm, " table not found"))
  
  nm <- getInfoTableDfName()
  x  <- loadDataFrame(f, nm)
  if (!nonEmptyDf(x)) stop(paste0("ERROR: ", nm, " table not found"))
  cohort <- getInfoTableValue(x, "cohort", ifNotFound=NULL, check.len=1) 
  if (!length(cohort)) stop("ERROR: name of cohort not found")
  modname <- getInfoTableValue(x, "model name", ifNotFound=NULL, check.len=0) 
  if (!length(modname)) stop("ERROR: name of model not found")
  modnumbers <- meta_getModelNumber(modname)
  nmodels    <- length(modnumbers)
  if (nmodels != length(unique(modnumbers))) stop("ERROR: model numbers are not unique")

  list(cohort=cohort, model.names=modname, model.numbers=modnumbers, info=x)

}

meta_initFileCheck <- function(filevec, op) {

  wrnm     <- runModel.getWarningsListName()
  wobj     <- op[[wrnm, exact=TRUE]]
  infolist <- list()
  n        <- length(filevec)
  ok       <- rep(TRUE, n)
  modnum   <- op$modelNumber
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
      tmp <- modnum %in% obj$model.numbers
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

  list(file.info.list=infolist, op=op)

}

getFileExt <- function(f, tolower=1) {

  vec <- trimws(unlist(strsplit(f, ".", fixed=TRUE)))
  tmp <- !(vec %in% c("", NA))
  vec <- vec[tmp]
  len <- length(vec)
  if (!len) return(NULL)
  ret <- vec[len]
  if (tolower) ret <- tolower(ret)
  ret
}

loadFile <- function(f, sheets=NULL) {
  
  f   <- trimws(f)
  ext <- getFileExt(f, tolower=1) 
  if (!length(ext)) {
    msg <- paste0("ERROR: file extension could not be determined for ", f)
    stop(msg)
  }
  if (ext == "rda") {
    ret <- myread_rda(f)
  } else if (ext == "xlsx") {
    ret <- myread_xlsx(f, sheets=sheets)
  } else {
    stop(paste0("ERROR: cannot read file with extension ", ext))
  }

  ret
}

loadDataFrame <- function(f, name) {

  x <- loadFile(f, sheets=name)
  if (is.list(x)) x <- x[[name, exact=TRUE]]
  if (!nonEmptyDf(x)) {
    x <- NULL
  } else {
    x <- as.data.frame(x, stringsAsFactors=FALSE)
  }
  x 

}

myread_xlsx <- function(f, sheets=NULL) {

  if (is.null(sheets)) sheets <- readxl::excel_sheets(f)
  nsheets <- length(sheets)
  if (!length(sheets)) {
    msg <- paste0("ERROR: ", f, " contains no sheets")
    stop(msg)
  }
  ret <- list()
  for (sheet in sheets) {
    ret[[sheet]] <- as.data.frame(readxl::read_excel(f, sheet), stringsAsFactors=FALSE)
  }
  ret
}

myread_rda <- function(f) {

  nm <- load(f)
  if (length(nm) != 1) {
    msg <- paste0("ERROR: ", f, " contains more than one object")
    stop(msg)
  }
  ret <- eval(parse(text=nm))
  ret
}