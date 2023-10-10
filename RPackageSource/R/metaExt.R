#' This function will combine output files assumed to be from the same cohort and same model.
#' @param filevec Character vector of files that contain
#'        the output files from \code{\link{runModel}} or \code{\link{runAllModels}}. 
#'        Valid file extensions are ".xlsx", and ".rda". 
#' @param out.file NULL or a file to save the results. The file extension must
#'        be either ".xlsx", or ".rda".
#' @param op A list containing the options to use or NULL. The options are \code{precedence}
#'           and \code{check.consistency} with default values of "nobs" and
#'           TRUE respectively (see details).
#'           The default is NULL.
#' @return List of data frames containing the merged results
#'
#' @details This function will merge output files containing results from the same model
#'          and same cohort. Within a file, metabolites with missing results (
#'          \code{estimate} and \code{std.error}) will be excluded.
#'          The files can have different and overlapping metabolites.
#'          When there is an overlapping metabolite among the files, the option
#'          \code{precedence="nobs"} will choose the file with the largest number 
#'          of subjects for that metabolite. With \code{precedence="data"}, the first
#'          file in \code{filevec} that contains the metabolite will be chosen.
#'          The option \code{check.consistency} will check for the same 
#'          outcome, exposure, and stratification variable names across the files.
#'
#' @export

mergeResultsFiles <- function(filevec, out.file, op=NULL) {

  # Check arguments
  filevec <- checkMetaFiles(filevec, valid.ext=c(getOutTypeOpRda(), getOutTypeOpExcel()))
  if (length(out.file)) out.file <- check_out.file(out.file, valid.ext=getOutTypeOpVals())
  op  <- mrf_check_op(op)
  ret <- mergeResultsFiles_main(filevec, out.file, op)
  ret
}

mrf_default_options <- function() {

  valid <- c(mrf_precedenceOpName(), mrf_consistencyOpName())
  def   <- list(mrf_precedenceDefault(), mrf_consistencyDefault())

  names(def) <- valid
  def
}

mrf_check_op <- function(x, name="op") {

  if (!length(x)) x <- list()

  # Get default options
  def   <- mrf_default_options()
  valid <- names(def)
  check.list(x, name, valid)
  x   <- default.list(x, valid, def)

  v <- mrf_precedenceOpName()
  check.string(x[[v]], mrf_precedenceValid(), paste0("op$", v))
  v <- mrf_consistencyOpName()
  check.logical(x[[v]], paste0("op$", v)) 

  x
}

mergeResultsFiles_main <- function(filevec, out.file, op) {

  tmp       <- mrf_load_data(filevec, op)
  data.list <- tmp$data.list
  exc.error <- tmp[["excluded", exact=TRUE]]
  tmp       <- NULL
  gc()

  # Check for consistency across the files
  mrf_consistency(data.list, op)

  all.ids   <- mrf_getAllIds(data.list)
  inc.mat   <- mrf_getIncMat(data.list, all.ids) 
  tmp       <- mrf_mergeMsEff(data.list, inc.mat, all.ids, op) 
  eff       <- tmp$eff
  ms        <- tmp$ms
  used      <- tmp$used.files
  files     <- tmp$files
  rm(tmp, all.ids, inc.mat)
  gc()
  
  # Merge Info sheets
  info <- mrf_mergeInfo(data.list, used) 
  wobj <- mrf_rbindSheets(data.list, used, runModel.getWarningsListName()) 
  tab1 <- mrf_rbindSheets(data.list, used, getTable1DfName()) 
  rm(data.list)
  gc()

  # Errors_warnings could be NULL
  if (!length(wobj)) wobj <- runModel.getEmptyErrorWarn()
 
  ret                                   <- list()
  ret[[getModelSummaryName()]]          <- ms
  ret[[getEffectsName()]]               <- eff
  ret[[runModel.getWarningsListName()]] <- wobj
  ret[[getTable1DfName()]]              <- tab1
  ret[[getInfoTableDfName()]]           <- info

  if (length(out.file)) saveObjectByFileExt(ret, out.file)

  inc <- files[used]
  if (!length(inc)) inc <- NULL
  exc <- files[!used]
  if (!length(exc)) exc <- NULL   

  list(result=ret, files.error=exc.error, files.included=inc, files.excluded=exc)
} 

mrf_consistency <- function(data.list, op) {

  if (!op[[mrf_consistencyOpName(), exact=TRUE]]) return(NULL)

  n    <- length(data.list)
  svar <- runModel.getStrataColName()
  tvar <- getEffectsTermName()
  ovar <- getModelSummaryOutUidCol()
  evar <- getModelSummaryExpUidCol()
  svec <- ovec <- evec <- rep("", n)

  for (i in 1:n) {
    x  <- data.list[[i]]$ms
    cx <- colnames(x)
    f  <- data.list[[i]]$file

    # Strata
    if (svar %in% cx) svec[i] <- mrf_check_col(x, svar, "stratification", f)
      
    # outcome_uid and exposure_uid should be in model summary
    ovec[i] <- mrf_check_col(x, ovar, "outcome", f)
    evec[i] <- mrf_check_col(x, evar, "exposure", f)
  }

  if (length(unique(svec)) > 1) stop("ERROR: files have different stratification variables")
  if (length(unique(ovec)) > 1) stop("ERROR: files have different outcome variables")
  if (length(unique(evec)) > 1) stop("ERROR: files have different exposure variables")

  NULL
}

mrf_check_col <- function(df, col, col.desc, f) {

  # Col could be a column of metabolites
  n   <- nrow(df)
  b   <- ceiling(n/2)
  vec <- unique(df[, col, drop=TRUE])
  m   <- length(vec)

  # Check for unique var, or possibly all metabs
  if (m > 1) {
    if (m <= b) {
      msg <- paste0("ERROR: more than one ", col.desc, " variable in file ", f)
      stop(msg)
    } else {
      # Probably all metabolites
      vec <- "*"
    }
  }
  vec[1]
}

mrf_rbindSheets <- function(data.list, used, sheet) {

  n      <- length(data.list)
  ret    <- NULL
  fcol   <- mrf_FileCol() 
  for (i in 1:n) {
    if (used[i]) {
      f <- data.list[[i]]$file
      x <- try(loadDataFrame(f, sheet))
      if ("try-error" %in% class(x)) next
      if (!length(x) || !nrow(x)) next

      # Add indicator
      x[, fcol] <- f
 
      ret <- df.rbind.all(ret, x) 
    }
  }
  ret
}

mrf_mergeInfo <- function(data.list, used) {

  isheet <- getInfoTableDfName()
  nmv    <- getInfoTableNameCol()
  valv   <- getInfoTableValueCol()
  n      <- length(data.list)
  ret    <- NULL
  sep    <- mrf_InfoMrgSep()
  for (i in 1:n) {
    if (used[i]) {
      info <- try(loadDataFrame(data.list[[i]]$file, isheet))
      if ("try-error" %in% class(info)) next
      # Change missing to empty string
      tmp <- is.na(info[, valv, drop=TRUE])
      if (any(tmp)) info[tmp, valv] <- "" 
      if (!length(ret)) {
        ret <- info
      } else {
        # Compare names and values
        rows <- match(ret[, nmv, drop=TRUE], info[, nmv, drop=TRUE])
        tmp0 <- !is.na(rows)
        rows <- rows[tmp0]
        if (!length(rows)) next
        info <- info[rows, , drop=FALSE]
        rows <- (1:nrow(ret))[tmp0] 
        tmp  <- ret[tmp0, valv, drop=TRUE] != info[, valv, drop=TRUE]
        if (!any(tmp)) next
        rows <- rows[tmp]
        ret[rows, valv] <- paste0(ret[rows, valv, drop=TRUE], sep, info[tmp, valv, drop=TRUE])  
      }
    }
  }
  ret
}

mrf_mergeMsEff <- function(data.list, inc.mat, all.ids, op) {

  ret.ms   <- NULL
  ret.eff  <- NULL
  n        <- length(data.list)
  rv       <- getEffectsRunName()
  pv       <- mrf_precedenceOpName()
  prec     <- op[[pv, exact=TRUE]]
  dataFlag <- prec == mrf_precedenceData()
  nobsFlag <- prec == mrf_precedenceNobs()

  # Keep track of the files the merged results come from
  used.files <- rep(FALSE, n)
  filevec    <- rep("", n)

  # Get the file indicator that gives max number of subs
  if (nobsFlag) maxind <- mrf_getMaxNsubInd(data.list, all.ids) 

  # If precedence is "data", keep track of ids included
  use.ids <- rep(TRUE, length(all.ids))
  for (i in 1:n) {
    filevec[i] <- data.list[[i]]$file
    if (nobsFlag) {
      # One maxind value for each id 
      tmp <- maxind %in% i
      tmp <- data.list[[i]]$ms.ids %in% all.ids[tmp]
    } else {
      # Exclude ids that were included previously
      tmp <- data.list[[i]]$ms.ids %in% all.ids[use.ids]
    }
    if (!any(tmp)) next
    msids         <- data.list[[i]]$ms.ids[tmp]
    ms            <- data.list[[i]]$ms[tmp, , drop=FALSE]  
    tmp           <- data.list[[i]]$eff[, rv, drop=TRUE] %in% ms[, rv, drop=TRUE]
    eff           <- data.list[[i]]$eff[tmp, , drop=FALSE]
    ret.eff       <- df.rbind.all(ret.eff, eff) 
    ret.ms        <- df.rbind.all(ret.ms, ms) 
    used.files[i] <- TRUE
    tmp           <- all.ids %in% msids
    use.ids[tmp]  <- FALSE
  }
  if (!length(ret.eff)) stop("ERROR: no rows to include in effects data frame")
  if (!length(ret.ms)) stop("ERROR: no rows to include in model summary data frame")
  if (!any(used.files)) stop("ERROR: No files included")

  list(eff=ret.eff, ms=ret.ms, used.files=used.files, files=filevec)
}

mrf_getMaxNsubInd <- function(data.list, all.ids) {

  n        <- length(data.list)
  nids     <- length(all.ids)
  max.nobs <- rep(-9999, nids)
  max.ind  <- rep(-9999, nids)
  nv       <- getModelSummaryNobsName()
  for (i in 1:n) {
    rows <- match(all.ids, data.list[[i]]$ms.ids)
    tmp  <- !is.na(rows)
    rows <- rows[tmp]
    if (!length(rows)) next 

    nobs            <- rep(-9999, nids)
    nobs[tmp]       <- data.list[[i]]$ms[rows, nv, drop=TRUE]
    tmp             <- nobs > max.nobs
    tmp[is.na(tmp)] <- FALSE
    if (any(tmp)) {
      max.nobs[tmp] <- nobs[tmp]
      max.ind[tmp]  <- i
    }
    rm(rows, tmp, nobs)
    gc()
  }
  if (any(max.ind < 0)) stop("INTERNAL CODING ERROR")
  max.ind
}

mrf_getIncMat <- function(data.list, all.ids) {

  # Get a logical matrix determining which metabs are in which file
  n   <- length(data.list)
  m   <- length(all.ids)
  ret <- matrix(FALSE, nrow=m, ncol=n)
  for (i in 1:n) {
    ret[, i] <- all.ids %in% data.list[[i]]$ms.ids 
  }
  ret
}

mrf_getAllIds <- function(data.list) {

  n   <- length(data.list)
  ret <- unique(data.list[[1]]$ms.ids)
  if (n < 2) return(ret)
  for (i in 2:n) {
    ret <- unique(c(ret, data.list[[i]]$ms.ids))
  }
  ret
}

mrf_load_data <- function(filevec, op) {

  n         <- length(filevec)
  idv       <- getEffectsRunName()
  runsep    <- mrf_runSep()
  ms.sheet  <- getModelSummaryName()
  eff.sheet <- getEffectsName()
  exc       <- NULL
  data.list <- list()
  ok        <- rep(FALSE, n)
  for (i in 1:n) {
    # Read in effects sheet
    eff   <- try(loadDataFrame(filevec[i], eff.sheet))
    if ("try-error" %in% class(eff)) next
    eff   <- mrf_subset_eff(eff)
    if (!length(eff)) next
    
    # Read in model summary
    ms <- try(loadDataFrame(filevec[i], ms.sheet))
    if ("try-error" %in% class(ms)) next
    ms <- try(mrf_subset_ms(ms, eff)) 
    if ("try-error" %in% class(ms)) next

    # Get the ids, we only need the uids, model and strata, not term
    ids <- meta_getIdNames(ms, term.col=FALSE) 

    # Change run numbers to other ids to prevent colliding with other files
    ms[, idv]      <- paste0(i, runsep, ms[, idv, drop=TRUE])
    eff[, idv]     <- paste0(i, runsep, eff[, idv, drop=TRUE])

    ok[i]          <- TRUE
    k              <- length(data.list) + 1 
    data.list[[k]] <- list(eff=eff, ms=ms, ms.ids=ids, file=filevec[i]) 
  }
  if (!all(ok)) {
    print(filevec[!ok])
    warning("The above files were removed due to errors. See log.")
  }
  exc <- filevec[!ok]
  if (!length(exc)) exc <- NULL
  
  list(data.list=data.list, excluded=exc)
}

mrf_subset_eff <- function(x) {

  bv  <- getEffectsEstName()
  sev <- getEffectsEstSeName() 
  cx  <- colnames(x)
  if (!nonEmptyDfHasCols(x, bv, allcols=1)) return(NULL)
  tmp <- is.finite(as.numeric(x[, bv, drop=TRUE]))
  if (!all(tmp)) x <- x[tmp, , drop=FALSE]
  if (!nrow(x)) return(NULL)
  if (sev %in% cx) {
    vec <- as.numeric(x[, sev, drop=TRUE])
    tmp <- is.finite(vec) & (vec > 0)
    tmp[is.na(tmp)] <- FALSE
    if (!all(tmp)) x <- x[tmp, , drop=FALSE]
  }
  if (!nrow(x)) x <- NULL
  x
}

mrf_subset_ms <- function(ms, eff) {

  rv  <- getEffectsRunName()
  ids <- eff[, rv, drop=TRUE]
  tmp <- ms[, rv, drop=TRUE] %in% ids
  if (!all(tmp)) ms <- ms[tmp, , drop=FALSE]
  if (!nrow(ms)) stop("ERROR matching model summary and effects")
  ms
}






meta2_check_file.list <- function(x, nm="file.list") {

  if (!is.list(x)) stop(paste("ERROR: ", nm, " must be a list", sep=""))
  n <- length(x)
  if (n < 2) stop(paste("ERROR: ", nm, " must have length > 1", sep=""))
  for (i in 1:n) {
    meta2_check_file.list.elmnt(x[[i]], paste0(nm, "[[", i, "]]"))
  }
  NULL
}

meta2_check_file.list.elmnt <- function(x, nm) {

  # Must be a string or a list
  if (!isString(x) && !is.list(x)) {
    stop(paste0("ERROR: ", nm, " must be a file or a list"))
  }
  if (is.list(x)) {
    check.list(x, nm, "file")
    f <- x[["file", exact=TRUE]]
  } else {
    f <- x
  }
  if (!file.exists(f)) stop(paste0("ERROR file ", x, " does not exist"))
  NULL
}

meta2_getFileInfo <- function(fobjlist) {

  ret <- list()
  for (i in 1:length(fobjlist)) {
    x <- fobjlist[[i]]
    if (isString(x)) x <- list(file=x)
    x$comets.file <- isCometsOutFile(x$file) 
    ret[[i]] <- x 
  }
  ret
}

isCometsOutFile <- function(f) {

  ret  <- FALSE
  lowf <- tolower(f)
  if (is.list(f)) {
    nms <- names(f)
    if (all(cometsReqOutSheetNames() %in% nms)) ret <- TRUE 
  } else if (isExcelFile(lowf)) {
    sheets <- try(readxl::excel_sheets(lowf), silent=TRUE)
    if ("try-error" %in% class(sheets)) return(ret)
    sheets <- tolower(sheets)
    req    <- tolower(cometsReqOutSheetNames())
    if (all(req %in% sheets)) ret <- TRUE 
  } else {
    tmp <- try(myread_rda(f), silent=TRUE)
    if ("try-error" %in% class(tmp)) return(ret)
    if (!is.list(tmp)) return(ret)
    nms <- names(tmp)
    if (all(cometsReqOutSheetNames() %in% nms)) ret <- TRUE 
  }
  ret
}

areCometsOutFiles <- function(fvec) {

  n   <- length(fvec)
  if (!n) return(FALSE)
  ret <- rep(FALSE, n)
  for (i in 1:n) ret[i] <- isCometsOutFile(fvec[i])
  ret
}


