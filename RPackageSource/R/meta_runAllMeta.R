
#' This function allows users to run meta-analyses on all models or a subset of models
#' @param filesFolders Character vector of files and/or folders that contain
#'        the output files from \code{\link{runAllModels}}. 
#'        Valid file extensions are ".xlsx", ".rda", ".zip", ".tar", and ".tar.gz". 
#'        Zip and tar files must contain files with extensions
#'        ".xlsx" or ".rda". See details.
#' @param out.dir Output directory to save the results for each model.
#' @param opfile Excel file containing the models and options.
#'           See \code{\link{meta_opfile}}.
#'           The default is NULL.
#' @return NULL
#' @details The names of the files output from \code{\link{runAllModels}} will be
#'        of the form \cr
#'        <model name>__<cohort name>__<date>.xlsx or \cr
#'        <model name>__<cohort name>__<date>.rda. \cr
#'        These output files from
#'        \code{\link{runAllModels}} can be bundled together into zip or tar files.
#'        There is no file name requirement for the zip and tar files.
#'
#'        The steps used to obtain the initial set of files to include in the meta-analyses are:\cr
#'        1. Recursively list all files found in any folder of \code{filesFolders}. \cr
#'        2. Unzip or untar any zip or tar file. \cr
#'        3. Remove files that do not have a file name of the forms above. \cr 
#'
#' @export
runAllMeta <- function(filesFolders, out.dir, opfile=NULL) {

  filevec <- checkMetaFilesFolders(filesFolders)
  out.dir <- check_out.dir(out.dir)  
  op      <- meta_check_opFile(opfile) 

  runAllMeta_main(filesFolders, out.dir, opfile)
  NULL
}

runAllMeta_main <- function(filevec, out.dir, opfile) {

  # Label for output files
  label  <- getMetaLabelForOutFiles()

  # Read options file
  op    <- meta_readAndSetGlobalOps(opfile) 
  DEBUG <- op$DEBUG

  # Create temp folder to extract files
  tmpdir <- meta_createTempDir(out.dir, nrand=6)
  if (DEBUG) cat(paste0("Temporary folder ", tmpdir, " created\n"))

  # Get all files from folders and by extracting from zip/tar files
  tmp       <- try(meta_extractAllFiles(filevec, tmpdir), silent=FALSE)
  if ("try-error" %in% class(tmp)) {
    meta_removeTempDir(tmpdir)
    stop(msg_meta_20())
  }

  if (DEBUG) print(tmp)
  files.ok  <- tmp$files
  files.rem <- tmp[["removed", exact=TRUE]]

  # Parse file names
  tmp            <- try(meta_parseFileNames(files.ok, op), silent=FALSE)
  if ("try-error" %in% class(tmp)) {
    meta_removeTempDir(tmpdir)
    stop(msg_meta_39())
  }
  finfo          <- tmp$info
  file.rem.fname <- tmp[["removed", exact=TRUE]]
  
  umodels <- unique(finfo[, "model"])
  nmodels <- length(umodels)
  for (i in 1:nmodels) {
    model  <- umodels[i]
    tmp    <- meta_getUseInfo(finfo, model, op)
    finfo2 <- tmp$file.info
    op2    <- tmp$op
    tmp    <- try(runMeta(finfo2[, "file", drop=TRUE], op2), silent=TRUE)
    err    <- "try-error" %in% class(tmp)
    if (err) {
      msg <- getErrorMsgFromTryError(tmp, addToEnd=NULL)
      msg <- msg_meta_40(c(model, msg))
      cat(msg)
      tmp <- getResListFromError(tmp, model)
    }
    writeObjectToFile(tmp, label, model, op2, dir=out.dir)
  }
  # Delete temp folder
  meta_removeTempDir(tmpdir)

  if (DEBUG) cat("End: runAllMeta_main\n")
  NULL
}

meta_getUseInfo <- function(finfo, model, op) {
 
  ret   <- NULL
  tmp   <- finfo[, "model.norm"] %in% meta_normModelStr(model)
  if (!any(tmp)) return(ret)

  # Get correct options for runMeta function
  op      <- meta_setOptions(op, model)
  finfo   <- finfo[tmp, , drop=FALSE]
  cohorts <- op[[metaOp_cohorts.include(), exact=TRUE]]
  if (!is.null(cohorts)) {
    tmp   <- finfo[, "cohort.norm"] %in% meta_normCohortStr(cohorts)
    if (!any(tmp)) return(ret)
    finfo <- finfo[tmp, , drop=FALSE]
  }
  cohorts <- op[[metaOp_cohorts.exclude(), exact=TRUE]]
  if (!is.null(cohorts)) {
    tmp   <- !(finfo[, "cohort.norm"] %in% meta_normCohortStr(cohorts))
    if (!any(tmp)) return(ret)
    finfo <- finfo[tmp, , drop=FALSE]
  }
  list(file.info=finfo, op=op)
 
}

meta_extractAllFiles <- function(fvec, dir) {

  ret   <- NULL  # returned vector of files containing results
  rem   <- NULL  # files removed or error

  # First, get all files in fvec and in the directories
  filevec <- meta_listFiles(fvec)

  # Remove files without correct extensions
  zip01 <- isZipFile(filevec)
  tar01 <- isTarFile(filevec)
  res01 <- isResultFile(filevec)
  tmp   <- !(zip01 | tar01 | res01)
  if (any(tmp)) {
    rem     <- filevec[tmp]
    filevec <- filevec[!tmp]
  }
  if (!length(filevec)) stop(msg_meta_21())

  # Intialize return vector of files to all non zip, tar files
  tmp <- !(zip01 | tar01)
  if (all(tmp)) return(list(files=filevec, removed=rem))
  if (any(tmp)) ret <- filevec[tmp]

  # For the zip and  tar files, extract all files to dir
  if (any(zip01)) {
    for (f in filevec[zip01]) meta_extractFiles(f, dir, ext="zip")
  }  
  if (any(tar01)) {
    for (f in filevec[tar01]) meta_extractFiles(f, dir, ext="tar")
  }  
  f2 <- list.files(dir, full.names=TRUE)

  # Check extracted files
  if (length(f2)) {
    ok  <- isResultFile(f2)
    tmp <- !ok
    if (any(tmp)) rem <- c(rem, f2[tmp])
    f2  <- f2[ok]
  }
  if (length(f2))  ret <- c(ret, f2)
  
  list(files=ret, removed=rem)
}

meta_extractFiles <- function(f, out.dir, ext=NULL) {

  ret <- NULL
  if (is.null(ext)) ext <- getFileExt(f, tolower=1)
  if (!length(ext)) return(ret)
  if (ext == "zip") {
    unzip(f, exdir=out.dir, junkpaths=TRUE)
  } else if ((ext == "tar.gz") || (ext == "tar")) {
    untar(f, exdir=out.dir) 
  }
  NULL
}

meta_listFiles <- function(fvec) {

  # Get all files in the directories
  tmp <- dir.exists(fvec)
  if (!any(tmp)) return(fvec)
  dirs  <- fvec[tmp]
  files <- fvec[!tmp]
  if (!length(files)) files <- NULL
  ndirs <- length(dirs)
  for (i in 1:ndirs) {
    tmp   <- list.files(dirs[i], full.names=TRUE, recursive=TRUE)
    files <- c(files, tmp)
  }
  n <- length(files)
  if (!n) stop(msg_meta_22())
  files <- unique(files)
  files

}

meta_parseFileNames <- function(fvec, op) {

  rem      <- NULL
  n        <- length(fvec)
  ok       <- rep(TRUE, n)
  cohort   <- rep("", n)
  model    <- rep("", n)
  bf       <- basename(fvec)
  sep      <- getOutfileCohortSep()
  mod.i    <- getOutfileModelPart()
  cohort.i <- getOutfileCohortPart()
  for (i in 1:n) {
    vec <- unlist(strsplit(bf[i], sep))
    if (length(vec) < 2) {
      ok[i] <- FALSE
      next
    }
    cohort[i] <- vec[cohort.i]
    model[i]  <- vec[mod.i]
  } 
  if (!any(ok)) stop(msg_meta_23())
  cohort.norm <- meta_normCohortStr(cohort)
  model.norm  <- meta_normModelStr(model)
  
  ret           <- cbind(cohort, cohort.norm, model, model.norm, fvec)
  ret           <- ret[ok, , drop=FALSE]
  rem           <- fvec[!ok]
  if (!length(rem)) rem <- NULL
  colnames(ret) <- c("cohort", "cohort.norm", "model", "model.norm", "file")

  # Subset based on options
  if (length(op)) {
    models <- op[["models", exact=TRUE]]
    if (length(models)) {
      tmp <- ret[, "model.norm"] %in% meta_normModelStr(models)
      ret <- ret[tmp, , drop=FALSE]
      if (!nrow(ret)) stop(msg_meta_24())
    }
  }

  list(info=ret, removed=rem)
}

meta_normModelStr <- function(x) {
  normOutFileStr(tolower(trimws(x)))
}

meta_normCohortStr <- function(x) {
  normOutFileStr(tolower(trimws(x)))
}
