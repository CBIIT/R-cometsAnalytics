runMeta2_main <- function(file.list, op) {

  



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
  if (isExcelFile(lowf)) {
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
