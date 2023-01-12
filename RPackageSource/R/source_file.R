getFileExt <- function(f, tolower=1, tarflag=1) {

  vec <- trimws(unlist(strsplit(f, ".", fixed=TRUE)))
  tmp <- !(vec %in% c("", NA))
  vec <- vec[tmp]
  len <- length(vec)
  if (!len) return(NULL)
  if (tolower) vec <- tolower(vec)
  ret <- vec[len]
  if (tarflag && (ret == "gz")) {
    if ((len > 1) && (vec[len-1] == "tar")) ret <- "tar.gz"
  } else if (ret == "gz") {
    if (len > 1) ret <- vec[len-1]
  }
  ret
}

loadFile <- function(fobj, sheets=NULL) {
 
  if (is.list(fobj)) {
    f <- fobj$file
    if (is.null(sheets)) sheets <- fobj[[filelistSheetOp(), exact=TRUE]]
  } else {
    f <- fobj
  }
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
    #stop(paste0("ERROR: cannot read file with extension ", ext))
    sep <- NULL 
    if (is.list(fobj)) sep <- fobj[["sep", exact=TRUE]]
    if (is.null(sep)) sep <- getFileDelim(f, default="")
    ret <- read.table(f, header=1, sep=sep, as.is=TRUE, check.names=FALSE)
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

myread_rda <- function(f) {

  nm <- load(f)
  if (length(nm) != 1) {
    msg <- paste0("ERROR: ", f, " contains more than one object")
    stop(msg)
  }
  ret <- eval(parse(text=nm))
  ret
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


isExcelFile <- function(filevec) {

  len <- nchar(filevec)
  ret <- (tolower(substr(filevec, len-4, len)) == ".xlsx") 
  tmp <- is.na(ret)
  if (any(tmp)) ret[tmp] <- FALSE
  ret
}

isRdaFile <- function(filevec) {

  len <- nchar(filevec)
  ret <- (tolower(substr(filevec, len-3, len)) == ".rda") 
  tmp <- is.na(ret)
  if (any(tmp)) ret[tmp] <- FALSE
  ret
}

isResultFile <- function(filevec) {

  len     <- nchar(filevec)
  filevec <- tolower(filevec)
  exts    <- paste0(".", getOutTypeOpVals())
  lenext  <- nchar(exts)
  ret     <- rep(FALSE, length(filevec))
  exts    <- tolower(exts)
  filevec <- tolower(filevec)
  for (i in 1:length(exts)) {
    tmp <- substr(filevec, len-lenext[i]+1, len) == exts[i]
    tmp[is.na(tmp)] <- FALSE
    ret <- ret | tmp
  }
  ret
}

isZipFile <- function(filevec) {

  len <- nchar(filevec)
  ret <- tolower(substr(filevec, len-3, len)) == ".zip"
  tmp <- is.na(ret)
  if (any(tmp)) ret[tmp] <- FALSE
  ret
}

isTarFile <- function(filevec) {

  len <- nchar(filevec)
  ret <- (tolower(substr(filevec, len-3, len)) == ".tar") | 
         (tolower(substr(filevec, len-6, len)) == ".tar.gz")
  tmp <- is.na(ret)
  if (any(tmp)) ret[tmp] <- FALSE
  ret
}

getFileDelim <- function(f, default="") {

  ret    <- default
  x      <- scan(f, nlines=2, what="character", sep="\n", quiet=TRUE)
  nx     <- length(x)
  if (!nx) stop("ERROR: file has no rows")
  n2Flag <- nx > 1
  seps   <- c("\t", " ", ",")  
  nseps  <- length(seps)
  n1     <- rep(0, nseps)
  
  for (i in 1:nseps) n1[i] <- length(getVecFromStr(x[1], delimiter=seps[i]))
  ord   <- order(n1)
  if (n2Flag) {
    n2 <- rep(0, nseps)
    for (i in 1:nseps) n2[i] <- length(getVecFromStr(x[2], delimiter=seps[i]))
  } else {
    n2 <- n1
  }
  seps  <- seps[ord]
  n1    <- n1[ord]
  n2    <- n2[ord]
  tmp   <- n1 == n2
  seps  <- seps[tmp]
  n1    <- n1[tmp]
  n2    <- n2[tmp]
  nseps <- length(seps)
  if (nseps && (n1[nseps] > 1)) {
    ret <- seps[nseps]
  } else {
    # Try white space
    if (n2Flag) {
      n1 <- length(scan(f, nlines=1, what="character", sep="", quiet=TRUE))
      n2 <- length(scan(f, nlines=1, skip=1, what="character", sep="", quiet=TRUE))
    }
    if (n1 == n2) ret <- ""
  } 

  ret

} 





