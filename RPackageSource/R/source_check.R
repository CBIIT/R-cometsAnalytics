##########################################################################
# Lists
##########################################################################
checkOptionListNames <- function(op, valid, name) {
    if (!length(op)) return(NULL)

    # Names cannot be ""
    nms <- trimws(names(op))
    tmp <- nchar(nms) < 1
    if (any(tmp)) {
      print(op)
      stop(paste("ERROR: the above ", name, " list is not valid", sep="")) 
    }
    tmp <- !(nms %in% valid)
    if (any(tmp)) {
      err <- paste(nms[tmp], collapse=",", sep="")
      if (length(err) > 1) {
        stop(paste("ERROR: ", err, " are not valid option names for ", name, sep=""))
      } else {
        stop(paste("ERROR: ", err, " is not a valid option name for ", name, sep=""))
      }
    }  

    NULL

} # END: checkOptionListNames

checkRequiredListNames <- function(x, req, name) {

  if (!length(x)) stop(paste0(name, " has length 0"))
  if (!is.list(x)) stop(paste0(name, " must be a list"))

  tmp  <- !(req %in% names(x))
  miss <- req[tmp]
  if (length(miss)) {
    tmp <- paste0(miss, collapse=", ")
    msg <- paste0("ERROR: the objects ", tmp, " are not in ", name)
    stop(msg)  
  }
  
  NULL

} # END: checkRequiredListNames

check.list <- function(x, name, valid) {

  if (!is.list(x)) stop(paste("ERROR: ", name, " must be a list", sep=""))
  checkOptionListNames(x, valid, name) 
  NULL 

} # END: check.list

##########################################################################
# char, string
##########################################################################

# Function to check that an object is a string
isString <- function(obj) {

  if ((length(obj) == 1) && is.character(obj)) {
    ret <- TRUE
  } else {
    ret <- FALSE
  }

  ret

} # END: isString

check.string <- function(obj, valid, parm) {

  # obj:   A character string (length 1)
  # valid: Character vector of valid values
  # parm:  The name of the argument being checked

  errFlag <- 0
 
  # Check for errors
  if (!isString(obj)) errFlag <- 1 
  obj <- trimws(obj)
  if (!(obj %in% valid)) errFlag <- 1

  if (errFlag) {
    msg <- paste(valid, collapse=", ")
    msg <- paste("ERROR: ", parm, " must be one of ", msg, sep="")
    stop(msg)
  }

  obj

} # END: check.string

##########################################################################
# numeric
##########################################################################

check.number <- function(obj, valid, parm) {

  # obj:   Numeric (length 1)
  # valid: Numeric vector of valid values
  # parm:  The name of the argument being checked

  errFlag <- 0
 
  # Check for errors
  if ((length(obj) != 1) || !is.numeric(obj) || !(obj %in% valid)) {
    errFlag <- 1 
  }

  if (errFlag) {
    msg <- paste(valid, collapse=", ")
    msg <- paste("ERROR: ", parm, " must be one of ", msg, sep="")
    stop(msg)
  }

  obj

} # END: check.number

check.logical <- function(x, name) {

  err <- 0
  n   <- length(x)
  if (!n || (n > 1)) {
    err <- 1
  } else {
    if ((x != 0) && (x != 1)) err <- 1
  }
  if (err) {
    msg <- paste("ERROR: ", name, " must be TRUE or FALSE", sep="")
    stop(msg)
  }

  x

} # END: check.logical

check.range <- function(x, name, lower, upper, upper.inc=TRUE) {

  err <- 0
  n   <- length(x)
  op1 <- ">= "
  op2 <- "<= "
  if (!n || (n > 1) || !is.finite(x)) {
    err <- 1
  } else {
    if ((x < lower) || (x > upper)) err <- 1
    if (!upper.inc && (x == upper)) {
      err <- 1
      op2 <- "< "
    }
  }
  if (err) {
    if (is.finite(upper)) {
      msg <- paste("ERROR: ", name, " must be ", op1,  
                   lower, " and ", op2, upper, sep="")
    } else {
      msg <- paste("ERROR: ", name, " must be >= ", lower, sep="")
    }
    stop(msg)
  }

  x

} # END: check.range

check_numVec <- function(x, nm, len=NA) {

  n <- length(x)
  if (!n) stop(paste0("ERROR: ", nm, " has length 0"))
  if (!is.numeric(x)) stop(paste0("ERROR: ", nm, " must be a numeric vector"))
  if (!is.vector(x)) stop(paste0("ERROR: ", nm, " must be a numeric vector"))
  if (is.finite(len) && (n != len)) {
    stop(paste0("ERROR: ", nm, " must have length ", len))
  }
  NULL
}

check_numMat <- function(x, nm, dim=NA) {

  if (!is.matrix(x)) stop(paste0("ERROR: ", nm, " must be a numeric matrix"))
  if (!is.numeric(x)) stop(paste0("ERROR: ", nm, " must be a numeric matrix"))
  nr <- nrow(x)
  nc <- ncol(x)
  if (!nr) stop(paste0("ERROR: ", nm, " has 0 rows"))
  if (!nc) stop(paste0("ERROR: ", nm, " has 0 columns"))
  if (length(dim) == 2) {
    if (nr != dim[1]) stop(paste0("ERROR: ", nm, " must have ", dim[1], " rows"))
    if (nc != dim[2]) stop(paste0("ERROR: ", nm, " must have ", dim[2], " columns"))
  }
  NULL
}

check_numVecMat <- function(x, nm, len=NA, dim=NA) {

  if (is.matrix(x)) {
    check_numMat(x, nm, dim=dim)
  } else if (is.vector(x)) {
    check_numVec(x, nm, len=len) 
  } else {
    stop(paste0("ERROR: ", nm, " must be a numeric matrix or vector"))
  }
  NULL
}

##########################################################################
# files
##########################################################################

checkFiles <- function(filevec, name="filevec") {

  if (!length(filevec)) stop("ERROR: No files specified")
  if (!is.character(filevec)) stop(paste0("ERROR: ", name, " must be a character vector"))
  filevec <- trimws(filevec)
  tmp     <- !file.exists(filevec)
  if (any(tmp)) {
    print(filevec[tmp])
    stop("ERROR: the above file(s) do not exist")
  }
  filevec
}

checkFileExtensions <- function(filevec, checkForExt=c(".xlsx", ".rda"), case=0) {

  tmp <- substr(checkForExt, 1, 1) != "."
  if (any(tmp)) checkForExt <- paste0(".", checkForExt[tmp])
  lens   <- unique(nchar(checkForExt))
  ok     <- rep(FALSE, length(filevec))
  flen   <- nchar(filevec)
  if (!case) {
    checkForExt <- tolower(checkForExt)
    filevec     <- tolower(filevec)
  }
  for (i in 1:length(lens)) {
    len <- lens[i]
    tmp <- substr(filevec, flen-len+1, flen) %in% checkForExt
    ok  <- ok | tmp
  }
  ok
}

check_out.file <- function(x, nm="out.file", valid.ext=NULL, delIfExists=1) {

  if (!isString(x)) stop(paste0("ERROR: ", nm, " must be an output file name"))
  x <- trimws(x)
  if (length(valid.ext)) {
    tmp <- checkFileExtensions(tolower(x), checkForExt=valid.ext)
    if (!tmp) {
      ext <- paste0(getOutTypeOpVals(), collapse=", ")
      msg <- paste0("ERROR: ", nm," must have a valid file extension (",
                    ext, ")")
      stop(msg) 
    } 
  }
  dir <- dirname(x)
  check_out.dir(dir, name=nm) 
  if (delIfExists && file.exists(x)) file.remove(x)
  x 

}

##########################################################################
# directory
##########################################################################

checkForSep <- function(x) {

  sep <- .Platform$file.sep
  x   <- trimws(x)
  n   <- nchar(x)
  if (!n) {
    x <- "./"
  } else {
    if (substr(x, n, n) != sep) x <- paste0(x, sep)
  }
  x
}

check.dir <- function(x, name) {

  if (!isString(x)) stop(paste0("ERROR: ", name, " must be a folder name"))
  x <- trimws(x)
  if (!dir.exists(x)) stop(paste0("ERROR: ", name, " must be a folder name"))
  x <- checkForSep(x)
  x
}

check_out.dir <- function(x, name="out.dir") {

  if (!isString(x)) stop(paste0("ERROR: ", name, " must be a character string"))
  x <- trimws(x)
  if (!dir.exists(x)) stop(paste0("ERROR: ", x, " does not exist"))
  if (file.access(x, mode=2)) stop(paste0("ERROR: ", x, " does not have write permission"))

  x <- gsub("\\", "/", x, fixed=TRUE)
  x <- checkForSep(x)
  x
}


