##########################################################################
# Lists
##########################################################################

isList <- function(x) {

  ret <- is.list(x) && ("list" %in% class(x))
  ret
}

checkOptionListNames <- function(op, valid, name) {
    if (!length(op)) return(NULL)

    # Names cannot be ""
    nms <- trimws(names(op))
    tmp <- nchar(nms) < 1
    if (any(tmp)) {
      print(op)
      stop(msg_arg_listNotValid(name)) 
    }
    if (length(valid)) {
      tmp <- !(nms %in% valid)
      if (any(tmp)) {
        err <- paste(nms[tmp], collapse=",", sep="")
        if (length(err) > 1) {
          stop(msg_arg_opsNotValidFor(c(err, name)))
        } else {
          stop(msg_arg_opNotValidFor(c(err, name)))
        }
      }
    }  

    NULL

} # END: checkOptionListNames

checkRequiredListNames <- function(x, req, name) {

  if (!length(x)) stop(msg_arg_len0(name))
  if (!is.list(x)) stop(msg_arg_notList(name))

  tmp  <- !(req %in% names(x))
  miss <- req[tmp]
  if (length(miss)) {
    tmp <- paste0(miss, collapse=", ")
    msg <- msg_arg_objNotIn(c(tmp, name))
    stop(msg)  
  }
  
  NULL

} # END: checkRequiredListNames

check.list <- function(x, name, valid) {

  if (!isList(x)) stop(msg_arg_notList(name))
  checkOptionListNames(x, valid, name) 
  NULL 

} # END: check.list

##########################################################################
# vector
##########################################################################

check.vector <- function(x, name, min.len=0, len=0) {

  lenx <- length(x)
  if (!lenx && min.len) stop(msg_arg_len0(name))
  if (len && (lenx != len)) stop(msg_arg_lenN(c(name, len)))
  if (!is.vector(x)) stop(msg_arg_notVec(name))
  NULL
}

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
  if (!errFlag) {
    obj <- trimws(obj)
    if (!(obj %in% valid)) errFlag <- 1
  }
  if (errFlag) {
    msg <- getQuotedVarStr(valid)
    msg <- msg_arg_objNotValid(c(parm, msg))
    stop(msg)
  }

  obj

} # END: check.string

##########################################################################
# integer
##########################################################################

check.integer <- function(x, nm, minvalue=0) {

  if (length(x) != 1) stop(msg_arg_lenN(c(nm, 1)))
  if (!is.numeric(x)) stop(msg_arg_notInt(nm))
  if (x != floor(x)) stop(msg_arg_notInt(nm))
  if (!is.finite(x)) stop(msg_arg_notInt(nm))
  if (length(minvalue)) {
    tmp <- x < minvalue
    tmp[is.na(tmp)] <- TRUE
    if (tmp) stop(msg_arg_objNotGEQ(c(nm, minvalue)))
  }
  NULL
}

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
    msg <- msg_arg_objNotValid(c(parm, msg))
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
    msg <- msg_arg_notTF(name)
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
      msg <- msg_arg_objNotInRange(c(name, op1, lower, op2, upper))
    } else {
      msg <- msg_arg_objNotGEQ(c(name, lower))
    }
    stop(msg)
  }

  x

} # END: check.range

check_numVec <- function(x, nm, len=NA) {

  n <- length(x)
  if (!n) stop(msg_arg_len0(nm))
  if (!is.numeric(x)) stop(msg_arg_notNumVec(nm))
  if (!is.vector(x)) stop(paste0(msg_arg_notNumVec(nm)))
  if (is.finite(len) && (n != len)) {
    stop(msg_arg_lenN(c(nm, len)))
  }
  NULL
}

check_numMat <- function(x, nm, dim=NA) {

  if (!is.matrix(x)) stop(msg_arg_notNumMat(nm))
  if (!is.numeric(x)) stop(msg_arg_notNumMat(nm))
  nr <- nrow(x)
  nc <- ncol(x)
  if (!nr) stop(msg_arg_0rows(nm))
  if (!nc) stop(msg_arg_0cols(nm))
  if (length(dim) == 2) {
    if (nr != dim[1]) stop(msg_arg_Nrows(c(nm, dim[1])))
    if (nc != dim[2]) stop(msg_arg_Ncols(c(nm, dim[2])))
  }
  NULL
}

check_numVecMat <- function(x, nm, len=NA, dim=NA) {

  if (is.matrix(x)) {
    check_numMat(x, nm, dim=dim)
  } else if (is.vector(x)) {
    check_numVec(x, nm, len=len) 
  } else {
    stop(msg_arg_notNumMatVec(nm))
  }
  NULL
}

##########################################################################
# files
##########################################################################

checkFiles <- function(filevec, name="filevec") {

  if (!length(filevec)) stop(msg_arg_noFiles())
  if (!is.character(filevec)) stop(msg_arg_notCharVec(name))
  filevec <- trimws(filevec)
  tmp     <- !file.exists(filevec)
  if (any(tmp)) {
    print(filevec[tmp])
    stop(msg_arg_filesDNE())
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

  if (!isString(x)) stop(msg_arg_outfile(nm))
  x <- trimws(x)
  if (length(valid.ext)) {
    tmp <- checkFileExtensions(tolower(x), checkForExt=valid.ext)
    if (!tmp) {
      ext <- paste0(getOutTypeOpVals(), collapse=", ")
      msg <- msg_arg_fileExtNotValid(c(nm, ext))
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

  if (!isString(x)) stop(msg_arg_dir(name))
  x <- trimws(x)
  if (!dir.exists(x)) stop(msg_arg_dir(name))
  x <- checkForSep(x)
  x
}

check_out.dir <- function(x, name="out.dir") {

  if (!isString(x)) stop(msg_arg_notString(name))
  x <- trimws(x)
  if (!dir.exists(x)) stop(msg_arg_dirDNE(x))
  if (file.access(x, mode=2)) stop(msg_arg_dirNoWrtPerm(x))

  x <- gsub("\\", "/", x, fixed=TRUE)
  x <- checkForSep(x)
  x
}


