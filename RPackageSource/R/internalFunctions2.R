checkVariableNames <- function(varnames, name, convertMissTo="", default=NULL, only.unique=0,
                               stopOnMissError=1, max.n=0) {

  if (!length(varnames)) return(default)
  varnames <- try(as.vector(varnames), silent=TRUE)
  if ("try-error" %in% class(varnames)) stop(msg_arg_notCharVec(name))
  if (!is.vector(varnames)) stop(msg_arg_notCharVec(name))
  if (max.n && (length(varnames) > max.n)) stop(msg_arg_maxlen(c(name, max.n)))

  # varnames could be all missing
  if (!is.na(convertMissTo)) {
    tmp <- is.na(varnames)
    if (any(tmp)) varnames[tmp] <- convertMissTo
  }
  if (all(is.na(varnames)) && !stopOnMissError && !only.unique) {
    return(varnames)
  }

  if (!is.character(varnames)) stop(msg_arg_notCharVec(name))
  
  varnames <- tolower(trimws(varnames))
  if (only.unique) varnames <- unique(varnames)
  tmp <- nchar(varnames)
  if (stopOnMissError && any(tmp < 1)) stop(msg_arg_colsNotValid(name))  
  tmp <- varnames == "all metabolites"  
  tmp[is.na(tmp)] <- FALSE
  if (any(tmp)) varnames[tmp] <- "All metabolites"

  varnames

} # END: checkVariableNames 

getVariableClass <- function(data, vars) {

  ret <- sapply(data[, vars, drop=FALSE], class)
  ret

} # END: getVariableClass

areCategorical <- function(data, vars) {

  cls <- getVariableClass(data, vars)
  ret <- cls %in% c("factor", "character")
  ret 

} # END: areCategorical

getNewWhereStr <- function(whereStr, operator) {

  ret <- ""
  vec <- strsplit(whereStr, operator, fixed=TRUE)[[1]]
  len <- length(vec)
  if (len) {
    var <- tolower(trimws(vec[1]))
    
    # Remainder of where clause
    if (len > 1) {
      rem <- paste(vec[-1], collapse="", sep="")
    } else {
      rem <- ""
    }
    ret <- paste(var, operator, rem, sep="") 
  }
  
  ret

} # END: getNewWhereStr

# Function to normalize the vector of where clauses
normalizeWhere <- function(whereVec, sep=",") {

  # vec : character vector of where conditions.
  #       Note that vec[i] could contain multiple rules
  #        separated by a comma

  ret <- whereVec
  tmp <- is.na(whereVec)
  if (any(tmp)) whereVec[tmp] <- ""
  for (i in 1:length(whereVec)) {
    str <- trimws(whereVec[i])
    if (!nchar(str)) next
    vec <- unlist(strsplit(str, sep, fixed=TRUE)) 
    tmp <- normalizeWhere.main(vec)
    if (length(tmp) < 2) {
      ret[i] <- tmp
    } else {
      ret[i] <- paste(tmp, collapse=sep, sep="")
    }
  }

  ret

} # END: normalizeWhere

normalizeWhere.main <- function(vec) {

  ret  <- vec
  tmp  <- is.na(vec)
  if (any(tmp)) vec[tmp] <- ""
  rows <- 1:length(vec)
  tmp1 <- grepl("<", vec, fixed=TRUE)
  tmp2 <- grepl(">", vec, fixed=TRUE)
  tmp3 <- grepl("!=", vec, fixed=TRUE)
  tmp4 <- grepl("=", vec, fixed=TRUE) & !tmp1 &!tmp2 & !tmp3

  if (any(tmp1)) {
    r2 <- rows[tmp1]
    for (i in 1:length(r2)) {
      row      <- r2[i]
      ret[row] <- getNewWhereStr(vec[row], "<") 
    }
  } 
  if (any(tmp2)) {
    r2 <- rows[tmp2]
    for (i in 1:length(r2)) {
      row      <- r2[i]
      ret[row] <- getNewWhereStr(vec[row], ">") 
    }
  } 
  if (any(tmp3)) {
    r2 <- rows[tmp3]
    for (i in 1:length(r2)) {
      row      <- r2[i]
      ret[row] <- getNewWhereStr(vec[row], "!=") 
    }
  } 
  if (any(tmp4)) {
    r2 <- rows[tmp4]
    for (i in 1:length(r2)) {
      row      <- r2[i]
      ret[row] <- getNewWhereStr(vec[row], "=") 
    }
  } 

  ret

} # END: normalizeWhere.main

# Function to get the variables from where string
getVarsFromWhereVec <- function(svec) {

  N <- length(svec)
  if (!N) return(NULL)
  if (!is.character(svec)) stop("INTERNAL CODING ERROR in getVarFromWhereStr")
  svec <- trimws(svec)
  ret  <- rep("", N)
  cvec <- c("<", ">", "!", "=")
  for (i in 1:N) {
    str <- svec[i]
    if (!nchar(str)) next 
    for (c in cvec) {
      if (grepl(c, str, fixed=TRUE)) {
        vec    <- strsplit(str, c, fixed=TRUE)[[1]]
        ret[i] <- trimws(vec[1])
        break
      }
    }
  }
  
  ret

} # END: getVarsFromWhereVec

updateWhereStr <- function(whereStr, varMap) {

  N        <- length(whereStr)
  whereStr <- trimws(whereStr)
  ret      <- whereStr
  whereVec <- whereStr
  if (N == 1) {
    # Break up string if it contains multiple rules 
    if (grepl(",", whereStr, fixed=TRUE)) {
      whereVec <- trimws(unlist(strsplit(whereStr, ",", fixed=TRUE)))
    }
  }

  vars     <- getVarsFromWhereVec(whereVec)
  new      <- runModel.getNewVarName(vars, varMap)
  for (i in 1:length(whereVec)) {
    v0 <- vars[i]
    v1 <- new[i]
    if (v0 != v1) whereVec[i] <- gsub(v0, v1, whereVec[i], fixed=TRUE)
  } 
  ret <- paste(whereVec, collapse=",", sep="")
   

  ret

} # END: updateWhereStr

# Function to order columns in a matrix or data frame
orderVars <- function(data, order) {

  # data     matrix or data frame with column names
  # order    Character vector

  if (ncol(data) == 1) return(data)
  cnames <- colnames(data)
  temp   <- order %in% cnames
  order  <- order[temp]
  temp   <- !(cnames %in% order)
  if (any(temp)) order <- c(order, cnames[temp])
    
  data <- data[, order, drop=FALSE]
  data

} # END: orderVars

# Function to assign a default value to an element in a list
default.list <- function(inList, names, default, error=NULL,
                         checkList=NULL) {

  # inList      List
  # names       Vector of names of items in inList
  # default     List of default values to assign if a name is not found
  #             The order of default must be the same as in names.
  # error       Vector of TRUE/FALSE if it is an error not to have the
  #             name in the list. 
  #             The default is NULL
  # checkList   List of valid values for each name.
  #             Use NA to skip a list element.
  #             The default is NULL

  n1 <- length(names)
  n2 <- length(default)
  if (n1 != n2) stop("INTERNAL CODING ERROR 1")

  if (is.null(error)) {
    error <- rep(0, times=n1)
  } else if (n1 != length(error)) {
    stop("INTERNAL CODING ERROR 2")
  }

  if (!is.null(checkList)) {
    if (n1 != length(checkList)) stop("INTERNAL CODING ERROR 3")
    checkFlag <- 1
  } else {
    checkFlag <- 0
  } 

  if (is.null(inList)) inList <- list()

  listNames <- names(inList)
  for (i in 1:n1) {
    if (!(names[i] %in% listNames)) {
      if (!error[i]) {
        inList[[names[i]]] <- default[[i]]
      } else {
        temp <- paste("ERROR: the name ", names[i], " was not found", sep="")
        stop("INTERNAL CODING ERROR 4")
      }
    } else if (checkFlag) {
      temp <- checkList[[i]]
      if (!all(is.na(temp))) {
        if (!all(inList[[names[i]]] %in% checkList[[i]])) {
          temp <- paste("ERROR: the name '", names[i], 
                      "' has an invalid value", sep="")
          stop("INTERNAL CODING ERROR 5")
        }
      }
    }
  }

  inList

} # END: default.list

# Function to un-factor a factor
unfactor <- function(fac, fun=NULL) {

  # fac   Factor
  # fun   Function like as.character or as.numeric, etc

  if (is.factor(fac)) {
    ret <- levels(fac)[fac]
  } else {
    ret <- fac
  }

  if (!is.null(fun)) ret <- fun(ret)

  ret

} # END: unfactor

# Function to parse string of options
parseStr <- function(str, sep=";") {

  ret <- trimws(unlist(strsplit(str, sep, fixed=TRUE)))
  tmp <- nchar(ret) > 0
  ret <- ret[tmp]
  if (!length(ret)) ret <- NULL
  ret

} # END: parseStr

convertVarsToNumeric <- function(data, vars) {

  if (!length(vars)) return(data)
  tmp  <- vars %in% colnames(data)
  vars <- vars[tmp]
  if (!length(vars)) return(data)

  cls <- sapply(data[, vars, drop=FALSE], class)
  tmp <- (cls != "numeric") & (names(cls) %in% vars)
  tmp[is.na(tmp)] <- FALSE
  if (any(tmp)) {
    for (v in names(cls[tmp])) data[, v] <- unfactor(data[, v], fun=as.numeric)
  }

  data

} # END: convertVarsToNumeric

updateStrWithNewVars <- function(str, old, new) {

  ret <- str
  n   <- length(old)
  if (nchar(str) && n) {
    tmp <- old != new
    tmp[is.na(tmp)] <- FALSE
    old <- old[tmp]
    new <- new[tmp]
    m   <- length(old)
    if (m) { 
      for (i in 1:m) ret <- gsub(old[i], new[i], ret, fixed=TRUE)
    } 
  }
  ret

} # END: updateStrWithNewVars

getLogicalValueFromStr <- function(str) {

  str <- tolower(trimws(str))
  if (str %in% c("true", "t")) {
    ret <- TRUE
  } else if (str %in% c("false", "f")) {
    ret <- FALSE 
  } else {
    ret <- as.logical(as.numeric(str))
  }
  ret

} # END: getLogicalValueFromStr

defFunctionString <- function(str) {

  if (!isString(str)) stop(msg_arg_notString("str"))
  str <- trimws(str)
  len <- nchar(str)
  if (len < 1) stop(paste("ERROR: the function string ", str, " is not valid", sep=""))
  if (substr(str, len, len) == ")") str <- substr(str, len-1)
  vec  <- strsplit(str, "(", fixed=TRUE)[[1]]
  fnc  <- vec[1]
  vec  <- vec[-1]
  args <- NULL
  if (length(vec)) args <- paste(vec, collapse="(", sep="")
  ret <- paste("function(x){", fnc, "(x", sep="")
  if (length(args)) ret <- paste(ret, ", ", args, sep="")
  ret <- paste(ret, ")}", sep="")

  ret

} # END: defFunctionString

transformDataFromString <- function(data, vars, funcStr) {

  str <- defFunctionString(funcStr)
  fnc <- eval(parse(text=str))
  for (v in vars) data[, v] <- fnc(data[, v, drop=TRUE])

  data 

} # END: transformDataFromString

newVersionOutToOldOut <- function(x) {

  if (!length(x)) return(x)
  if (!is.list(x)) return(x)
 
  msn  <- getModelSummaryName()
  effn <- getEffectsName()
  ms   <- x[[msn, exact=TRUE]]
  eff  <- x[[effn, exact=TRUE]]

  if (!length(ms) || !length(eff)) return(x)
  info   <- x[[getInfoTableDfName(), exact=TRUE]]
  
  # Merge data frames, if stratified run, then include strata to match rows
  cms  <- colnames(ms)
  ceff <- colnames(eff)
  idv  <- getEffectsRunName()
  sv   <- "strata"
  eids <- eff[, idv, drop=TRUE]
  mids <- ms[, idv, drop=TRUE]
  if (sv %in% cms) {
    eids <- paste(eids, eff[, sv, drop=TRUE], sep=":")
    mids <- paste(mids, ms[, sv, drop=TRUE], sep=":")
  } 
  rows <- match(eids, mids)
  if (any(is.na(rows))) return(x)
  tmp  <- !(cms %in% ceff)
  cms  <- cms[tmp]
  if (length(cms)) ret <- cbind(eff, ms[rows, cms, drop=FALSE])
  rm(ms, eff)
  gc()
  ret <- as.data.frame(ret, stringsAsFactors=FALSE)

  # Add in some variables
  ret[, "cohort"] <- getInfoTableValue(info, "cohort", ifNotFound="")
  ret[, "model"]  <- getInfoTableValue(info, "op$model", ifNotFound="")
  ret[, "spec"]   <- getInfoTableValue(info, "run type", ifNotFound="")

  # Old and new column names. NOTE _NO_COL1_ and _NO_COL2_ are placeholders
  old.req <- c("cohort", "spec", "model", "outcomespec", "exposurespec",
               "corr", "n", "pvalue", 
               "adjspec", "adjvars", "outcome_uid",
               "outcome", "exposure_uid", "exposure", "adj_uid", "adj")
  old.op  <- c("stratavar", "strata")
  new.req <- c("cohort", getModelSummaryRunModeName(), "model", "outcomespec", getEffectsTermName(),
               getEffectsCorrEstName(), getModelSummaryNobsName(), getEffectsPvalueName(),
               "adjvars",  "_NO_COL1_", "outcome_uid",
               "outcome", "exposure_uid", "exposure", "adj_uid", "_NO_COL2_")
  new.op  <- c("stratavar", "strata")

  # Change exposure_uid and exposure 
  vars <- c("exposure_uid", "exposure", "exposurespec", "term")
  if (all(vars %in% colnames(ret))) {
    tmp <- ret[, "exposurespec", drop=TRUE] != ret[, "term", drop=TRUE]
    tmp[is.na(tmp)] <- FALSE
    if (any(tmp)) {
      ret[tmp, "exposure_uid"] <- ret[tmp, "term"]
      ret[tmp, "exposure"]     <- ret[tmp, "term"]
    }
  }

  # Change strata, strata.num, variable sep in new
  sep0 <- runModel.getOldVarSep()
  sep1 <- runModel.getVarSep()
  tmp  <- colnames(ret)
  if (sep0 != sep1) {
    for (v in new.op) {
      if (v %in% tmp) ret[, v] <- gsub(sep1, sep0, ret[, v], fixed=TRUE)
    }
  }
  
  vars <- c("adjvars", "adjspec", "adj_uid")
  vars <- vars[vars %in% colnames(ret)]
  if (length(vars) && (sep0 != sep1)) {
    for (v in vars) ret[, v] <- gsub(sep1, sep0, ret[, v, drop=TRUE], fixed=TRUE)
  }

  # Rename cols
  ret$exposurespec <- NULL
  old <- c(old.req, old.op)
  new <- c(new.req, new.op)
  cx  <- colnames(ret)
  tmp <- (new != old) & (new %in% cx)
  if (any(tmp)) {
    new2 <- new[tmp]
    old2 <- old[tmp]
    for (i in 1:length(new2)) cx[cx %in% new2[i]] <- old2[i]
    colnames(ret) <- cx
  }
  v <- "adjspec"
  if (v %in% colnames(ret)) {
    ret[, "adjvars"] <- ret[, v, drop=TRUE]
    ret[, "adj"]     <- ret[, v, drop=TRUE]
  }
  
  # Get the correct cols and order
  tmp <- old %in% colnames(ret)
  if (any(tmp)) ret <- ret[, old[tmp], drop=FALSE]  

  # Set unadjusted columns to None
  vv  <- c("adjspec", "adjvars", "adj_uid", "adj")
  tmp <- vv %in% colnames(ret)
  vv  <- vv[tmp]
  if (length(vv)) {
    for (v in vv) {
      tmp <- !nchar(trimws(ret[, v, drop=TRUE]))
      if (any(tmp)) ret[tmp, v] <- "None"
    }
  }

  tmp <- attributes(x)
  if (is.list(tmp)) {
    v            <- runmodel.getTimeAttr()
    attr(ret, v) <- tmp[[v, exact=TRUE]]
  }
  rownames(ret) <- NULL

  ret

} # END: newVersionOutToOldOut

parseDelimVec <- function(vec, sep, ncol, numeric=FALSE) {

  mat <- unlist(strsplit(vec, sep, fixed=TRUE))
  if (numeric) mat <- as.numeric(mat)
  if (length(mat) != length(vec)*ncol) return(NULL)
  mat <- matrix(mat, byrow=TRUE, ncol=ncol)

  mat

} # END: parseDelimVec

# Function to break up character vector
parseStratVar <- function(vec, sep) {

  nr <- length(vec)
  if (!nr) return(NULL)
 
  # See if more than one var
  nsep <- stringr::str_count(vec[1], pattern=sep)
  ncol <- nsep + 1

  ret <- matrix(data=vec, nrow=nr, ncol=2) 

  if (nsep) {
    mat <- parseDelimVec(vec, sep, ncol)
    if (length(mat)) {
      flag <- 0
      for (i in 1:ncol) {
        tmp      <- parseDelimVec(mat[, i], "=", 2) 
        if (length(tmp)) {
          if (!flag) {
            ret[, 1] <- trimws(tmp[, 1])
            ret[, 2] <- trimws(tmp[, 2])
            flag     <- 1
          } else {
            ret[, 1] <- trimws(paste(ret[, 1], tmp[, 1], sep=" "))
            ret[, 2] <- trimws(paste(ret[, 2], tmp[, 2], sep=" "))
          }
        } 
      }
    }
  } else {
    tmp <- parseDelimVec(vec, "=", 2)
    if (length(tmp)) {
      ret[, 1] <- trimws(tmp[, 1])
      ret[, 2] <- trimws(tmp[, 2])
    }
  } 
    
  ret   

} # END: parseStratVar

expParms_deltaMethod <- function(beta, beta.se) {

  ret.beta <- exp(beta)
  ret.se   <- ret.beta*beta.se

  list(exp.beta=ret.beta, exp.beta.se=ret.se)
}

getVecFromStr <- function(str, delimiter=",") {
  unlist(strsplit(str, delimiter, fixed=TRUE))
}

getQuotedVecStr <- function(vec, sep=", ") {

  if (!length(vec)) return("")
  ret <- paste0(vec, collapse=sep)
  ret <- paste0("'", ret, "'")
  ret

}

getQuotedVarStr <- function(vec, sep=", ") {

  if (!length(vec)) return("")
  ret <- paste0("'", vec, "'")
  ret <- paste0(ret, collapse=sep)
  ret

}

setMissValToPrev <- function(vec, miss=NA) {

  n <- length(vec)
  if (n < 2) return(vec)
  tmp    <- vec %in% miss
  tmp[1] <- FALSE
  if (!any(tmp)) return(vec)
 
  rows <- (1:n)[tmp] 
  for (i in 1:length(rows)) {
    row      <- rows[i]  
    vec[row] <- vec[row-1]
  }

  vec
}

addNamedValueToList <- function(lst, name, value) {

  if (!length(lst)) lst <- list()
  if (length(name) != 1) return(lst)
  if (!is.list(lst)) stop("ERROR 1")
  if (!is.null(value)) {
    lst[[name]] <- value
  } else {
    lst[name]   <- list(value)
  }
  lst
}

formIdsFromCols <- function(x, cols, sep=":") {

  ncols <- length(cols)
  if (!ncols) stop("ERROR 1")
  if (!all(cols %in% colnames(x))) stop("ERROR 2")
  ret <- x[, cols[1], drop=TRUE]
  if (ncols > 1) {
    for (i in 2:ncols) ret <- paste0(ret, sep, x[, cols[i], drop=TRUE])
  }
  ret
}

getSeqsFromList <- function(inlist) {

  ncomb <- 1
  nc    <- length(inlist)
  for (i in 1:nc) ncomb <- ncomb*length(inlist[[i]])
  nn    <- names(inlist)

  mat <- matrix(NA, nrow=ncomb, ncol=nc)
  if (length(nn)) colnames(mat) <- nn
  for (j in 1:nc) {
     vec  <- inlist[[j]]
     nvec <- length(vec)
     if (j == 1) {
       m <- ncomb/nvec
     } else {
       m <- m/nvec
     }
     mat[, j] <- rep(vec, each=m)
  }

  mat

} # END: getSeqsFromList

removeEOL <- function(str) {

  if (!isString(str)) return(str)
  len <- nchar(str)
  if (!len) return(str)
  flag <- substr(str, len, len) == "\n"
  if (flag) {
    if (len > 1) {
      str <- substr(str, 1, len-1)
    } else {
      str <- ""
    }
  } 
  str
}
