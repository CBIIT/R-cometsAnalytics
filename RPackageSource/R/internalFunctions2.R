checkVariableNames <- function(varnames, name, convertMissTo="", default=NULL, only.unique=0,
                               stopOnMissError=1, max.n=0) {

  if (!length(varnames)) return(default)
  varnames <- try(as.vector(varnames), silent=TRUE)
  if ("try-error" %in% class(varnames)) stop(paste(name, " must be a character vector", sep=""))
  if (!is.vector(varnames)) stop(paste(name, " must be a character vector", sep=""))
  if (max.n && (length(varnames) > max.n)) stop(paste(name, " must have length ", max.n, sep=""))

  # varnames could be all missing
  if (!is.na(convertMissTo)) {
    tmp <- is.na(varnames)
    if (any(tmp)) varnames[tmp] <- convertMissTo
  }
  if (all(is.na(varnames)) && !stopOnMissError && !only.unique) {
    return(varnames)
  }

  if (!is.character(varnames)) stop(paste(name, " must be a character vector", sep=""))
  
  varnames <- tolower(trimws(varnames))
  if (only.unique) varnames <- unique(varnames)
  tmp <- nchar(varnames)
  if (stopOnMissError && any(tmp < 1)) stop(paste("some variable names in ", name, " are not valid", sep=""))  
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

# Function to check that an object is a string
isString <- function(obj) {

  if ((length(obj) == 1) && is.character(obj)) {
    ret <- TRUE
  } else {
    ret <- FALSE
  }

  ret

} # END: isString

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
  if (n1 != n2) stop("ERROR: in calling default.list")

  if (is.null(error)) {
    error <- rep(0, times=n1)
  } else if (n1 != length(error)) {
    stop("ERROR: in calling default.list")
  }

  if (!is.null(checkList)) {
    if (n1 != length(checkList)) stop("ERROR: in calling default.list")
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
        stop(temp)
      }
    } else if (checkFlag) {
      temp <- checkList[[i]]
      if (!all(is.na(temp))) {
        if (!all(inList[[names[i]]] %in% checkList[[i]])) {
          temp <- paste("ERROR: the name '", names[i], 
                      "' has an invalid value", sep="")
          stop(temp)
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

  if (!isString(str)) stop("ERROR: str must be a string")
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

