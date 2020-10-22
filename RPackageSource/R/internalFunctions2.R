checkVariableNames <- function(varnames, name, convertMissTo="", default=NULL, only.unique=0,
                               stopOnMissError=1) {

  if (!length(varnames)) return(default)
  varnames <- try(as.vector(varnames), silent=TRUE)
  if ("try-error" %in% class(varnames)) stop(paste(name, " must be a character vector", sep=""))
  if (!is.character(varnames)) stop(paste(name, " must be a character vector", sep=""))
  if (!is.vector(varnames)) stop(paste(name, " must be a character vector", sep=""))

  if (!is.na(convertMissTo)) {
    tmp <- is.na(varnames)
    if (any(tmp)) varnames[tmp] <- convertMissTo
  }
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
normalizeWhere <- function(vec) {

  # vec : character vector of where conditions
  
  ret  <- vec
  tmp  <- is.na(vec)
  if (any(tmp)) vec[tmp] <- ""
  rows <- 1:length(vec)
  tmp1 <- grepl("<", vec, fixed=TRUE)
  tmp2 <- grepl(">", vec, fixed=TRUE)
  tmp3 <- grepl("==", vec, fixed=TRUE)
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
      ret[row] <- getNewWhereStr(vec[row], "==") 
    }
  } 

  ret

} # END: normalizeWhere

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
