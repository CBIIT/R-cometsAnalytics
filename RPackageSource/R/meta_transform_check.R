dfToComets.fobj.valid.names <- function() {

  ret <- c(dfToC_file(), dfToC_fileSep(), 
           dfToC_cohort(), getCorrModelName(),
           dfToC_se.col(), dfToC_est.col(),       
           dfToC_nobs.col(), dfToC_nobs(),  
           dfToC_outcome.col(), dfToC_outcome.name(),   
           dfToC_exposure.col(), dfToC_exposure.name(), 
           dfToC_stratavar.col(), dfToC_strata.col(), 
           dfToC_strata.name(), dfToC_strata.value(),  
           dfToC_model.col(), dfToC_model.name(),     
           dfToC_change.col.values(),
           dfToC_where(), dfToC_newRef()
         )
  ret
}

dfToComets.check_changeValues <- function(x, name=NULL) {

  # x can be a list with names col, old, new or a list of sublists
  len <- length(x)
  if (!length(x)) return(NULL)
  if (!length(name)) name <- dfToC_change.col.values()
  col.nm <- dfToC_change.col()
  old.nm <- dfToC_change.old() 
  new.nm <- dfToC_change.new()
  req    <- c(col.nm, old.nm, new.nm)
  if (is.list(x) && all(req %in% names(x))) x <- list(x)

  for (i in 1:length(x)) {
    xi <- x[[i]]
    checkRequiredListNames(xi, req, name)
    old <- xi[[old.nm, exact=TRUE]]
    new <- xi[[new.nm, exact=TRUE]]
    check.vector(old, paste0(name, "$", old.nm), min.len=1, len=0)
    check.vector(new, paste0(name, "$", new.nm), min.len=0, len=length(old))
  }
  x
}

dfToComets.check_where <- function(x, name) {

  if (!length(x)) return(NULL)
  if (!is.vector(x)) stop(paste0("ERROR: ", name, " must be a character vector"))
  if (!is.character(x)) stop(paste0("ERROR: ", name, " must be a character vector"))

  NULL
}

dfToComets.check_fobj.i <- function(x, name="file.obj") {

  if (!isString(x) && !is.list(x)) {
    stop(paste0("ERROR: ", name, " must be a list or a string"))
  }
  if (isString(x)) {
    if (!file.exists(x)) stop(paste0("ERROR: file ", x, " not found"))
    x <- list(file=x)
  } 
  f <- x[["file", exact=TRUE]]
  if (is.null(f)) stop("ERROR: ", name, "$file not specified")
  if (!file.exists(f)) stop(paste0("ERROR: file ", f, " not found"))

  # Check for valid option
  valid <- dfToComets.fobj.valid.names()
  checkOptionListNames(x, valid, name) 

  nm  <- dfToC_change.col.values()
  obj <- x[[nm, exact=TRUE]]
  if (!is.null(obj)) {
    obj     <- dfToComets.check_changeValues(obj)
    x[[nm]] <- obj
  }

  nm  <- dfToC_where()
  obj <- x[[nm, exact=TRUE]]
  if (!is.null(obj)) dfToComets.check_where(obj, paste0("name$", nm))

  x
}

dfToComets.check_fobj <- function(x, name="file.obj") {

  if (!length(x)) stop(paste0("ERROR: ", name, " has length 0"))
  if (!is.character(x) && !is.list(x)) {
    stop(paste0("ERROR: ", name, " must be a list or a character vector"))
  }
  N   <- length(x)
  nms <- paste0(name, "[[", 1:N, "]]") 
  for (i in 1:N) {
    x[[i]] <- dfToComets.check_fobj.i(x[[i]], name=nms[i])
  }
  x
}
