vardef_catvars <- function(vmap, subjdata) {
 
  # Check the category values in the varmap sheet against the actual data

  sheet  <- getVarMapSheetName()
  vref   <- tolower(getVarMapVarRefCol())
  vdef   <- tolower(getVarMapVarDefCol())
  vtype  <- tolower(getVarMapVarTypeCol())
  catstr <- getVarMapVarTypeCat()
  cx     <- colnames(vmap)
  tmp    <- c(vref, vdef, vtype)
  if (!all(tmp %in% cx)) return(NULL)
  vec    <- tolower(trimws(vmap[, vtype, drop=TRUE]))
  tmp    <- substr(vec, 1, 3) %in% substr(catstr, 1, 3)
  if (!any(tmp)) return(NULL)
  refvec <- tolower(trimws(vmap[tmp, vref, drop=TRUE]))
  defvec <- tolower(trimws(vmap[tmp, vdef, drop=TRUE]))
  cx     <- colnames(subjdata)
  for (i in 1:length(refvec)) {
    var <- refvec[i]
    def <- defvec[i]
    if (!(var %in% cx)) {
      msg <- msg_rci_23(var)
      warning(msg)
      next
    }

    # Get the categories
    defcats <- vardef_getCats(def) 

    # Check against the data. A group variable for conditional logistic models
    #   is problematic.
    datacats <- as.character(unique(subjdata[, var, drop=TRUE]))
    tmp      <- !(datacats %in% defcats)
    m        <- sum(tmp)
    if (m && (m < 10)) {
      datacats <- datacats[tmp]
      str      <- vardef_getMissCatStr(datacats)
      msg      <- msg_rci_24(c(var, str, toupper(vdef), sheet))
      stop(msg)
    }
  }
  NULL
}

vardef_getMissCatStr <- function(x) {

  maxn <- 3
  flag <- 0
  if (length(x) > maxn) {
    x    <- x[1:maxn]
    flag <- 1
  }
  xstr <- paste0(x, collapse=", ")
  if (flag) xstr <- paste0(xstr, ", ...")
  xstr

}

vardef_getCats <- function(str) {

  ret <- NULL
  vec <- trimws(unlist(strsplit(trimws(str), "(", fixed=TRUE)))
  if (!length(vec)) return(ret)
  vec <- vec[nchar(vec) > 0] 
  if (length(vec) != 2) return(ret)
  x   <- vec[2]
  vec <- trimws(unlist(strsplit(x, ",", fixed=TRUE)))
  vec <- vec[nchar(vec) > 0]
  n   <- length(vec)
  if (!n) return(ret)
  for (i in 1:n) {
    vec2 <- trimws(unlist(strsplit(vec[i], "=", fixed=TRUE)))
    if (length(vec2)) vec2 <- vec2[nchar(vec2) > 0]
    if (length(vec2)) ret <- c(ret, vec2[1])
  }
  ret
}