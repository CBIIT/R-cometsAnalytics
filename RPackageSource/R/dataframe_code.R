nonEmptyDf <- function(x) {

  if (!length(x)) return(0)
  if (!is.data.frame(x)) return(0)
  if (!nrow(x)) return(0)
  if (!ncol(x)) return(0) 
  1

}

nonEmptyDfHasCols <- function(x, cols, allcols=1, ignoreCase=0) {

  ret <- 0
  if (nonEmptyDf(x)) {
    cx  <- colnames(x)
    if (ignoreCase) {
      cols <- toupper(cols)
      cx   <- toupper(cx) 
    }
    tmp <- cols %in% cx
    if (allcols) {
      if (all(tmp)) ret <- 1
    } else {
      if (any(tmp)) ret <- 1
    }
  }
  ret

}

addColsToDF <- function(base.df, base.id, x.df, x.id, x.add, init=1, DEBUG=0) {

  if (!length(x.add)) return(base.df)
  if (!nonEmptyDfHasCols(base.df, base.id)) return(base.df)
  if (!nonEmptyDfHasCols(x.df, x.id)) return(base.df)
  base.cols <- colnames(base.df)
  x.cols    <- colnames(x.df)
  tmp       <- (x.add %in% x.cols) 
  rem       <- x.add[!tmp] 
  x.add     <- x.add[tmp]
  if (DEBUG && length(rem)) {
    print(paste0("Removed columns: ", paste0(rem, collapse=",")))
  }
  if (!length(x.add)) return(base.df)

  # Initialize
  if (init) for (v in x.add) base.df[, v] <- NA_character_
  rows <- match(base.df[, base.id, drop=TRUE], x.df[, x.id, drop=TRUE])
  tmp  <- !is.na(rows)
  rows <- rows[tmp]
  if (length(rows)) {
    for (v in x.add) base.df[tmp, v] <- x.df[rows, v, drop=TRUE]
  }

  base.df

}

df.add.cols <- function(addToDf, x, x.cols, miss.num=NA, miss.char="") {

  tmp    <- !(x.cols %in% colnames(addToDf)) & (x.cols %in% colnames(x))
  x.cols <- x.cols[tmp]
  n.cols <- length(x.cols)
  if (!n.cols) return(addToDf)
  for (i in 1:n.cols) {
    col <- x.cols[i]
    if (is.numeric(x[, col, drop=TRUE])) {
      addToDf[, col] <- miss.num
    } else {
      addToDf[, col] <- miss.char
    }
  }
  addToDf

} # END: df.add.cols

df.add1ColAfterCol <- function(df, vecOrConst, new.nm, after.nm) {

  # Assumes df is valid and has the columns
  if (!isString(new.nm) || !isString(after.nm)) stop("ERROR new.nm/after.nm")
  if (new.nm == after.nm) stop("ERROR new.nm==after.nm")
  nr <- nrow(df)
  n  <- length(vecOrConst)
  if (n == 1) vecOrConst <- rep(vecOrConst, nr)
  if (length(vecOrConst) != nr) stop("ERROR with vecOrConst")
  cx  <- colnames(df)
  tmp <- cx %in% new.nm
  if (any(tmp)) cx <- cx[!tmp]
  ii <- match(after.nm, cx)
  if (!is.finite(ii)) stop("ERROR 1")
  nc  <- length(cx)
  ord <- c(cx[1:ii], new.nm)
  if (ii < nc) ord <- c(ord, cx[(ii+1):nc]) 
  df[, new.nm] <- vecOrConst
  df           <- df[, ord, drop=FALSE]
  df

}

df.rbind.common <- function(base, new, doNotRemoveCols=NULL) {

  ok.base <- nonEmptyDf(base)
  ok.new  <- nonEmptyDf(new)
  if (ok.base && !ok.new) {
    return(base)
  } else if (!ok.base && ok.new) {
    return(new)
  } else if (!ok.base && !ok.new) {
    # Watch out for data frames with no rows, try not to return NULL
    if (is.data.frame(base)) return(base) 
    if (is.data.frame(new)) return(new) 
    return(NULL)
  }

  cx.base <- colnames(base)
  cx.new  <- colnames(new)
  tmp     <- (cx.base %in% cx.new) | (cx.base %in% doNotRemoveCols)
  cx.keep <- cx.base[tmp]
  if (!length(cx.keep)) stop("INTERNAL CODING ERROR 1 in df.rbind.common")  
  base    <- base[, cx.keep, drop=FALSE]
  tmp     <- (cx.new %in% cx.base) | (cx.new %in% doNotRemoveCols)
  cx.keep <- cx.new[tmp]
  if (!length(cx.keep)) stop("INTERNAL CODING ERROR 2 in df.rbind.common")  
  new     <- new[, cx.keep, drop=FALSE]
  base    <- df.add.cols(base, new, colnames(new),  miss.num=NA, miss.char="")
  new     <- df.add.cols(new, base, colnames(base), miss.num=NA, miss.char="")
  cx.base <- colnames(base)
  cx.new  <- colnames(new)
  if (!all(cx.new %in% cx.base)) stop("INTERNAL CODING ERROR 3 in df.rbind.common")
  ret     <- rbind(base, new[, cx.base, drop=FALSE])
  ret

} # END: df.rbind.common

df.rbind.all <- function(base, new) {

  ok.base <- nonEmptyDf(base)
  ok.new  <- nonEmptyDf(new)
  if (ok.base && !ok.new) {
    return(base)
  } else if (!ok.base && ok.new) {
    return(new)
  } else if (!ok.base && !ok.new) {
    # Watch out for data frames with no rows, try not to return NULL
    if (is.data.frame(base)) return(base) 
    if (is.data.frame(new)) return(new) 
    return(NULL)
  }

  base    <- df.add.cols(base, new, colnames(new),  miss.num=NA, miss.char="")
  new     <- df.add.cols(new, base, colnames(base), miss.num=NA, miss.char="")
  cx.base <- colnames(base)
  cx.new  <- colnames(new)
  if (!all(cx.new %in% cx.base)) stop("INTERNAL CODING ERROR 1 in df.rbind.all")
  ret     <- rbind(base, new[, cx.base, drop=FALSE])
  ret

} # END: df.rbind.all

df.checkRemConstantCol <- function(df, col) {

  cx <- colnames(df)
  if (col %in% cx) {
    vec <- df[, col, drop=TRUE]
    if (length(unique(vec)) < 2) {
      tmp <- !(cx %in% col)
      cx  <- cx[tmp]
      df  <- df[, cx, drop=FALSE]
    }
  }
  df
}