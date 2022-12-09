runModel.getTable1 <- function(ret, modeldata, op) {

  if (!is.list(ret)) return(ret)
  tab1 <- try(getTable1(modeldata, op), silent=TRUE)
  if (!("try-error" %in% class(tab1))) {
    ret[[getTable1DfName()]] <- tab1
  }

  ret
}

getTable1ColNames <- function() {

  sVar.v   <- runModel.getStrataColName()
  sNum.v   <- runModel.getStrataNumColName()
  catCols  <- c("category")
  contCols <- c("min", "quartile1", "median", "mean", "quartile3", "max", "n.missing")
  ov0      <- "n.outcomeEqual0"
  ov1      <- "n.outcomeEqual1"
  cx       <- c(sVar.v, sNum.v, "variable", "in.model", "type", catCols)
  cx       <- c(cx, c("n", ov0, ov1, "n.unique"))
  cx       <- c(cx, contCols)

  list(cols=cx, sVar.v=sVar.v, sNum.v=sNum.v, catCols=catCols, contCols=contCols,
       ov0=ov0, ov1=ov1)
}

getTable1 <- function(modeldata, op) {

  modeldata <- table1_getVars(modeldata)
  vars      <- c("rcovs", "ccovs", "acovs", 
                 "timecov", "groupcov", "wgtcov", "offcov")
  nms       <- c("outcome", "exposure", "adjustment", 
                 "time", "group", "weight", "offset")
  yv        <- modeldata[["rcovs", exact=TRUE]]
  sv        <- modeldata[["scovs", exact=TRUE]]

  # See if outcome is a binary variable
  yv.bin <- 0
  if ((length(yv) == 1) && isBinaryVar(modeldata$gdta, yv)) yv.bin <- 1
  allvars   <- NULL
  allmtypes <- NULL
  for (i in 1:length(vars)) {
    v     <- vars[i]
    mtype <- nms[i]
    tmp   <- modeldata[[v, exact=TRUE]]
    m     <- length(tmp)
    if (m) {
      allvars   <- c(allvars, tmp)
      allmtypes <- c(allmtypes, rep(mtype, m))   
    }
  }
  tot     <- length(allvars)
 
  # Determine if stratified
  sFlag <- length(sv)
  if (sFlag) {
    stratvec  <- runModel.getStratVec(modeldata$gdta, sv)
    stratlist <- unique(stratvec)
    nstrata   <- length(stratlist)
    if (nstrata > op$max.nstrata) sFlag <- 0
  }
  if (!sFlag) {
    nstrata   <- 1
    stratvec  <- rep(1, nrow(modeldata$gdta))
    stratlist <- 1
    sv        <- "SV"
  }
  sv2 <- paste0(sv, collapse=" ")
  
  # Return column names 
  tmp      <- getTable1ColNames()
  sVar.v   <- tmp$sVar.v
  sNum.v   <- tmp$sNum.v
  catCols  <- tmp$catCols
  contCols <- tmp$contCols
  ov0      <- tmp$ov0
  ov1      <- tmp$ov1
  cx       <- tmp$cols
  ret      <- matrix(data="", nrow=2*tot, ncol=length(cx))
  colnames(ret) <- cx
  ncatCols      <- length(catCols)
  ncontCols     <- length(contCols)
  numvars       <- c("n", ov0, ov1, "n.unique", contCols)
  ncontVar      <- 0
  ncatVar       <- 0

  row <- 0
  for (strat in 1:nstrata) {
    strata <- stratlist[strat]
    tmp    <- stratvec %in% strata
    data   <- (modeldata$gdta)[tmp, , drop=FALSE] 
    yeq0   <- data[, yv, drop=TRUE] %in% 0 
    yeq1   <- data[, yv, drop=TRUE] %in% 1
    #stratS <- table1_getStratStr(sv, strata)
    for (i in 1:tot) {
      var     <- allvars[i]
      mtype   <- allmtypes[i]
      catvec  <- rep("", ncatCols)
      contvec <- rep(NA, ncontCols)
      vec     <- data[, var, drop=TRUE]
      if ("numeric" %in% class(vec)) {
        type     <- "continuous"
        contFlag <- 1
        ncontVar <- ncontVar + 1
      } else {
        type     <- "categorical"
        contFlag <- 0
        ncatVar  <- ncatVar + 1
      }

      if (contFlag) {
        contvec    <- table1_getContVec(vec)   
        tmp        <- is.finite(vec)
        N          <- sum(tmp)
        N.unq      <- length(unique(vec[tmp])) 
        N.0        <- sum(yeq0 & tmp)
        N.1        <- sum(yeq1 & tmp)
        row        <- row + 1
        tmp        <- c(sv2, strata, var, mtype, type, catvec, N, N.0, N.1, N.unq, contvec)
        ret[row, ] <- tmp
        if (row == nrow(ret)) ret <- rbind(ret, ret) 
      } else {
        # Loop over each category
        cats  <- sort(unique(vec))
        ncats <- length(cats) 
        for (j in 1:ncats) {
          cat        <- cats[j]
          tmp        <- vec %in% cat
          N          <- sum(tmp)
          N.unq      <- NA
          N.0        <- sum(yeq0[tmp])
          N.1        <- sum(yeq1[tmp])
          row        <- row + 1
          tmp        <- c(sv2, strata, var, mtype, type, cat, N, N.0, N.1, N.unq, contvec)
          if (j > 1) tmp[3:5] <- ""
          ret[row, ] <- tmp
          if (row == nrow(ret)) ret <- rbind(ret, ret) 
        }
      }
    }
  }
  ret <- as.data.frame(ret[1:row, , drop=FALSE], stringsAsFactors=FALSE)
  for (v in numvars) ret[, v] <- as.numeric(ret[, v])
  rem <- NULL
  if (!yv.bin) rem <- c(rem, ov0, ov1)
  if (!sFlag) rem <- c(rem, sVar.v, sNum.v)
  if (!ncatVar) rem <- c(rem, catCols)
  if (!ncontVar) rem <- c(rem, contCols, "n.unique")
  if (length(rem)) {
    cx  <- colnames(ret)
    tmp <- !(cx %in% rem)
    cx  <- cx[tmp]
    if (length(cx)) ret <- ret[, cx, drop=FALSE] 
  }

  ret
}

table1_getStratStr <- function(sv, strat) {

  nsv   <- length(sv)
  strat <- as.character(strat)
  vec   <- trimws(unlist(strsplit(strat, " ", fixed=TRUE)))
  tmp   <- !(vec %in% c("", "NA", NA))
  vec   <- vec[tmp]
  if (length(vec) != nsv) vec <- rep("", nsv) 
  ret <- paste(sv, vec, sep="=")
  ret <- paste0(ret, collapse=", ")
  ret

}

table1_getContVec <- function(vec) {

  ret <- summary(vec)
  len <- length(ret)
  if (len == 7) {
    return(ret)
  } else if (len == 6) {
    ret <- c(ret, 0)
  } else {
    ret <- rep(NA, 7)
  }
  ret
}

table1_getVars <- function(modeldata) {

  ret    <- modeldata
  metabs <- names(modeldata[["dict_metabnames", exact=TRUE]])
  vars   <- c("rcovs", "ccovs", "acovs", "scovs", "timecov",
              "groupcov", "wgtcov", "offcov")
  for (i in 1:length(vars)) {
    v    <- vars[i]
    obj  <- modeldata[[v, exact=TRUE]]
    if (length(obj)) {
      tmp      <- !(obj %in% metabs)
      obj      <- obj[tmp]
      ret[[v]] <- obj
    }  
  }
  ret
}

isBinaryVar <- function(data, v, exclude.miss=1) {

  ret <- FALSE
  vec <- data[, v, drop=TRUE]
  if (exclude.miss) vec <- vec[is.finite(vec)]
  if (!length(ret)) return(FALSE)
  if (all(vec %in% 0:1)) ret <- TRUE  

  ret
}
