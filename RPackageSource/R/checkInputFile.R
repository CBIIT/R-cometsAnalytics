readExcelSheet <- function(f, sheet, f.sheets, stopOnError=0, optional=0) {

  ret <- NULL

  # make sheet names not case-sensitive
  tmp <- toupper(f.sheets) %in% toupper(sheet)
  sh2 <- f.sheets[tmp]
  m   <- sum(tmp)  

  if (!m) {
    if (optional) return(NULL)
    msg <- paste0("ERROR: the input Excel file is missing the ", sheet, " sheet.\n")
    if (stopOnError) stop(msg)
    cat(msg)
    return(NULL)
  }

  # This should not be possible
  if (m > 1) {
    msg <- paste0("ERROR: multiple sheets in the input Excel file map to the ", sheet, " sheet.\n")
    if (stopOnError) stop(msg)
    cat(msg)
    return(NULL)
  }

  ret <- try(readxl::read_excel(f, sh2), silent=TRUE)
  if ("try-error" %in% class(ret)) {
    msg <- paste0("ERROR attempting to read the ", sheet, " sheet.\n")
    if (stopOnError) stop(msg)
    cat(msg)
    return(NULL)
  } else {
    cat(paste0(sheet, " sheet is read in.\n"))
  }
  
  compbl <- FALSE
  if (sheet %in% c(getModelsSheetName(), getOptionsSheetName())) compbl <- TRUE
  if (length(ret)) {
    if (nrow(ret)) {
      # column names are lower case after fixData is called  
      ret <- suppressWarnings(fixData(ret, compbl=compbl))
    } else {
      if (optional) return(NULL)
      msg <- paste0("ERROR the ", sheet, " sheet has no rows.\n")
      if (stopOnError) stop(msg)
      cat(msg)
    }
  } else {
    if (optional) return(NULL)
    msg <- paste0("ERROR the ", sheet, " sheet is empty.\n")
    if (stopOnError) stop(msg)
    cat(msg)
  }

  # Check column names for duplicate names. Currently, this seems impossible to know for sure, 
  #   because the readxl::read_excel function will rename the columns to something suffixed 
  #   with "..." and print a message without returning that info.
  if (length(ret)) {
    cx  <- colnames(ret)
    if (length(cx)) {
      tmp <- grepl("...", cx, fixed=TRUE) & (substr(cx, 1, 3) != "...")
      if (any(tmp)) {
        cx   <- cx[tmp]
        n    <- length(cx)
        vars <- rep("", n)
        for (i in 1:n) vars[i] <- unlist(strsplit(cx[i], "...", fixed=TRUE))[1]
        vars <- toupper(unique(vars))
        vars <- infile.collapseVec(vars) 
        msg  <- paste0("POSSIBLE ERROR: the ", sheet, " sheet may contain duplicated column(s) ", vars, ".\n")
        cat(msg)
      }
    }
  }

  ret

} # END: readExcelSheet

infile.normIdCol <- function(x, vmap, which="metab") {

  if (!length(x) || !length(vmap)) return(x)
  if (!nrow(x)) return(x)
  
  # Get the id column
  tmp <- infile.checkVarMapSheet(vmap, only.ids=1)
  if (which == "metab") {
    idv <- tmp[["metab.idvar", exact=TRUE]]
  } else {
    idv <- tmp[["sub.idvar", exact=TRUE]]
  }
  if (!length(idv)) return(x)

  idv <- tolower(idv)
  cx  <- tolower(colnames(x))
  tmp <- cx %in% idv
  if (any(tmp)) {
    col <- (1:length(cx))[tmp]
    for (v in col) x[, v] <- trimws(tolower(x[, v]))
  }
  x

} # END: infile.normIdCol

infile.catVarsToFactors <- function(dta.sdata, dta.vmap) {

  if (!length(dta.sdata) || !nrow(dta.sdata)) return(dta.sdata)
  if (!length(dta.vmap) || !nrow(dta.vmap)) return(dta.sdata)
  cx <- colnames(dta.vmap)
  
  v1  <- tolower(getVarMapVarTypeCol())
  v2  <- tolower(getVarMapVarRefCol())
  v3  <- tolower(getVarMapCohortVarCol())
  mid <- tolower(getVarRef_metabId())
  sid <- tolower(getVarRef_subjectId())
  if (!(v1 %in% cx)) return(dta.sdata)
  if (!(v2 %in% cx)) return(dta.sdata)

  tmp       <- (dta.vmap[, v1, drop=TRUE] %in% getVarMapVarTypeCat()) & !(dta.vmap[, v2, drop=TRUE] %in% c(sid, mid))
  myfactors <- dta.vmap[, v3, drop=TRUE][which(tmp)]
  myfactors <- myfactors[myfactors %in% colnames(dta.sdata)]
  m         <- length(myfactors)
  cat(paste0("There are ", m, " categorical variables.\n"))
  if (m) {
    for (v in myfactors) dta.sdata[,v] <- factor(dta.sdata[,v, drop=TRUE])
  }  

  dta.sdata

} # END: infile.catVarsToFactors

checkVarMapCols <- function(x) {

  if (length(x)) {
    nm   <- getVarMapSheetName()
    cols <- tolower(c(getVarMapVarRefCol(), getVarMapCohortVarCol(), getVarMapVarTypeCol()))
    tmp  <- cols %in% colnames(x)
    cols <- cols[tmp]
    if (length(cols)) {
      for (col in cols) {
        x[, col] <- trimws(tolower(x[, col, drop=TRUE]))
        tmp      <- is.na(x[, col, drop=TRUE])
        if (any(tmp)) x[tmp, col] <- ""
      }
    }
  }

  x

} # END: checkVarMapCols

checkModelsCols <- function(x) {

  if (length(x)) {
    nm   <- getModelsSheetName()
    cx   <- colnames(x)
    cols <- tolower(c(getModelsOutcomeCol(), getModelsExposureCol()))
    for (col in cols) {
      if (col %in% cx) {
        str      <- paste0(nm, " sheet, ", toupper(col), " column")
        x[, col] <- checkVariableNames(x[, col, drop=TRUE], str, stopOnMissError=0) 
      }
    }
    cols <- tolower(c(getModelsAdjCol(), getModelsStratCol(), getModelsTimeCol()))
    for (col in cols) {
      if (col %in% cx) {
        str      <- paste0(nm, " sheet, ", toupper(col), " column")
        x[, col] <- checkVariableNames(x[, col, drop=TRUE], str, convertMissTo=NA, stopOnMissError=0) 
      }
    }
    col <- tolower(getModelsWhereCol())
    if (col %in% cx) x[, col] <- normalizeWhere(x[, col, drop=TRUE]) 
  }
    
  x

} # END: checkModelsCols

checkModelOptionsCols <- function(x) {

  if (length(x)) {
    nm   <- getOptionsSheetName()
    cols <- tolower(getReqModOpSheetCols())
    tmp  <- cols %in% colnames(x)
    cols <- cols[tmp]
    if (length(cols)) {
      idcol <- getModelOptionsIdCol()

      for (col in cols) {
        # Replace missing values with empty string
        tmp <- is.na(x[, col, drop=TRUE])
        if (any(tmp)) x[tmp, col] <- ""

        # Remove quotation marks
        if (col != idcol) x[, col] <- setupOpStr(x[, col, drop=TRUE])
      }
    }
  }

  x

} # END: checkModelOptionCols

infile.collapseVec <- function(vec, sep=", ", begin="(", end=")", removeMiss=1) {

  if (!length(vec)) return("")
  if (removeMiss) {
    tmp <- !is.na(vec) & (nchar(vec) > 0)
    vec <- vec[tmp]
  }
  if (!length(vec)) return("")

  ret <- paste(vec, collapse=sep, sep="")
  ret <- paste0(begin, ret, end)
  ret

} # END: infile.collapseVec

infile.getAllVarsFromModels <- function(Models) {

  all  <- NULL
  if (!length(Models)) return(all)

  vars    <- tolower(c(getModelsOutcomeCol(), getModelsExposureCol(), 
                       getModelsAdjCol(), getModelsStratCol(), getModelsTimeCol()))
  str     <- "all metabolites"
  allFlag <- 0

  for (v in vars) {
    vec <- infile.getColumn(Models, v)
    if (length(vec)) {
      vec <- tolower(vec)
      if (any(grepl(str, vec, fixed=TRUE))) return("*")
      vec <- unique(trimws(unlist(strsplit(vec, " ", fixed=TRUE))))
      vec[is.na(vec)] <- ""
      tmp <- nchar(vec) > 0
      vec <- vec[tmp]
      tmp <- !(vec %in% all)
      if (any(tmp)) all <- c(all, vec[tmp]) 
    }
  }

  # Get vars in WHERE conditions
  vec <- infile.getColumn(Models, getModelsWhereCol())
  if (length(vec)) {
    vec <- tolower(vec)
    if (any(grepl(str, vec, fixed=TRUE))) return("*")
    vec <- unique(trimws(unlist(strsplit(vec, getStrataSep(), fixed=TRUE))))
    vec[is.na(vec)] <- ""
    vec <- getVarsFromWhereVec(vec)
    tmp <- nchar(vec) > 0
    vec <- vec[tmp]
    tmp <- !(vec %in% all)
    if (any(tmp)) all <- c(all, vec[tmp]) 
  }
  
  all

} # END: infile.getAllVarsFromModels

infile.getAllVarsFromModelOptions <- function(ModelOptions) {

  ret  <- NULL
  if (!length(ModelOptions)) return(ret)

  nameVec <- infile.getColumn(ModelOptions, getOptionNameCol())
  valVec  <- infile.getColumn(ModelOptions, getOptionValueCol())
  if (length(nameVec) && length(valVec)) {
    nameVec <- trimws(nameVec)
    tmp     <- nameVec %in% runModel.getOptionsThatAreVars()
    if (any(tmp)) {
      ret <- unique(trimws(tolower(valVec[tmp])))
    }
  }

  ret

} # END: infile.getAllVarsFromModelOptions

infile.getModelVars <- function(Models, ModelOptions, inc=NULL) {

  # Models sheet is now optional
  if (!length(Models)) return(inc)

  all <- 0
  ret <- infile.getAllVarsFromModels(Models)
  len <- length(ret)
  if ((len == 1) && (ret == "*")) all <- 1
  if (!all) {
    ret2 <- infile.getAllVarsFromModelOptions(ModelOptions)
    len2 <- length(ret2)
    if ((len2 == 1) && (ret2 == "*")) all <- 1
  } 
  if (!all) {
    tmp <- !(ret2 %in% ret)
    if (any(tmp)) ret <- c(ret, ret2[tmp])
    if (length(inc) && length(ret)) ret <- ret[ret %in% inc]
  } else {
    if (length(inc)) ret <- inc
  }
  if (!length(ret)) ret <- NULL
  
  ret

} # END: infile.getModelVars

infile.checkSheets <- function(dta.metab,dta.smetab, dta.sdata,dta.vmap,dta.models,dict_metabnames, dta.op) {
 
  # Columns have been renamed at this point
 
  err     <- 0
  allcols <- trimws(c(colnames(dta.sdata), dict_metabnames))

  # Check VarMap sheet first
  tmp      <- infile.checkVarMapSheet(dta.vmap)
  err      <- err + tmp$err
  metab.id <- tmp[["metab.idvar", exact=TRUE]]
  sub.id   <- tmp[["sub.idvar", exact=TRUE]]

  # Remove sub id from dict_metabnames and get new id 
  if (length(dict_metabnames)) {
    tmp        <- !(dict_metabnames %in% tolower(sub.id))
    dict2      <- dict_metabnames[tmp]
    sub.id.new <- names(dict_metabnames)[!tmp]
  } else {
    dict2      <- dict_metabnames
    sub.id.new <- NULL
  }

  # Get the metabolite variables that are included in the models
  metabs   <- infile.getModelVars(dta.models, dta.op, inc=dict2) 
  err      <- err + infile.checkMetabolitesSheet(dta.metab, metab.id)
  err      <- err + infile.checkSubMetabSheet(dta.smetab, sub.id.new, sub.id, metabs=dict2) 
  err      <- err + infile.checkSubDataSheet(dta.sdata, sub.id, dta.vmap) 
  err      <- err + infile.checkModelsSheet(dta.models, allcols, dta.vmap, metabs, dta.op)
  err      <- err + infile.checkModelOptionsSheet(dta.op, dta.models)

  # Check subject ids
  err <- err + infile.checkSubIds(dta.smetab, sub.id.new, dta.sdata)

  err

} # END: infile.checkSheets

infile.basicSheetCheck <- function(data, reqCols, sheetName, min.ncol=2, reqCols.orig=NULL) {

  err <- 0
  n   <- length(reqCols.orig)
  if (n && (n != length(reqCols))) {
    if (n == 1) {
      # Missing id variable
      cat(paste0("ERROR in the ", sheetName, " sheet. The column(s) (", toupper(reqCols.orig), ") are missing.\n"))
      err <- 1
    } else {
      stop("INTERNAL CODING ERROR in infile.basicSheetCheck")
    } 
  }
  
  if (!length(data)) return(1)
  if (!n) reqCols.orig <- reqCols
  
  # column names are made lower case in readExcelSheet
  cols <- colnames(data) 
  
  if (length(reqCols)) {
    reqCols <- trimws(tolower(reqCols))
    tmp     <- !(reqCols %in% cols)
    if (any(tmp)) {
      msg <- infile.collapseVec(reqCols.orig[tmp])
      msg <- paste0("ERROR in the ", sheetName, " sheet. The column(s) ", toupper(msg), " are missing.\n")
      cat(msg)
      err <- 1
    }
  } 
  if (length(cols) < min.ncol) {
    cat(paste0("ERROR in the ", sheetName, " sheet. There must be at least ", min.ncol, " columns.\n"))
    err <- 1
  }
  
  if (!nrow(data)) err <- 1
  
  err

} # END: infile.basicSheetCheck

infile.getColumn <- function(x, col) {

  ret <- NULL
  if (!length(x)) return(ret)
  col <- infile.modifySetOfVars(col)
  if (!length(col)) return(ret)
  col <- tolower(col)
  if (col %in% colnames(x)) ret <- x[, col, drop=TRUE]
  if (!length(ret)) ret <- NULL
  
  ret

} # END: infile.getColumn

infile.getSubset1col <- function(x, col, values, lower=1, trim=1) {

  ret <- NULL
  if (!length(x)) return(ret)
  if (!nrow(x)) return(ret)
  if (!length(col)) return(ret)
  if (!length(values)) return(ret)
  col <- tolower(col)
  if (!(col %in% colnames(x))) return(ret)
  
  vec <- x[, col, drop=TRUE]
  if (lower) {
    vec    <- tolower(vec)
    values <- tolower(values)
  } 
  if (trim) {
    vec    <- trimws(vec)
    values <- trimws(values)
  }
  ret <- vec %in% values
  
  ret

} # END: infile.getSubset1col

infile.checkForValInCol <- function(col, val, sheetName, colName) {

  err <- 0
  if (!(val %in% col)) {
    err <- 1
    msg <- paste0("ERROR in the ", sheetName, " sheet. A row with ", 
            toupper(colName), "=", val, " was not found.\n")
    cat(msg)
  }

  err

} # END: infile.checkForValInCol

infile.checkColForMiss <- function(colvec, sheetName, colName, error=1) {

  err    <- 0
  n      <- length(colvec)
  if (!n) return(err)

  colvec <- infile.normVarNames(colvec, miss="", toLower=0) 
  tmp    <- colvec %in% ""
  if (any(tmp)) {
    if (error) {
      str <- "ERROR"
      err <- 1
    } else {
      str <- "WARNING" 
    }
    rows <- (1:n)[tmp] + 1   
    rows <- infile.collapseVec(rows)
    msg  <- paste0(str, " on row(s) ", rows, " of the ", sheetName, " sheet. ",
                   toupper(colName), " contains missing values.\n")
    cat(msg)  
    if (!error) warning(msg)
  }

  err

} # END: infile.checkColForMiss

infile.checkColForReqVals <- function(colvec, reqVals, sheetName, colName) {

  err    <- 0
  n      <- length(colvec)
  if (!n) return(err)

  tmp    <- !(colvec %in% reqVals)
  if (any(tmp)) {
    err  <- 1
    rows <- (1:n)[tmp] + 1
    rows <- infile.collapseVec(rows)   
    vals <- infile.collapseVec(reqVals) 
    msg  <- paste0("ERROR on row(s) ", rows, " of the ", 
                   sheetName, " sheet. ", toupper(colName),
                   " must be one of ", vals, ".\n")
    cat(msg)  
  }

  err

} # END: infile.checkColForReqVals

infile.checkColForDups <- function(colvec, sheetName, colName, error=1) {

  err    <- 0
  n      <- length(colvec)
  if (!n) return(err)

  colvec <- infile.normVarNames(colvec, miss="", toLower=0) 
  tmp    <- duplicated(colvec)
  if (any(tmp)) {
    if (error) {
      str <- "ERROR"
      err <- 1
    } else {
      str <- "WARNING" 
    }
    dups <- infile.collapseVec(colvec[tmp])   
    msg  <- paste0(str, " in the ", sheetName, " sheet. ", 
                   toupper(colName), "=", dups, " are duplicated.\n")
    cat(msg)
    if (!error) warning(msg)  
  }

  err

} # END: infile.checkColForDups

infile.checkVarMapSheet <- function(x, only.ids=0) {

  # only.ids  A flag added to only return the id variables without perform the checks

  err  <- 0
  mvar <- NULL
  svar <- NULL
  nm   <- "VarMap"
  if (!only.ids) err  <- err + infile.basicSheetCheck(x, getReqVarMapSheetCols(), nm, min.ncol=3)

  # Check for required ids
  mid <- getVarRef_metabId()
  sid <- getVarRef_subjectId()

  if (length(x)) {
    vref  <- tolower(getVarMapVarRefCol())
    cvar  <- tolower(getVarMapCohortVarCol())
    flag1 <- 0
    flag2 <- 0
    if (vref %in% colnames(x)) {
      vvec  <- x[, vref, drop=TRUE]
      if (!only.ids) {
        err   <- err + infile.checkColForDups(vvec, nm, vref)
        err   <- err + infile.checkColForMiss(vvec, nm, vref)
        err   <- err + infile.checkForValInCol(vvec, mid, nm, vref)
        err   <- err + infile.checkForValInCol(vvec, sid, nm, vref)
      }
      flag1 <- 1
    }
    if (cvar %in% colnames(x)) {
      cvec  <- x[, cvar, drop=TRUE]
      if (!only.ids) {
        err   <- err + infile.checkColForDups(cvec, nm, cvar)
        err   <- err + infile.checkColForMiss(cvec, nm, cvar)
      }
      flag2 <- 1
    }

    # Get the id variables in the data
    tmp0 <- rep(FALSE, nrow(x))
    if (flag1 && flag2) {
      tmp  <- vvec %in% mid
      if (sum(tmp) == 1) {
        mvar <- cvec[tmp]
      } else {
        err <- 1
        # Message printed from above code
      }
      tmp0 <- tmp0 | tmp 
      tmp  <- vvec %in% sid
      if (sum(tmp) == 1) {
        svar <- cvec[tmp]
      } else {
        err <- 1
        # Message printed from above code
      }
      tmp0 <- tmp0 | tmp
    }

    # Check the vartype column, ignore the vartype for the id variables
    if (!only.ids) {
      var <- tolower(getVarMapVarTypeCol())
      if (var %in% colnames(x)) {
        req <- getVarMapVarTypeVals()
        vec <- infile.normVarNames(x[, var, drop=TRUE])
        if (any(tmp0)) vec[tmp0] <- req[1]  # ignore the ids, set to one of req
        err <- err + infile.checkColForReqVals(vec, req, nm, var) 
      }
    }
  }

  list(err=err, metab.idvar=mvar, sub.idvar=svar)
 
} # END: infile.checkVarMapSheet

infile.checkMetabolitesSheet <- function(x, idvar) {

  nm  <- getMetabSheetName()
  req <- c(idvar, getReqMetabSheetCols())
  err <- infile.basicSheetCheck(x, req, nm)
  
  # Check for duplicated ids, missing values. Warning if there are duplicates, missing values
  if (length(x) && nrow(x)) {
    cx <- colnames(x)
    for (v in req) {
      if (length(v) && (v %in% cx)) {
        err <- err + infile.checkColForDups(x[, v, drop=TRUE], nm, v, error=0)
        err <- err + infile.checkColForMiss(x[, v, drop=TRUE], nm, v, error=0)
      }
    }
  }
  
  err
 
} # END: infile.checkMetabolitesSheet

infile.checkNumericVars <- function(x, vars, sheetName, maxN.error=5, vars.orig=NULL) {

  nerr    <- 0
  nvars   <- length(vars)
  if (!nvars) return(nerr)
  if (!length(x) || !nrow(x)) return(nerr)

  norig   <- length(vars.orig)
  if (norig && (norig != nvars)) stop("INTERNAL CODING ERROR")
  if (!length(vars.orig)) vars.orig <- vars
  
  vars      <- tolower(vars)
  tmp       <- vars %in% colnames(x)
  vars      <- vars[tmp]
  vars.orig <- vars.orig[tmp]
  nvars     <- length(vars)
  if (!nvars) return(nerr)
  
  N       <- maxN.error  
  err     <- rep(FALSE, nvars)
  err2    <- rep(FALSE, nvars)
  invalid <- NULL

  for (i in 1:nvars) {
    vec <- x[, vars[i], drop=TRUE]
    if (!is.numeric(vec) && !all(is.na(vec))) {    
      vec2   <- as.numeric(vec)
      tmp    <- is.na(vec2)
      vec2   <- unique(vec[tmp])
      tmp    <- !is.na(vec2)
      vec2   <- vec2[tmp]
      if (length(vec2)) {
        err[i] <- TRUE
        nerr   <- nerr + 1
        vec2   <- vec2[!(vec2 %in% invalid)] 
        if (length(vec2)) invalid <- c(invalid, vec2)   
      } else {
        err2[i] <- TRUE
      }
      if (nerr > N) break
    }
  }

  if (nerr) {
    vars <- vars.orig[err]
    n    <- min(N, length(vars))
    vars <- vars[1:n]
    str1 <- ""
    str2 <- ""
    cstr <- getVarMapVarTypeCont()
    if (nerr > N) str1 <- ",..."
    if (length(invalid) > N) str2 <- ",..."
    cvars  <- infile.collapseVec(c(vars, str1)) 
    c2     <- infile.collapseVec(c(invalid, str2))
    msg    <- paste0("ERROR in the ", sheetName, " sheet. The ", cstr,  " column(s) ", 
                     cvars, " contain invalid values ", c2, ".\n")
    cat(msg) 
  }

  nerr2 <- sum(err2)
  if (nerr2) {
    vars <- vars.orig[err2]
    n    <- min(N, length(vars))
    vars <- vars[1:n]
    str1 <- ""
    cstr <- getVarMapVarTypeCont()
    if (nerr2 > N) str1 <- ",..."
    cvars  <- infile.collapseVec(c(vars, str1)) 
    msg    <- paste0("ERROR in the ", sheetName, " sheet. The ", cstr,  " column(s) ", 
                     cvars, " are not numeric.\n")
    cat(msg) 
  }


  nerr

} # END: infile.checkNumericVars

infile.checkSubMetabSheet <- function(x, idvar, idvar.orig, metabs=NULL) {

  nm  <- getSubMetabSheetName()
  req <- idvar
  ret <- infile.basicSheetCheck(x, req, nm, min.ncol=2, reqCols.orig=idvar.orig)
  N   <- 5
  
  if (!length(x)) return(ret)
  if (length(metabs)) {
    # Metabolites have been renamed
    vars      <- names(metabs) 
    tmp       <- vars %in% colnames(x)
    vars      <- vars[tmp]
    vars.orig <- metabs[tmp]
    nmetabs   <- length(vars) 
    if (nmetabs) {
      ret <- ret + infile.checkNumericVars(x, vars, nm, maxN.error=5, vars.orig=vars.orig)
    }
  }

  # Check for duplicated ids
  if (length(x) && length(idvar) && (idvar %in% colnames(x))) {
    ret <- ret + infile.checkColForDups(x[, idvar, drop=TRUE], nm, idvar, error=0)
  }

  # Check that the metabolites are in the Metabolites sheet

  ret
 
} # END: infile.checkSubMetabSheet

infile.checkSubDataSheet <- function(x, idvar, varmap) {

  nm  <- getSubDataSheetName()
  req <- getVarRef_subjectId()
  ret <- infile.basicSheetCheck(x, req, nm, min.ncol=1, reqCols.orig=idvar)
  
  # Check for duplicated ids
  if (length(x) && length(req) && (req %in% colnames(x))) {
    ret <- ret + infile.checkColForDups(x[, req, drop=TRUE], nm, idvar, error=0)
  }

  # Check that continuous vars are numeric
  tmp <- infile.getSubset1col(varmap, getVarMapVarTypeCol(), getVarMapVarTypeCont(), lower=1, trim=1)
  if (length(tmp) && sum(tmp)) {
    vars <- trimws(tolower(varmap[tmp, tolower(getVarMapVarRefCol()), drop=TRUE]))
    orig <- trimws(tolower(varmap[tmp, tolower(getVarMapCohortVarCol()), drop=TRUE]))
    ret  <- ret + infile.checkNumericVars(x, vars, nm, maxN.error=5, vars.orig=orig)
  }

  ret
 
} # END: infile.checkSubDataSheet

infile.checkWhere0 <- function(wstr, wvars, row, sheetName, colName) {

  err   <- 0
  lenw  <- nchar(wstr)
  if (is.na(lenw)) lenw <- 0
  if (!lenw) return(err)

  if (length(wvars)) {
    tmp <- !(wvars %in% "") & !is.na(wvars)
    wvars <- wvars[tmp]  
    if (lenw && !length(wvars)) {
      err <- 1
      msg <- paste0("ERROR on row ", row, " of the ", sheetName, " sheet. ", toupper(colName), 
                    "=(", wstr, ") is not valid.\n")
      cat(msg)
    }
  }

  err

} # END: infile.checkWhere0

infile.checkModelsSheet <- function(x, allcols, VarMap, metabs, ModelOptions) {

  # Models sheet is now optional. There may be a time variable.
  err <- 0
  if (!length(x)) return(err)
  nr  <- nrow(x)
  if (!nr) return(err)

  nm  <- getModelsSheetName()
  err <- err + infile.basicSheetCheck(x, getReqModelsSheetCols(), nm) 

  reserved  <- getGlobalOptionName()  
  cx        <- colnames(x)
  vref      <- infile.getColumn(VarMap, getVarMapVarRefCol()) 
  vrefFlag  <- length(vref)
  invalid   <- tolower(c(getVarRef_metabId(), getVarRef_subjectId()))
  modOpFlag <- length(ModelOptions)

  mv      <- tolower(getModelsModelCol())
  ov      <- tolower(getModelsOutcomeCol())
  ev      <- tolower(getModelsExposureCol())
  av      <- tolower(getModelsAdjCol())
  sv      <- tolower(getModelsStratCol())
  wv      <- tolower(getModelsWhereCol())
  iv      <- tolower(getModelOptionsIdCol())
  tv      <- tolower(getModelsTimeCol())
  mflag   <- mv %in% cx
  oflag   <- ov %in% cx
  eflag   <- ev %in% cx
  aflag   <- av %in% cx
  sflag   <- sv %in% cx
  wflag   <- wv %in% cx
  iflag   <- iv %in% cx
  tflag   <- tv %in% cx
  MV      <- toupper(mv)
  OV      <- toupper(ov)
  EV      <- toupper(ev)
  AV      <- toupper(av)
  SV      <- toupper(sv)
  WV      <- toupper(wv)
  IV      <- toupper(iv)
  TV      <- toupper(tv)

  allMetabs0 <- tolower(getAllMetabsName())
  allMetabs1 <- getAllMetabsNewName()

  if (mflag) mvec <- unlist(x[, mv, drop=TRUE])
  if (oflag) {
    ovec <- tolower(unlist(x[, ov, drop=TRUE]))
    ovec <- gsub(allMetabs0, allMetabs1, ovec, fixed=TRUE)
  }
  if (eflag) {
    evec <- tolower(unlist(x[, ev, drop=TRUE]))
    evec <- gsub(allMetabs0, allMetabs1, evec, fixed=TRUE)
  }

  if (aflag) avec <- tolower(unlist(x[, av, drop=TRUE]))
  if (sflag) svec <- tolower(unlist(x[, sv, drop=TRUE]))
  if (wflag) wvec <- tolower(unlist(x[, wv, drop=TRUE]))
  if (iflag) ivec <- trimws(unlist(x[, iv, drop=TRUE]))
  if (tflag) tvec <- trimws(unlist(x[, tv, drop=TRUE]))
  
  allcols <- trimws(tolower(allcols))
  osflag  <- oflag && sflag
  esflag  <- eflag && sflag
  asflag  <- aflag && sflag 
  otflag  <- oflag && tflag

  if (modOpFlag) {
    if (iv %in% colnames(ModelOptions)) {
      modOpsIds <- trimws(unlist(ModelOptions[, iv, drop=TRUE]))
    } else {
      modOpFlag <- 0
    }
  }
  modOpSheet <- getOptionsSheetName()

  # Check for duplicate model labels, missing values
  if (mflag) {
    err <- err + infile.checkColForDups(x[, mv, drop=TRUE], nm, mv, error=1)
  }

  for (i in 1:nr) {
    row <- i + 1

    if (oflag) ovars <- trimws(unlist(strsplit(ovec[i], " ", fixed=TRUE)))
    if (eflag) evars <- trimws(unlist(strsplit(evec[i], " ", fixed=TRUE)))
    if (aflag) avars <- trimws(unlist(strsplit(avec[i], " ", fixed=TRUE)))
    if (sflag) svars <- trimws(unlist(strsplit(svec[i], " ", fixed=TRUE)))
    if (tflag) tvars <- trimws(unlist(strsplit(tvec[i], " ", fixed=TRUE)))
    if (wflag) {
      wstr  <- wvec[i]
      wvars <- wstr
      if (grepl(",", wvars, fixed=TRUE)) wvars <- trimws(unlist(strsplit(wvars, ",", fixed=TRUE)))   
      tmp   <- !is.na(wvars)
      wvars <- wvars[tmp]
      wvars <- getVarsFromWhereVec(wvars)
      err   <- err + infile.checkWhere0(wstr, wvars, row, nm, WV)
      err   <- err + infile.searchForWhereOp(wstr, row, colName=WV, sheet=nm) 
    }
    if (iflag) ival <- ivec[i]

    # Check for missing values 
    if (mflag) err <- err + infile.checkForMissingValues(row, MV, mvec[i], sheet=nm)
    if (oflag) err <- err + infile.checkForMissingVarNames(row, OV, ovars, sheet=nm) 
    if (eflag) err <- err + infile.checkForMissingVarNames(row, EV, evars, sheet=nm)
    if (iflag) err <- err + infile.checkForMissingValues(row, IV, ival, sheet=nm)

    # Remove All metabolites
    if (oflag) ovars <- ovars[!(ovars %in% allMetabs1)]
    if (eflag) evars <- evars[!(evars %in% allMetabs1)]

    # Check that the variables exist in the data
    if (oflag) err <- err + infile.checkForVarsInData(row, OV, ovars, allcols, sheet=nm)
    if (eflag) err <- err + infile.checkForVarsInData(row, EV, evars, allcols, sheet=nm)
    if (aflag) err <- err + infile.checkForVarsInData(row, AV, avars, allcols, sheet=nm)
    if (sflag) err <- err + infile.checkForVarsInData(row, SV, svars, allcols, sheet=nm)
    if (wflag) err <- err + infile.checkForVarsInData(row, WV, wvars, allcols, sheet=nm)
    if (tflag) err <- err + infile.checkForVarsInData(row, TV, tvars, allcols, sheet=nm)

    # Check for overlapping variables
    if (osflag) err <- err + infile.checkForOverlappingVars(row, OV, SV, ovars, svars, sheet=nm)
    if (esflag) err <- err + infile.checkForOverlappingVars(row, EV, SV, evars, svars, sheet=nm)
    if (asflag) err <- err + infile.checkForOverlappingVars(row, AV, SV, avars, svars, sheet=nm)
    if (otflag) err <- err + infile.checkForOverlappingVars(row, OV, TV, ovars, tvars, sheet=nm)

    if (iflag) err <- err + infile.checkForReservedWords(row, IV, ival, reserved, sheet=nm)

    # Check that variables are listed in the VarMap sheet
    if (length(vrefFlag)) {
      if (oflag) err <- err + infile.checkVarsInVarRef(row, OV, ovars, vref, metabs, sheet=nm) 
      if (eflag) err <- err + infile.checkVarsInVarRef(row, EV, evars, vref, metabs, sheet=nm) 
      if (aflag) err <- err + infile.checkVarsInVarRef(row, AV, avars, vref, metabs, sheet=nm) 
      if (tflag) err <- err + infile.checkVarsInVarRef(row, TV, tvars, vref, metabs, sheet=nm) 
    }
 
    # Check that variables do not contain id variables
    if (oflag) err <- err + infile.checkForInvalidVars(row, OV, ovars, invalid, sheet=nm) 
    if (eflag) err <- err + infile.checkForInvalidVars(row, EV, evars, invalid, sheet=nm)
    if (aflag) err <- err + infile.checkForInvalidVars(row, AV, avars, invalid, sheet=nm) 
    if (sflag) err <- err + infile.checkForInvalidVars(row, SV, svars, invalid, sheet=nm)
    if (tflag) err <- err + infile.checkForInvalidVars(row, TV, tvars, invalid, sheet=nm)

    # Check that MODELSPEC column has a corresponding row in the model options sheet
    if (iflag && modOpFlag && !is.na(ival) && !(ival %in% modOpsIds)) {
      err <- err + 1
      msg <- paste0("ERROR on row ", row, " of the ", nm, " sheet. ", IV, "=", ival, 
                    " not found in the ", modOpSheet, " sheet.\n")
      cat(msg)
    } 
  }

  err

} # END: infile.checkModelsSheet

infile.checkSubIds <- function(subMetab, mIdVar, subData) {

  err    <- 0
  sIdVar <- getVarRef_subjectId()

  if (!length(subMetab)) return(err)
  if (!length(subData)) return(err)
  if (!length(mIdVar)) return(err)
  if (!(mIdVar %in% colnames(subMetab))) return(err)
  if (!(sIdVar %in% colnames(subData))) return(err)

  msheet      <- getSubMetabSheetName()
  dsheet      <- getSubDataSheetName()
  subMetabIds <- unlist(subMetab[, mIdVar, drop=TRUE])
  subDataIds  <- unlist(subData[, sIdVar, drop=TRUE])
  
  tmp <- subMetabIds %in% subDataIds
  if (!any(tmp)) {
    # No overlapping ids
    err <- 1
    msg <- paste0("ERROR: the ", msheet, " and ", dsheet, 
                  " sheets have no subject ids in common.\n")
    cat(msg)
    return(err)
  }

  m <- sum(!tmp)
  if (m) {
    msg <- paste0("WARNING: there were ", m, " subject ids from the ", msheet, " sheet",
                  " that were not found in the ", dsheet, " sheet.\n")
    cat(msg)
    warning(msg)
  }
  
  tmp <- !(subDataIds %in% subMetabIds)
  m   <- sum(tmp)
  if (m) {
    msg <- paste0("WARNING: there were ", m, " subject ids from the ", dsheet, " sheet",
                  " that were not found in the ", msheet, " sheet.\n")
    cat(msg)
    warning(msg)
  }
 
  err

} # END: infile.checkSubIds

infile.checkAllModels <- function(readData) {

  err <- 0
  if (!length(readData)) return(err)
  if (!is.list(readData)) return(err)
  mymodels <- readData[["mods", exact=TRUE]]
  if (!length(mymodels)) return(err)
  mymodels <- mymodels[["model", exact=TRUE]]
  nmodels  <- length(mymodels)
  if (!nmodels) return(err)
  sheet <- getModelsSheetName()

  cat(runModel.testModelString())
  opnm <- getMetabDataOpsName()
  for (i in 1:nmodels) {
    row       <- i + 1 
    modeldata <- try(getModelData(readData, modlabel=mymodels[i]), silent=TRUE)
    if ("try-error" %in% class(modeldata)) {
      err <- err + 1
      # Get and print error message
      msg <- getErrorMsgFromTryError(modeldata)
      msg <- paste0("ERROR for model on row ", row, " of the ", sheet, " sheet: \n", msg, "\n")
      cat(msg)
      next
    }

    op <- modeldata[[opnm, exact=TRUE]]
    if (!is.list(op)) op <- list()
    op$DONOTRUN <- 1 # This will only cause the beginning of runModel to execute
    op          <- try(runModel.checkOptions(op, modeldata), silent=TRUE)
    if ("try-error" %in% class(op)) {
      err <- err + 1
      # Get and print error message
      msg <- getErrorMsgFromTryError(op)
      msg <- paste0("ERROR for model on row ", row, " of the ", sheet, " sheet: \n", msg, "\n")
      cat(msg)
      next
    }
    op$cohort <- ""
    tmp       <- try(runModel.start(modeldata, readData, op), silent=TRUE)
    if ("try-error" %in% class(tmp)) {
      err <- err + 1
      # Get and print error message
      msg <- getErrorMsgFromTryError(tmp)
      msg <- paste0("ERROR for model on row ", row, " of the ", sheet, " sheet: \n", msg, "\n")
      cat(msg)
      next
    }
  }
  cat(runModel.testModelString2())

  err

} # END: infile.checkAllModels

infile.checkModelOptionsSheet <- function(x, Models) {

  # Model options sheet is optional
  err <- 0
  if (!length(x)) return(err)
  nr   <- nrow(x)
  if (!nr) return(err)
  if (!length(Models)) return(err)
  if (!nrow(Models)) return(err)
  if (!(getModelOptionsIdCol() %in% colnames(Models))) return(err)

  nm  <- getOptionsSheetName()
  err <- err + infile.basicSheetCheck(x, getReqModOpSheetCols(), nm) 

  err

} # END: infile.checkModelOptionsSheet

infile.modifySetOfVars <- function(vars) {

  if (!length(vars)) return(NULL)
  tmp  <- is.character(vars) & !is.na(vars)
  vars <- vars[tmp]
  if (length(vars)) {
    tmp  <- nchar(vars) > 0
    vars <- vars[tmp]
  }
  if (!length(vars)) vars <- NULL

  vars

} # END: infile.modifySetOfVars

infile.normVarNames <- function(vars, miss="", toLower=1) {

  if (!length(vars)) return(NULL)
  tmp  <- is.na(vars)
  if (any(tmp)) vars[tmp] <- miss
  vars <- trimws(vars)
  if (toLower) vars <- tolower(vars)

  vars

} # END: infile.normVarNames

infile.checkForReservedWords <- function(row, colName, word, reserved, sheet="Models") {

  err  <- 0
  if (word %in% reserved) {
    err <- 1
    msg <- paste0("ERROR on row ", row, " of the ", sheet, " sheet. ",
                  toupper(colName), "=", word, " is a reserved word.\n")  
    cat(msg)
  }

  err

} # END: infile.checkForReservedWords

infile.checkForInvalidVars <- function(row, colName, vec, invalid, sheet="Models") {

  err  <- 0
  tmp <- vec %in% invalid
  if (any(tmp)) {
    err  <- 1
    vec  <- vec[tmp]
    cvec <- infile.collapseVec(vec)  
    msg  <- paste0("ERROR on row ", row, " of the ", sheet, " sheet.",
                  toupper(colName), "=", cvec, " is invalid.\n")  
    cat(msg)
  }

  err

} # END: infile.checkForInvalidVars

infile.checkVarsInVarRef <- function(row, colName, vars, varRefVec, metabs, sheet="Models") {

  err  <- 0
  vars <- infile.modifySetOfVars(vars)
  if (!length(vars)) return(err)
  vars <- vars[!(vars %in% metabs)] 
  if (!length(vars)) return(err)

  tmp <- !(vars %in% varRefVec) 
  if (any(tmp)) {
    vv  <- infile.collapseVec(vars[tmp]) 
    err <- 1
    msg <- paste0("ERROR on row ", row, " of the ", sheet, " sheet. ",
                  toupper(colName), "=", vv, " are not listed in the ",
                  toupper(getVarMapVarRefCol()), " column of the ", getVarMapSheetName(), " sheet.\n")  
    cat(msg)
  }

  err

} # END: infile.checkVarsInVarRef

infile.checkForVarsInData <- function(row, colName, vars, allcols, sheet="Models", exc="_all_metabolites_") {

  err  <- 0
  vars <- infile.modifySetOfVars(vars)
  if (!length(vars)) return(err)
  vars <- vars[!(vars %in% exc)] 
  if (!length(vars)) return(err)

  tmp <- !(vars %in% allcols)
  if (any(tmp)) {
    err <- 1
    msg <- infile.collapseVec(vars[tmp]) 
    msg <- paste0("ERROR on row ", row, " of the ", sheet, " sheet. ",
                  toupper(colName), "=", msg,
                  " do not exist in the data! Check the naming!\n")  
    cat(msg)
  }

  err

} # END: infile.checkForVarsInData

infile.checkForMissingVarNames <- function(row, colName, vars, sheet="Models") {

  err  <- 0
  vars <- infile.modifySetOfVars(vars)
  if (!length(vars)) {
    err <- 1
    msg <- paste0("ERROR on row ", row, " of the ", sheet, " sheet.",
                  " The ", toupper(colName), " column must contain variable names.\n")  
    cat(msg)
  }

  err

} # END: infile.checkForMissingVarNames

infile.checkForMissingValues <- function(row, colName, values, sheet="Models") {

  err  <- 0
  if (any(is.na(values))) {
    err <- 1
    msg <- paste0("ERROR on row ", row, " of the ", sheet, " sheet.",
                  " The ", toupper(colName), " column cannot have missing values.\n")  
    cat(msg)
  }

  err

} # END: infile.checkForMissingValues

infile.checkForOverlappingVars <- function(row, colName1, colName2, vars1, vars2, sheet="Models", 
                                    exc="_all_metabolites_") {

  err   <- 0
  vars1 <- infile.modifySetOfVars(vars1)
  vars2 <- infile.modifySetOfVars(vars2)
  if (!length(vars1) || !length(vars2)) return(err)
  vv <- intersect(vars1, vars2)
  if (length(vv)) vv <- vv[!(vv %in% exc)]
  if (length(vv)) {
    err <- 1
    msg <- infile.collapseVec(vv) 
    msg <- paste0("ERROR on row ", row, " of the ", sheet, " sheet.",
                  " The variable(s) ", msg, " appear on both the ",
                  toupper(colName1), " and ", toupper(colName2), 
                  " columns. This is not allowed.\n")  
    cat(msg)
  }
  
  err

} # END: infile.checkForOverlappingVars

infile.searchForWhereOp <- function(str, row, colName="WHERE", sheet="Models") {

  ret <- 0
  if (!length(str) || is.na(str)) return(ret)
  if (grepl("=", str, fixed=TRUE)) return(ret)
  if (grepl("<", str, fixed=TRUE)) return(ret)
  if (grepl(">", str, fixed=TRUE)) return(ret)

  msg <- paste0("ERROR on row ", row, " of the ", sheet, " sheet.",
                  " The ", toupper(colName), " column does not contain", 
                  " an operator (<, >, =, >=, <=, !=)\n")  
  cat(msg)

  1

} # END: infile.searchForWhereOp

