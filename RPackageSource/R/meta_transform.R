#' A list of 20:
#' \itemize{
#' \item{\code{file}}{ The complete path to the file. Must be specified.}
#' \item{\code{cohort}}{ The name of the cohort. Use this option to set or change
#'          the name of the cohort.
#'          Must be specified for files not obtained from version 3.0 of RcometsAnalytics.}
#' \item{\code{sep}}{ The file delimiter. For example "," for csv files, "\\t" for
#'       tab-delimited files. The default is NULL.}
#' \item{\code{correlation}}{ 0 or 1 for correlation results. The default is 0, 
#'           so that the file is assumed to be from a non-correlation model.}
#' \item{\code{estimate.col}}{ Column name for the beta estimate or correlation. 
#'                         The default value is "estimate".}
#' \item{\code{se.col}}{ Column name for the standard error. Needed for non-correlation models.
#'                      The default value is "std.error".}
#' \item{\code{nobs.col}}{ Column name for the number of subjects. If such a column
#'        does not exist in the file, set \code{nobs} to a value.
#'                     The default is "nobs".}
#' \item{\code{nobs}}{ Number of observations. Must be specified if \code{nobs.col}
#'                     is not specified. The default is NULL.}
#' \item{\code{outcome.col}}{ Column name for the outcome. If such a column does not
#'                   exist in the file, then set \code{outcome.name}.
#'                            The default is "outcome_uid".}
#' \item{\code{outcome.name}}{ The name of the outcome variable. Must be specified if
#'                    \code{outcome.col} is not specified. No default.}
#' \item{\code{exposure.col}}{ Column name for the exposure. If such a column does not
#'                   exist in the file, then set \code{exposure.name}.
#'                            The default is "exposure_uid".}
#' \item{\code{exposure.name}}{ The name of the exposure variable. Must be specified if
#'                    \code{exposure.col} is not specified. No default.}
#' \item{\code{stratavar.col}}{ Column name for the stratification variable. 
#'                           The default is NULL.}
#' \item{\code{strata.col}}{ Column name for the stratification values. 
#'                           The default is NULL.}
#' \item{\code{strata.name}}{ The name of the stratification variable.
#'                  Use this option to change the name of the stratification variable or
#'                 to add a stratification variable to the file. When adding a
#'                 stratification variable, set \code{strata.value} also.
#'                 The default is NULL.}
#' \item{\code{strata.value}}{ The constant value of the stratification.
#'                 The default is NULL.}
#' \item{\code{model.col}}{ Column name for the model.  
#'                           The default is NULL.}
#' \item{\code{model.name}}{ The name of the model.
#'                  Use this option to change or add the model name to the results.
#'                 The default is NULL.}
#' \item{\code{change.col.values}}{ A list of sublists, where each sublist has
#'          elements "col", "old", and "new". Use this option to change the 
#'          values of a (exposure or stratification) column to new values.
#'      For example, list(list(col="smoke", old=c("never", "current", "former"),
#'                             new=c(0, 1, 2))) will change the values
#'         "never" to 0, "current" to 1, and "former" to 2 in the "smoke" column.
#'                 The default is NULL.}
#' \item{\code{where}}{Vector of strings with a variable name, 
#'        a comparison operator (e.g. "<", ">", "<=", ">=", "!=", "="), and a value.  
#'        The strings are combined by the logical AND (&) operator.
#'        For example, \code{where = c("study = A", "age > 50")} uses all subjects
#'        with age > 50 and in study A. This option would primarily be used if the file contained
#'        results from multiple cohorts and each cohort needed to be included
#'        in the meta-analysis. }
#' \item{\code{new.ref.value}}{ The new reference value for a categorical.
#'                exposure variable. This option will update the Effects data frame
#'                based on the new reference value. Note that the updated p-values
#'                will be based on a Wald test regardless of the original test.
#'                This option is applied before the option \code{change.col.values}.
#'                This option is only for results files from non-correlation
#'                models obtained using version 3.0 of RcometsAnalytics.
#'                 The default is NULL.}
#' }
#'
#' @name file.list
#' @title list to describe or transform a file for meta-analysis
#' @details For any type of file, the parameter \code{file} must be specified. For files not obtained
#'         from version 3.0 of RcometsAnalytics, other parameters may need to be specified.
#'         This list can be used with the functions \code{\link{runMeta}} and
#'         \code{\link{meta_transform}}.  
#' 
#' @examples 
#' # Suppose \code{f} is a file containing results from a linear regression model with
#' #  the metabolites as the outcomes in column called "metabolite", and the exposure
#' #  was a variable called "age" which is not a column in \code{f}. Also, 572 subjects
#' # were included in the analysis and the cohort name is "study_XYZ".
#' f <- "path_to_file"
#' file.list <- list(file=f, sep=",", cohort="study_XYZ", outcome.col="metabolite",
#'                        exposure.name="age", nobs=572)
NULL




dfToComets <- function(df, infolist) {

  # important columns for meta-analysis outcome_uid, exposure_uid,
  #   estimate, stdError, nobs, strata, stratavar
  
  # Check for empty data frame
  if (!nonEmptyDf(df)) stop(msg_meta_25())

  # cohort name must be given
  cohort <- infolist[[dfToC_cohort(), exact=TRUE]]
  if (is.null(cohort)) {
    msg <- paste(msg_meta_26())
    stop(msg)
  }

  # Apply where first
  df <- dfToComets.where(df, infolist)

  # Change levels second
  df <- dfToComets.changeLevels(df, infolist)

  df <- dfToComets.outcome(df, infolist) 
  df <- dfToComets.exposure(df, infolist)
  df <- dfToComets.nobs(df, infolist) 
  df <- dfToComets.beta(df, infolist)
  df <- dfToComets.se(df, infolist)
  df <- dfToComets.strata(df, infolist) 
  df <- dfToComets.model(df, infolist) 
  
  # Check for necessary variables
  dfToComets.checkForVars(df, infolist) 

  # Create return list
  ret <- dfToComets.returnList(df, infolist) 

  ret
}

dfToComets.returnList <- function(df, infolist) {

  ret <- list()
  ret[[getModelSummaryName()]]          <- dfToComets.modelSumDF(df, infolist) 
  ret[[getEffectsName()]]               <- dfToComets.effectsDF(df, infolist) 
  ret[[runModel.getWarningsListName()]] <- dfToComets.warnDF(df, infolist) 
  ret[[getInfoTableDfName()]]           <- dfToComets.infoDF(df, infolist) 

  ret
}

dfToComets.checkForVars <- function(df, infolist) {

  ov   <- getModelSummaryOutUidCol()
  ev   <- getModelSummaryExpUidCol()
  nv   <- getModelSummaryNobsName()
  bv   <- getEffectsEstName()
  sev  <- getEffectsEstSeName()
  tv   <- getEffectsTermName()

  # Check that needed variables exist
  cx <- colnames(df)

  if (!(ov %in% cx)) {
    msg <- msg_meta_27(c(ov, dfToC_outcome.col(), dfToC_outcome.name()))
    stop(msg)
  }
  if (!(ev %in% cx)) {
    msg <- msg_meta_27(c(ev, dfToC_exposure.col(), dfToC_exposure.name()))
    stop(msg)
  }
  if (!(nv %in% cx)) {
    msg <- msg_meta_27(c(nv, dfToC_nobs.col(), dfToC_nobs()))
    stop(msg)
  }
  if (!(bv %in% cx)) {
    msg <- msg_meta_28(c(bv, dfToC_est.col()))
    stop(msg)
  }
  if (!infolist$corrFlag) {
    if (!(bv %in% cx)) {
      msg <- msg_meta_28(c(bv, dfToC_se.col()))
      stop(msg)
    } 
  }
  if (!(tv %in% cx)) {
    msg <- msg_meta_27(c(tv, dfToC_exposure.col(), dfToC_exposure.name()))
    stop(msg)
  }

  # Check that both strata vars exits if one exists
  svar  <- runModel.getStrataColName()
  sval  <- runModel.getStrataNumColName()
  flag1 <- svar %in% cx
  flag2 <- sval %in% cx
  if (flag1 && !flag2) {
    msg <- msg_meta_28(c(sval, dfToC_strata.col()))
    stop(msg)
  }
  if (!flag1 && flag2) {
    msg <- msg_meta_28(c(svar, dfToC_stratavar.col()))
    stop(msg)
  }


  NULL
}

dfToComets.setCols <- function(df) {

  num.cols  <- c(getModelSummaryNobsName(), getEffectsEstName(), getEffectsEstSeName())
  char.cols <- c(getModelSummaryOutUidCol(), getModelSummaryExpUidCol(), 
                 getEffectsTermName(), 
                 runModel.getStrataColName(), runModel.getStrataNumColName())

  cx        <- colnames(df)
  tmp       <- num.cols %in% cx
  num.cols  <- num.cols[tmp]
  tmp       <- char.cols %in% cx
  char.cols <- char.cols[tmp]
  if (length(num.cols)) {
    for (v in num.cols) df[, v] <- as.numeric(df[, v])
  }
  if (length(char.cols)) {
    for (v in char.cols) df[, v] <- trimws(df[, v])
  }
  df
}

dfToComets.effectsDF <- function(df, infolist) {

  ov   <- getModelSummaryOutUidCol()
  ev   <- getModelSummaryExpUidCol()
  rv   <- getEffectsRunName()
  svar <- runModel.getStrataColName()
  sval <- runModel.getStrataNumColName()
  bv   <- getEffectsEstName()
  sev  <- getEffectsEstSeName()
  tv   <- getEffectsTermName()
  flag <- infolist$corrFlag

  # Add run number
  df[, rv] <- 1:nrow(df)

  vars <- c(svar, sval, rv, ov, ev, tv, bv)
  if (!infolist$corrFlag) vars <- c(vars, sev)
  tmp  <- vars %in% colnames(df)
  vars <- vars[tmp]
  #if (length(vars) < 5) stop("INTERNAL CODING ERROR")
  ret <- df[, vars, drop=FALSE]
  ret <- as.data.frame(ret, stringsAsFactors=FALSE, check.names=FALSE)
  ret <- dfToComets.setCols(df) 
  ret
}


dfToComets.modelSumDF <- function(df, infolist) {

  mv   <- getModelSummaryModelCol()
  ov   <- getModelSummaryOutUidCol()
  ev   <- getModelSummaryExpUidCol()
  nv   <- getModelSummaryNobsName()
  rv   <- getEffectsRunName()
  svar <- runModel.getStrataColName()
  sval <- runModel.getStrataNumColName()

  # Add run number
  df[, rv] <- 1:nrow(df)

  vars <- c(rv, ov, ev, nv, svar, sval, mv)
  tmp  <- vars %in% colnames(df)
  vars <- vars[tmp]
  #if (length(vars) < 4) stop("INTERNAL CODING ERROR")
  ret <- df[, vars, drop=FALSE]
  ret <- as.data.frame(ret, stringsAsFactors=FALSE, check.names=FALSE)
  ret <- dfToComets.setCols(df)
  ret
}

dfToComets.warnDF <- function(df, infolist) {
  runModel.getEmptyErrorWarn()
}

dfToComets.infoDF <- function(df, infolist) {
 
  out <- unique(df[, getModelSummaryOutUidCol(), drop=TRUE])
  exp <- unique(df[, getModelSummaryExpUidCol(), drop=TRUE]) 
  if (length(out) > 1) out <- getInfoTable2plusVars()
  if (length(exp) > 1) exp <- getInfoTable2plusVars()
  strvar <- runModel.getStrataColName()
  strval <- runModel.getStrataNumColName()
  cx     <- colnames(df)
  str    <- ""
  # Allow more than one strata variable now
  if ((strvar %in% cx) && (strval %in% cx)) {
    str <- unique(df[, strvar, drop=TRUE])
    if (length(str) > 1) str <- "*"
  } 
  model <- ""
  if (infolist$corrFlag) model <- getCorrModelName()

  nms  <- c(getInfoTableCohortName(), getInfoTableFileNmName(),
            getInfoTableModelNmName(), getInfoTableOutcomeName(),
            getInfoTableExposureName(), getInfoTableStrataName(),
            getInfoTableModelFuncName())
  vals <- c(infolist[[dfToC_cohort(), exact=TRUE]], infolist[[dfToC_file(), exact=TRUE]],
            "", out,
            exp, str,
            model)
  if (length(nms) != length(vals)) stop("INTERNAL CODING ERROR 2")
  ret <- cbind(nms, vals)    
  colnames(ret) <- c(getInfoTableNameCol(), getInfoTableValueCol()) 
  ret <- as.data.frame(ret, stringsAsFactors=FALSE, check.names=FALSE)

  ret
}

dfToComets_checkCol <- function(cx, col) {

  if (!length(col)) return(NULL)
  if (!(col %in% cx)) {
    msg <- msg_meta_29(col)
    stop(msg)
  }
  NULL
}

dfToComets.se <- function(df, infolist) {

  var  <- infolist[[dfToC_se.col(), exact=TRUE]]
  if (!length(var)) return(df)
  dfToComets_checkCol(colnames(df), var) 
  ovar <- getEffectsEstSeName()
  if (var != ovar) df[, ovar] <- df[, var, drop=TRUE]

  df
}

dfToComets.beta <- function(df, infolist) {

  var  <- infolist[[dfToC_est.col(), exact=TRUE]]
  if (!length(var)) return(df)
  dfToComets_checkCol(colnames(df), var)
  ovar <- getEffectsEstName()
  if (var != ovar) df[, ovar] <- df[, var, drop=TRUE]

  df
}

dfToComets.nobs <- function(df, infolist) {

  var  <- infolist[[dfToC_nobs.col(), exact=TRUE]]
  dfToComets_checkCol(colnames(df), var)
  ovar <- getModelSummaryNobsName()
  nobs <- infolist[[dfToC_nobs(), exact=TRUE]]
  if (length(var)) {
    if (var != ovar) df[, ovar] <- df[, var, drop=TRUE]
  } else if (length(nobs)) {
    # nobs must be specified
    df[, ovar] <- nobs
  }
  df
}



dfToComets.outcome <- function(df, infolist) {

  var  <- infolist[[dfToC_outcome.col(), exact=TRUE]]
  dfToComets_checkCol(colnames(df), var)
  ovar <- getModelSummaryOutUidCol()
  if (length(var)) {
    if (var != ovar) df[, ovar] <- df[, var, drop=TRUE]
  } else {
    # outcome.name must be specified
    nm <- infolist[[dfToC_outcome.name(), exact=TRUE]]
    if (length(nm)) df[, ovar] <- nm
  }
  df
}

dfToComets.exposure <- function(df, infolist) {

  var  <- infolist[[dfToC_exposure.col(), exact=TRUE]]
  dfToComets_checkCol(colnames(df), var)
  ovar <- getModelSummaryExpUidCol()

  if (length(var)) {
    if (var != ovar) df[, ovar] <- df[, var, drop=TRUE]
  } else {
    # exposure.name must be specified
    nm <- infolist[[dfToC_exposure.name(), exact=TRUE]]
    if (length(nm)) df[, ovar] <- nm
  }

  # Add term col for now
  tv <- getEffectsTermName()
  if (!(tv %in% colnames(df))) df[, tv] <- df[, ovar, drop=TRUE]

  df
}

dfToComets.model <- function(df, infolist) {

  var  <- infolist[[dfToC_model.col(), exact=TRUE]]
  dfToComets_checkCol(colnames(df), var)
  ovar <- getModelSummaryModelCol()

  if (length(var)) {
    if (var != ovar) df[, ovar] <- df[, var, drop=TRUE]
  } else {
    # outcome.name must be specified
    nm <- infolist[[dfToC_outcome.name(), exact=TRUE]]
    if (length(nm)) df[, ovar] <- nm
  }
  # Normalize column
  if (ovar %in% colnames(df)) df[, ovar] <- meta_normModelStr(df[, ovar, drop=TRUE]) 

  df
}


dfToComets.changeLevels <- function(df, infolist, stopOnError=1) {

  lst <- infolist[[dfToC_change.col.values(), exact=TRUE]]
  if (!length(lst)) return(df)
  n <- length(lst)
  for (i in 1:n) {
    tmp <- lst[[i]]
    if (!is.list(tmp)) stop("INTERNAL CODING ERROR")
    var <- tmp[[dfToC_change.col(), exact=TRUE]]
    old <- tmp[[dfToC_change.old(), exact=TRUE]]
    new <- tmp[[dfToC_change.new(), exact=TRUE]]
    df  <- dfToComets.changeLevels.var(df, var, old, new, stopOnError=stopOnError)
  }
  df
}

dfToComets.changeLevels.var <- function(x, var, from, to, stopOnError=1) {

  if (!(var %in% colnames(x))) {
    if (stopOnError) stop(msg_meta_29(var))
    return(x)
  }
  tmp <- from != to
  tmp[is.na(tmp)] <- TRUE
  from <- from[tmp]
  to   <- to[tmp]
  if (!length(from)) {
    warning(msg_meta_41(var))
    return(x)
  }

  vec <- x[, var, drop=TRUE]
  ret <- vec
  for (i in 1:length(from)) {
    tmp <- vec %in% from[i]
    if (!any(tmp)) {
      #msg <- msg_meta_30(c(var, from[i]))
      #warning(msg)
    } else {
      ret[tmp] <- to[i]
    }
  }

  # Warning if variable contains values that were not changed
  vec[is.na(vec)] <- ""
  vec  <- trimws(vec)
  uvec <- unique(vec)
  tmp  <- (nchar(uvec) > 0) & !(uvec %in% from)
  uvec <- uvec[tmp]
  if (length(uvec)) {
    str1 <- getQuotedVarStr(uvec, sep=", ")
    str2 <- getQuotedVarStr(var, sep=", ")
    str3 <- dfToC_change.col.values()
    msg  <- msg_meta_42(c(str1, str2, str3))
    warning(msg)
  }


  x[, var] <- ret
  x
}

dfToComets.strata <- function(df, infolist) {

  new.svar <- runModel.getStrataColName()
  new.sval <- runModel.getStrataNumColName()
  old.svar <- infolist[[dfToC_stratavar.col(), exact=TRUE]]
  old.sval <- infolist[[dfToC_strata.col(), exact=TRUE]]
  sname    <- infolist[[dfToC_strata.name(), exact=TRUE]]
  constval <- infolist[[dfToC_strata.value(), exact=TRUE]]
  cx       <- colnames(df)

  if (length(old.svar) && (old.svar != new.svar)) {
    dfToComets_checkCol(cx, old.svar)
    df[, new.svar] <- df[, old.svar]
  }
  if (length(old.sval) && (old.sval != new.sval)) {
    dfToComets_checkCol(cx, old.sval)
    df[, new.sval] <- df[, old.sval]
  }
  if (length(constval)) df[, new.sval] <- constval  
  if (length(sname)) df[, new.svar]    <- sname  

  df
}

dfToComets.where <- function(df, infolist) {

  where.vec <- infolist[[dfToC_where(), exact=TRUE]]
  if (!length(where.vec)) return(df)

  df <- filterCOMETSinput(df, where=where.vec)
  df
}

dfToComets.isCorrModel <- function(data.obj, data.list) {

  if (data.list$is.comets) {
    info    <- getInfoTableDfName()
    modfunc <- getInfoTableValue(data.obj[[info, exact=TRUE]], getInfoTableModelFuncName(), ifNotFound=NULL, check.len=1) 
    if (is.null(modfunc)) stop("INTERNAL CODING ERROR")
    ret <- (modfunc == getCorrModelName()) 
  } else {
    ret <- data.list[[getCorrModelName(), exact=TRUE]]
    if (!length(ret)) ret <- FALSE 
    ret <- ret %in% 1
  }
  ret
}

dfToComets.loadAndCheck <- function(flist) {

  if (!is.list(flist)) {
    tmp <- list()
    tmp[[dfToC_file()]] <- flist
    flist <- tmp
  }
  obj <- loadFile(flist)
  if (!is.data.frame(obj) && !is.list(obj)) {
    stop(msg_meta_31())
  }

  # First determine if COMETS output
  flist$is.comets <- isCometsOutFile(obj)
  corrModel <- flist[[getCorrModelName(), exact=TRUE]]
  if (is.null(corrModel)) corrModel <- dfToComets.isCorrModel(obj, flist)
  flist$corrFlag <- corrModel
  if (flist$is.comets) {
    obj <- CometsToComets(obj, flist) 
  } else {
    # data frame
    obj <- dfToComets(obj, flist)
  }

  list(data=obj, info.list=flist)
}

remColFromChangeColList <- function(lst, rem.col) {

  # lst is the list dfToC_change.col.values()
  n   <- length(lst)
  if (!n) return(lst)
  col <- dfToC_change.col()
  rem <- NULL
  for (i in 1:n) {
    tmp <- lst[[i]]
    var <- tmp[[col,  exact=TRUE]]
    if (length(var) && (var == rem.col)) rem <- c(rem, i)
  }
  if (length(rem)) lst <- lst[-rem]
  lst
}

CometsToComets <- function(obj, infolist) {

  # For a comets file, the only things that should change are the levels,
  #  adding a stratification, changing cohort name

  ms    <- getModelSummaryName()
  es    <- getEffectsName()
  is    <- getInfoTableDfName()
  ts    <- getTable1DfName()
  ws    <- runModel.getWarningsListName()

  # First, apply where
  obj <- CometsToComets.where(obj, infolist)

  # Second, change the exposure reference if specified
  tmp <- meta_changeExpRef(obj, infolist)
  obj <- tmp$res.list
  ref <- tmp$ref.info 

  df.ms <- obj[[ms, exact=TRUE]]
  df.es <- obj[[es, exact=TRUE]]
  df.is <- obj[[is, exact=TRUE]]
  df.ts <- obj[[ts, exact=TRUE]]
  df.ws <- obj[[ws, exact=TRUE]]

  # Change levels 
  lst <- infolist[[dfToC_change.col.values(), exact=TRUE]]
  if (!is.null(lst)) {
    df.es <- dfToComets.changeLevels(df.es, infolist, stopOnError=0)
    
    # Check if the values for column "term" changed. If so, then the exposureref
    #   value needs to be updated. 
    df.is <- CometsToComets.infoExpRef(df.is, lst, ref)

    # For the ModelSummary df, term col does not need to be updated. This is done 
    #   to prevent warnings.
    lst2 <- remColFromChangeColList(lst, getEffectsTermName())
    if (length(lst2)) {
      tmp <- infolist
      tmp[[dfToC_change.col.values()]] <- lst2
      df.ms <- dfToComets.changeLevels(df.ms, infolist, stopOnError=0)
    }
  }

  # For strata columns
  df.ms <- dfToComets.strata(df.ms, infolist) 
  df.es <- dfToComets.strata(df.es, infolist) 
  df.ts <- dfToComets.strata(df.ts, infolist)
  if (length(df.ws)) df.ws <- dfToComets.strata(df.ws, infolist)

  stratval <- infolist[[dfToC_strata.value(), exact=TRUE]]
  if (length(stratval)) {
    df.is <- setInfoTableValue(df.is, getInfoTableStrataName(), stratval)
  }
  nm <- infolist[[dfToC_strata.name(), exact=TRUE]]
  if (length(nm)) {
    df.is <- setInfoTableValue(df.is, getInfoTableStrataName(), nm)
  }

  cohort <- infolist[[dfToC_cohort(), exact=TRUE]]
  if (length(cohort)) {
    df.is <- setInfoTableValue(df.is, getInfoTableCohortName(), cohort)
  }

  obj[[ms]] <- df.ms
  obj[[es]] <- df.es
  obj[[is]] <- df.is
  obj[[ts]] <- df.ts
  obj[[ws]] <- df.ws
 
  obj
}

CometsToComets.infoExpRef <- function(info.table, change.list, ref.info) {

  n <- length(change.list)
  if (!n) return(info.table)
  new.ref.term <- ref.info$new.ref.term
  expvar       <- ref.info$exposure.var
  termv        <- getEffectsTermName()
  colv         <- dfToC_change.col()
  oldv         <- dfToC_change.old()
  newv         <- dfToC_change.new()  
  for (i in 1:n) {
    tmp <- change.list[[i]]
    col <- tmp[[colv, exact=TRUE]]
    if (col == termv) {
      old <- tmp[[oldv, exact=TRUE]]
      new <- tmp[[newv, exact=TRUE]]
      tmp <- old %in% new.ref.term
      m   <- sum(tmp)
      if (m != 1) next
      newval     <- new[tmp] # expvar.newvlue, remove expvar
      newval     <- gsub(paste0(expvar, "."), "", newval, fixed=TRUE) 
      info.table <- setInfoTableValue(info.table, getInfoTableExpRefName(), newval)
      break
    }
  }

  info.table
}

CometsToComets.where <- function(reslist, infolist) {

  where.vec <- infolist[[dfToC_where(), exact=TRUE]]
  if (!length(where.vec)) return(reslist)

  # Subset Effects and ModelSummary
  df <- reslist[[getEffectsName(), exact=TRUE]]
  df <- filterCOMETSinput(df, where=where.vec)
  reslist[[getEffectsName()]] <- df

  df <- reslist[[getModelSummaryName(), exact=TRUE]]
  df <- filterCOMETSinput(df, where=where.vec)
  reslist[[getModelSummaryName()]] <- df

  reslist
}

#---------------------------------------------------------
#' Function for transforming a set of results data
#'
#' @param file.obj list of type \code{\link{file.list}}
#' @param out.file NULL or the name of an output file to save the results.
#'          The file extension must be ".xlsx" or ".rda".
#'
#' @return A list of objects with names \code{\link{ModelSummary}},
#'        \code{\link{Effects}}, \code{\link{Errors_Warnings}},
#'        \code{\link{Table1}}, \code{Info}. \cr
#'
#' @details This function will transform or update a file of model results
#'          so that the results can be used for a meta-analysis. 
#'          The types of changes that can be made are:\cr
#'          1. Column values can be modified. For example, if the smoking status
#'             values 'never', 'current', and 'former' need to be changed to 0, 1, 2.\cr
#'          2. A column with a constant value can be added. For example, a column
#'             with the number of subjects used in the analysis can be added.  
#'             Or for instance, for a file from an all-female study, a stratification
#'             column containing the value 'female' can be added. \cr
#'          3. The file can be subsetted if it contains results from multiple models
#'             or multiple cohorts.\cr
#'          4. The results can be updated to reflect a different reference value
#'             of a categorical exposure variable.
#'          5. The name of the cohort can be renamed.\cr
#'          6. The name of the model can be renamed.
#'
#' @export
meta_transform <- function(file.obj, out.file=NULL) {

  file.obj <- dfToComets.check_fobj.i(file.obj)
  if (length(out.file)) out.file <- check_out.file(out.file, valid.ext=getOutTypeOpVals())
  ret <- dfToComets.loadAndCheck(file.obj)
  ret <- ret$data
  if (length(out.file)) try(saveObjectByFileExt(ret, out.file))
  ret
}

meta_transformAndSave <- function(file.obj, op) {

  DEBUG <- op$DEBUG
  if (DEBUG) cat("Begin: meta_transformAndSave\n")
  tdir  <- op[["temp.dir", exact=TRUE]]
  if (is.null(tdir)) stop("INTERNAL CODING ERROR")

  stopOnError <- op[[metaOp_stopOnFileError(), exact=TRUE]]
  wnm         <- runModel.getWarningsListName()
  wobj        <- op[[wnm, exact=TRUE]]
  N           <- length(file.obj)
  ok          <- rep(FALSE, N)
  out         <- paste0(tdir, "transform", 1:N, ".rda")
  for (i in 1:N) {
    f    <- getFileNameFromObj(file.obj[[i]])
    if (DEBUG) cat(paste0("*** Loading file ", f, "\n"))  
    ret  <- try(meta_transform(file.obj[[i]]), silent=FALSE)
    wobj <- runmodel.checkForError(ret, warnStr="ERROR", objStr=f, rem.obj=wobj, msg=NULL)
    if ("try-error" %in% class(ret)) {
      msg <- msg_meta_37(f)
      if (stopOnError) {
        print(ret)
        stop(msg)
      }
      cat(msg) 
      if (DEBUG) print(ret) 
      next
    }
    save(ret, file=out[i])
    ok[i] <- TRUE
  }
  op[[wnm]] <- wobj
  if (DEBUG) cat("End: meta_transformAndSave\n")

  list(files=out[ok], op=op)
}

meta_changeExpRef <- function(reslist, file.list) {

  ret <- list(res.list=reslist, ref.info=NULL)

  new.ref <- file.list[[dfToC_newRef(), exact=TRUE]]
  if (!length(new.ref)) return(ret)
  tmp <- file.list[["is.comets", exact=TRUE]]
  if (!length(tmp)) stop("INTERNAL CODING ERROR 1")
  if (!tmp) stop("ERROR: exposure reference can only be changed for version 3.0")
  tmp <- file.list[["corrFlag", exact=TRUE]]
  if (!length(tmp)) stop("INTERNAL CODING ERROR 2")
  if (tmp) stop("ERROR: exposure reference can only be changed for non-correlation models")

  ecv          <- getModelSummaryCovStrCol()
  efv          <- getEffectsEstName()
  sev          <- getEffectsEstSeName()
  tv           <- getEffectsTermName()
  rv           <- getEffectsRunName()
  
  # Check the effects and ModelSummary
  x    <- reslist[[getEffectsName(), exact=TRUE]]
  cols <- c(rv, efv, sev, tv) 
  if (!nonEmptyDfHasCols(x, cols, allcols=1, ignoreCase=0)) {
    stop("ERROR: Effects data frame is missing columns")
  }
  y       <- reslist[[getModelSummaryName(), exact=TRUE]]
  cols <- c(rv, ecv) 
  if (!nonEmptyDfHasCols(y, cols, allcols=1, ignoreCase=0)) {
    stop("ERROR: ModelSummary data frame is missing columns")
  }
  x <- addColsToDF(x, rv, y, rv, ecv, init=1, DEBUG=0)
  rm(y); gc()

  # Data must be ordered by run col
  tmp     <- order(x[, rv, drop=TRUE])
  x       <- x[tmp, , drop=FALSE]

  # Get the old and new reference terms
  tmp          <- meta_changeRef.new.old.refs(x, new.ref) 
  old.ref.term <- tmp$old.ref.term
  new.ref.term <- tmp$new.ref.term
  old.ref      <- tmp$old.ref
  if (old.ref == new.ref) {
    warning(paste0("The new reference value ", new.ref, " is the same as the original reference.")) 
    return(ret)
  }
  save.ref <- tmp

  runcol  <- x[, rv, drop=TRUE]
  termcol <- x[, tv, drop=TRUE]
  estcol  <- x[, efv, drop=TRUE]
  secol   <- x[, sev, drop=TRUE]
  covstr  <- x[, ecv, drop=TRUE]

  rm(x); gc()

  tmp        <- meta_changeRef.refcol(runcol, termcol, estcol, secol, new.ref.term)
  estcol.ref <- tmp$est
  secol.ref  <- tmp$se
  covmat     <- meta_changeRef.parseCov(covstr, new.ref.term) 
  estcol     <- meta_changeRef.newEst(estcol, termcol, estcol.ref, new.ref.term)
  secol      <- meta_changeRef.newSE(secol, termcol, secol.ref, covmat, new.ref.term) 
  termcol    <- meta_changeRef.newTerm(termcol, new.ref.term, old.ref.term)

  rm(covmat, runcol, covstr, tmp); gc()
  x        <- reslist[[getEffectsName(), exact=TRUE]]
  x[, tv]  <- termcol
  x[, efv] <- estcol
  x[, sev] <- secol

  # Update effects table
  x <- meta_changeRef.updateEffects(x) 
  reslist[[getEffectsName()]] <- x

  # Update info table.
  x <- reslist[[getInfoTableDfName(), exact=TRUE]]
  x <- setInfoTableValue(x, getInfoTableExpRefName(), new.ref)
  reslist[[getInfoTableDfName()]] <- x

  # Update covariance string. Difficult to do in general, so for now set to "".
  x    <- reslist[[getModelSummaryName(), exact=TRUE]]
  #vec  <- x[, getModelSummaryCovStrCol(), drop=TRUE]
  #str1 <- paste0(old.ref, getModelSummaryCovStrSep2())
  #str2 <- paste0(new.ref, getModelSummaryCovStrSep2())
  #len  <- nchar(str1)
  #substr(vec, 1, len)              <- str2
  x[, getModelSummaryCovStrCol()]  <- ""
  reslist[[getModelSummaryName()]] <- x

  list(res.list=reslist, ref.info=save.ref)
}

meta_changeRef.updateEffects <- function(x) {

  cx   <- colnames(x)
  sv   <- getEffectsStatName()  
  pv   <- getEffectsPvalueName()
  bv   <- getEffectsEstName()
  sev  <- getEffectsEstSeName()
  lv   <- getEffectsLowerName()
  uv   <- getEffectsUpperName()
  ebv  <- getEffectsExpEstName()
  esev <- getEffectsExpEstSeName()
  elv  <- getEffectsExpLowerName()
  euv  <- getEffectsExpUpperName()

  # Use Wald test for now, perhaps change in the future for linear models
  beta <- x[, bv, drop=TRUE]
  se   <- x[, sev, drop=TRUE]
  stat <- beta/se
  if (sv %in% colnames(x)) x[, sv] <- stat
  if (pv %in% colnames(x)) x[, pv] <- 2*pnorm(abs(stat), lower.tail=FALSE)

  alpha <- getAddCiOpDefault()
  zval  <- qnorm((1-alpha)/2, lower.tail=FALSE)
  if (lv %in% cx) x[, lv] <- beta - zval*se
  if (uv %in% cx) x[, uv] <- beta + zval*se

  # For exponeniated estimates
  tmp <- expParms_deltaMethod(beta, se)
  if (ebv  %in% colnames(x)) x[, ebv]  <- tmp$exp.beta
  if (esev %in% colnames(x)) x[, esev] <- tmp$exp.beta.se
  if (elv  %in% colnames(x)) x[, elv]  <- exp(x[, lv, drop=TRUE])
  if (euv  %in% colnames(x)) x[, euv]  <- exp(x[, uv, drop=TRUE])

  x
}

meta_changeRef.new.old.refs <- function(x, new.ref) {
  # x data frame with expsoure, cov.str

  sep1         <- getModelSummaryCovStrSep1()
  sep2         <- getModelSummaryCovStrSep2()
  covv         <- getModelSummaryCovStrCol()
  expvar       <- x[1, getEffectsExposurespecName(), drop=TRUE]
  covstr       <- x[1, getModelSummaryCovStrCol(), drop=TRUE]
  vec          <- unlist(strsplit(covstr, sep2, fixed=TRUE))
  oldref       <- vec[1]
  new.ref.term <- paste0(expvar, ".", new.ref)
  old.ref.term <- paste0(expvar, ".", oldref)

  list(old.ref.term=old.ref.term, new.ref.term=new.ref.term, 
       old.ref=oldref, new.ref=new.ref, exposure.var=expvar)

}

meta_changeRef.newTerm <- function(termcol, new.ref.term, old.ref.term) {

  tmp <- termcol == new.ref.term
  tmp[is.na(tmp)] <- FALSE
  termcol[tmp]    <- old.ref.term
  termcol
}

meta_changeRef.newEst <- function(estcol, termcol, estcol.ref, new.ref.term) {

  # beta_i --> beta_i - beta_new.ref.term, i != new.ref
  # beta_new.ref --> -beta_new.ref
  ret <- estcol - estcol.ref
  tmp <- termcol %in% new.ref.term
  if (any(tmp)) ret[tmp] <- -estcol.ref[tmp]
  ret

}

meta_changeRef.newSE <- function(secol, termcol, secol.ref, covmat, new.ref.term) {

  # se.beta_i --> sqrt(var(beta_i) + var(beta_new.ref.term) -2cov(,)), i != new.ref
  # se.beta_new.ref --> se.beta_old.ref
  var.col <- secol*secol
  var.ref <- secol.ref*secol.ref
  uterms  <- trimws(unique(termcol))
  tmp     <- !(uterms %in% c("", new.ref.term, NA))
  uterms  <- uterms[tmp]
  nterms  <- length(uterms)
  if (!nterms) stop("INTERNAL CODING ERROR 1")

  covcols <- paste0(uterms, "_", new.ref.term)
  if (!all(covcols %in% colnames(covmat))) stop("INTERNAL CODING ERROR 2")
  ret <- rep(NA, length(secol))
  for (i in 1:nterms) {
    term <- uterms[i]
    tmp  <- termcol == term
    tmp[is.na(tmp)] <- FALSE
    se       <- sqrt(var.col + var.ref - 2*covmat[, covcols[i], drop=TRUE])
    ret[tmp] <- se[tmp]     
  }
  tmp             <- termcol == new.ref.term
  tmp[is.na(tmp)] <- FALSE
  ret[tmp]        <- secol[tmp]

  ret

}


meta_changeRef.parseCov <- function(covstr, new.ref.term) {

  # order of cov string is (1,2), (1,3), (2,3), (1,4), (2,4), (3,4), ....

  mat   <- parseDelimVec(covstr, getModelSummaryCovStrSep2(), ncol=3)
  tmp   <- unique(mat[, 1, drop=TRUE])
  if (length(tmp) != 1) stop("exposure reference values are not unique")
  mat   <- mat[, -1, drop=FALSE]
  parms <- unique(mat[, 1])
  if (length(parms) != 1) stop("exposure covariance parms are not unique")
  parms <- unlist(strsplit(parms, getModelSummaryCovStrSep1(), fixed=TRUE))
  np    <- length(parms)
  # Get column names
  N     <- np*(np - 1)/2
  cx    <- rep("", N)
  row   <- 1
  col   <- 2
  ii    <- 1

  while (1) {
    # Let the new ref term be second
    prow   <- parms[row]
    pcol   <- parms[col]
    if (prow == new.ref.term) {
      prow <- pcol
      pcol <- new.ref.term
    }
    cx[ii] <- paste0(prow, "_", pcol)
    ii     <- ii + 1 
    row    <- row + 1
    if (row >= col) {
      row <- 1
      col <- col + 1
    }
    if (col > np) break
  }
  if (ii != N + 1) stop("INTERNAL CODING ERROR")
  mat <- parseDelimVec(mat[, 2], getModelSummaryCovStrSep1(), ncol=N, numeric=TRUE)
  if (!length(mat)) stop("ERROR: parsing covariance string")
  colnames(mat) <- cx

  # Get the columns we need
  tmp   <- parms != new.ref.term
  parms <- parms[tmp]
  if (length(parms) != np - 1) stop("ERROR with new.ref.term")  
  cx  <- paste0(parms, "_", new.ref.term)
  mat <- mat[, cx, drop=FALSE]
  
  mat
}

meta_changeRef.refcol <- function(runcol, termcol, estcol, secol, new.ref.term) {

  # data must already be sorted by run col
  nr               <- length(runcol)
  ret.est          <- rep(NA, nr)
  ret.se           <- rep(NA, nr)
  trmEqRefRow      <- rep(0, nr)
  tmp              <- termcol == new.ref.term
  tmp[is.na(tmp)]  <- FALSE
  trmEqRefRow[tmp] <- (1:nr)[tmp]
  a                <- 1
  b                <- 0
  est.ref          <- NA
  se.ref           <- NA
  runcol0          <- runcol[1]

  for (i in 1:nr) {
    runcoli <- runcol[i] 
    row     <- trmEqRefRow[i]
    if (runcoli == runcol0) {
      b <- b + 1
      if (row) {
        est.ref <- estcol[i]
        se.ref  <- secol[i]
      }
    } else {
      vec          <- a:b
      ret.est[vec] <- est.ref
      ret.se[vec]  <- se.ref
      # reset
      a       <- b + 1
      b       <- a
      runcol0 <- runcoli   
      if (row) {
        est.ref <- estcol[i]
        se.ref  <- secol[i]
      } else {
        est.ref <- NA
        se.ref  <- NA
      }
    }
  }
  # Set last group
  if (a < b) {
    if (b != nr) stop("INTERNAL CODING ERROR 1")
    vec          <- a:b
    ret.est[vec] <- est.ref
    ret.se[vec]  <- se.ref
  } else {
    # final row is different run
    if (trmEqRefRow[nr]) {
      ret.est[nr] <- estcol[nr]
      ret.se[nr]  <- secol[nr]
    }
  } 

  list(est=ret.est, se=ret.se)
}
