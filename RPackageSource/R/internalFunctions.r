# ---------------------------------------------------------------------------
# fixData function ----------------------------------------------------------
# ---------------------------------------------------------------------------
#' Fixes input data (e.g. takes care of factors, and other data frame conversions)
#' @keywords internal
#' @param dta any data frame
#' @param compbl compress multiple blank spaces to single blank space for all character or factor variables in the dataset

fixData <- function(dta,compbl=FALSE) {
  dta<-as.data.frame(dta) # have to convert to dataframe from local dataframe from readxl
  # run through the data
  colnames(dta) <- tolower(trimws(colnames(dta)))

  # remove rows that have all NAs (EM)
  countnas=as.numeric(apply(data.frame(dta),1,function(x) length(which(is.na(x)))))
  if (length(which(countnas==ncol(dta)))>0) {
  	dta=dta[-c(which(countnas==ncol(dta))),]
  }

  cls <- sapply(dta, class)
  # do conversions for data types: integer to numeric and dates are identified by date in name

  if (length(which(cls == "integer")) > 0) {
    for (ind in which(cls == "integer"))
      dta[,ind] <- as.numeric(dta[,ind])
  }

  if (length(which(cls == "factor")) > 0) {
    for (ind in which(cls == "factor"))
      dta[,ind] <- trimws(as.character(dta[,ind])) # trim and convert to character
  }
  if (length(which(cls == "character")) > 0) {
    for (indc in names(dta)) {
      if(class(dta[, indc]) %in% c("factor", "character")){
        if (compbl==TRUE)
          dta[, indc] <- gsub("\\s+", " ",trimws(dta[, indc])) # compress duplicate blanks
        else
          dta[, indc] <- trimws(dta[, indc])
      }
    }

  }

  return(dta)
} # end fixData function

# ---------------------------------------------------------------------------
# checkIntegrity function ---------------------------------------------------
# ---------------------------------------------------------------------------
#' Checks integrity of sheets in the user input CSV file
#' @keywords internal
#' @param dta.metab dta.metab
#' @param dta.smetab dta.smetab
#' @param dta.sdata dta.sdata
#' @param dta.vmap dta.vmap
#' @param dta.models dta.models
#' @param dict_metabnames dict_metabnames
#' @param dta.op dta.op
checkIntegrity <- function (dta.metab,dta.smetab, dta.sdata,dta.vmap,dta.models,dict_metabnames, dta.op) {

  cat(msg_rci_7())
  err <- infile.checkSheets(dta.metab,dta.smetab, dta.sdata,dta.vmap,dta.models,dict_metabnames, dta.op) 
  if (err) stop(msg_rci_8())

  # Check accepted values
  checkAcceptedValues(dta.sdata, dta.vmap)

  # Check exposure reference 
  checkExposureRef(dta.vmap, dta.models)

  # rename subjid in dta.smetab sheet for merging later on
  cv             <- tolower(getVarMapCohortVarCol())
  refv           <- tolower(getVarMapVarRefCol())
  subjidNew      <- tolower(getVarRef_subjectId())
  subjid         <- tolower(dta.vmap[, cv, drop=TRUE][tolower(dta.vmap[, refv, drop=TRUE]) == subjidNew])
  subjid.smetab  <- names(dict_metabnames)[which(dict_metabnames==subjid)] 
  colnames(dta.smetab)[which(colnames(dta.smetab)==subjid.smetab)] <- subjidNew

  msg <- msg_rci_9()

  ret <- list(dta.smetab=dta.smetab, dta.metab=dta.metab, dta.sdata=dta.sdata, integritymessage=msg)

  ret

}

checkExposureRef <- function(dta.vmap, dta.models) {

  if (!length(dta.models)) return(NULL)
  # Get all categorical variables
  typev   <- tolower(getVarMapVarTypeCol())
  refv    <- tolower(getVarMapVarRefCol())
  tmp     <- tolower(dta.vmap[, typev, drop=TRUE]) %in% tolower(getVarMapVarTypeCat())
  catvars <- dta.vmap[tmp, refv, drop=TRUE]
  if (!length(catvars)) return(NULL)
  tmp      <- tolower(dta.vmap[, typev, drop=TRUE]) %in% tolower(getVarMapVarTypeCont())
  contvars <- dta.vmap[tmp, refv, drop=TRUE]

  # Get exposure variables
  expv               <- tolower(getModelsExposureCol())
  exprefv            <- tolower(getModelsExpRefCol())
  strs               <- tolower(c(getAllMetabsName(), getAllMetabsNewName()))
  dta.models[, expv] <- trimws(tolower(dta.models[, expv]))

  # Remove rows with "All metabolites"
  tmp                <- !(dta.models[, expv, drop=TRUE] %in% strs)
  dta.models         <- dta.models[tmp, , drop=FALSE]
  if (!nrow(dta.models)) return(NULL)

  # Remove any cont variable
  tmp        <- !(dta.models[, expv, drop=TRUE] %in% contvars)
  dta.models <- dta.models[tmp, , drop=FALSE]
  if (!nrow(dta.models)) return(NULL)

  # Determine if exp ref col is in the data
  expRefColFlag <- exprefv %in% colnames(dta.models)

  # Get accepted values for any remaining categorical variable
  sep     <- runModel.getVarSep()
  allvars <- unique(parseStr(dta.models[, expv, drop=TRUE], sep=sep))
  tmp     <- allvars %in% catvars
  allvars <- allvars[tmp]
  if (!length(allvars)) return(NULL)
  acc.list <- list()
  rows     <- match(allvars, dta.vmap[, refv, drop=TRUE])
  tmp      <- is.na(rows)
  if (any(tmp)) {
    str <- paste0(allvars[tmp], collapse=", ")
    msg <- c(str, toupper(refv))
    stop(msg_rci_10(msg))
  }
  accv <- tolower(getVarMapAccValsCol())
  if (accv %in% colnames(dta.vmap)) {
    vec <- dta.vmap[, accv, drop=TRUE]
    for (i in 1:length(allvars)) {
      v             <- allvars[i]
      row           <- rows[i]
      accvals       <- parseAccValues(vec[row], TRUE)
      acc.list[[v]] <- accvals
    }
  }
  accFlag <- length(acc.list)

  # Loop over remaining rows
  expvec    <- dta.models[, expv, drop=TRUE]
  exprefvec <- as.character(dta.models[, exprefv, drop=TRUE]) 
  modelsv   <- tolower(getModelsModelCol())
  for (i in 1:length(expvec)) {
    vars  <- parseStr(expvec[i], sep=sep) 
    nvars <- length(vars)
    if (!nvars) next
    tmp0  <- vars %in% catvars
    if (!any(tmp0)) next
    
    # Some of vars are categorical
    if (!expRefColFlag) {
      stop(msg_rci_11(toupper(exprefv)))
    }
    # Parse to get ref levels
    refvals <- parseStr(exprefvec[i], sep=sep) 

    # There should be 1 ref val for each variable
    if (length(refvals) != nvars) {
      msg <- c(toupper(exprefv), dta.models[i, modelsv, drop=TRUE], 
               toupper(expv), toupper(exprefv))
      stop(msg_rci_12(msg))
    }

    # Check that the ref levels are in the accepted values column of varmap sheet
    cvars <- vars[tmp0]
    refs  <- refvals[tmp0]
    for (j in 1:length(cvars)) {
      ref <- refs[j]
      v   <- cvars[j]
      if (accFlag) {
        if (!(ref %in% acc.list[[v, exact=TRUE]])) {
          msg <- c(toupper(exprefv), ref, v, dta.models[i, modelsv, drop=TRUE])
          stop(msg_rci_13(msg)) 
        }
      }
    }
  }
  NULL
}

checkAcceptedValues <- function(dta.sdata, dta.vmap) {

  # Check cont and categorical variables
  colnames(dta.sdata) <- tolower(colnames(dta.sdata))
  colnames(dta.vmap)  <- tolower(colnames(dta.vmap))
  accvalCol           <- tolower(getVarMapAccValsCol())
  if (!(accvalCol %in% colnames(dta.vmap))) return(NULL)

  vtypeCol            <- tolower(getVarMapVarTypeCol())   
  vrefCol             <- tolower(getVarMapVarRefCol())
  cont.str            <- tolower(getVarMapVarTypeCont())
  cat.str             <- tolower(getVarMapVarTypeCat())   
  types               <- tolower(dta.vmap[, vtypeCol, drop=TRUE])
  accvals             <- dta.vmap[, accvalCol, drop=TRUE]
  vars                <- tolower(dta.vmap[, vrefCol, drop=TRUE])
  tmp                 <- (types %in% c(cont.str, cat.str)) & (vars %in% colnames(dta.sdata))
  types               <- trimws(types[tmp])
  accvals             <- trimws(accvals[tmp])
  vars                <- trimws(vars[tmp])
  tmp                 <- is.na(accvals)
  if (any(tmp)) accvals[tmp] <- ""
  n                   <- length(types)
  if (!n) return(NULL)
  catflag <- types == cat.str
  for (i in 1:n) {

    # Special case of group variable for conditional logistic regression
    if (toupper(accvals[i]) %in% c("NA", "")) next

    obj <- parseAccValues(accvals[i], catflag[i])
    if (!length(obj)) {
      msg <- c(toupper(accvalCol), vars[i], toupper(getVarMapSheetName()))
      stop(msg_rci_14(msg))
    }
    checkAccValuesInData(dta.sdata, vars[i], obj, catflag[i]) 
  }

}

checkAccValuesInData <- function(sdata, var, obj, catflag) {

  vec <- sdata[, var, drop=TRUE]
  if (catflag) {
    vec <- trimws(vec)
    tmp <- !(vec %in% obj)
    if (any(tmp)) {
      vec <- unique(vec[tmp])
      str <- paste0(vec, collapse=", ")
      msg <- c(str, var, getQuotedVecStr(obj))
      stop(msg_rci_15(msg)) 
    }
  } else {
    # Continuous
    #list(min=vec[1], max=vec[2], include.min=inc1, include.max=inc2)
    vec <- as.numeric(vec)
    a   <- obj$min
    if (obj$include.min) {
      tmp <- vec >= a
    } else {
      tmp <- vec > a
    }
    flag1 <- contAccValuesVecAllOk(tmp)
    b     <- obj$max
    if (obj$include.max) {
      tmp <- vec <= b
    } else {
      tmp <- vec < b
    }
    flag2 <- contAccValuesVecAllOk(tmp)
    if (!flag1 || !flag2) {
      msg <- c(var, getQuotedVecStr(obj$obj))
      stop(msg_rci_16(msg)) 
    }
  }
  
  NULL
}

contAccValuesVecAllOk <- function(vec) {

  # Return TRUE if all good, FALSE otherwise
  tmp <- is.na(vec)
  if (any(tmp)) vec[tmp] <- TRUE
  all(vec)
 
}

parseAccValues <- function(valstr, cat.flag) {

  ret <- NULL
  if (!length(valstr)) return(ret)
  valstr <- trimws(valstr)
  nc     <- nchar(valstr)
  if (!nc) return(ret)
  sep    <- getVarMapAccValsSep()

  if (cat.flag) {
    # Categorical
    ret <- getVecFromStr(valstr, delimiter=sep)
    ret <- trimws(ret)
    tmp <- nchar(ret) > 0
    ret <- ret[tmp]
  } else {
    # Continuous
    c1 <- substr(valstr, 1, 1)
    c2 <- substr(valstr, nc, nc)
    if (!(c1 %in% c("(", "["))) return(ret)
    if (!(c2 %in% c(")", "]"))) return(ret)
    str <- tolower(valstr)
    for (x in c("(", ")", "[", "]")) str <- gsub(x, "", str, fixed=TRUE)
    vec <- getVecFromStr(str, delimiter=sep)
    vec <- trimws(vec)
    tmp <- nchar(vec) > 0
    vec <- vec[tmp]
    if (length(vec) != 2) return(ret)
    vec <- as.numeric(vec)
    tmp <- is.na(vec) | is.nan(vec)
    if (any(tmp)) return(ret)
    if (vec[1] >= vec[2]) return(ret)
    inc1 <- c1 == "["
    inc2 <- c2 == "]" 
    ret <- list(min=vec[1], max=vec[2], include.min=inc1, include.max=inc2, obj=valstr)
  }
  if (!length(ret)) ret <- NULL
  ret  

}

# ---------------------------------------------------------------------------
# Harmonize ---------------------------------------------------
# ---------------------------------------------------------------------------
#' Harmonizes metabolites by looking up metabolites names from user input and finding the corresponding COMETS harmonized name.
#' @keywords internal
#' @param dtalist results of reading a CSV data sheet (with read_excel)

Harmonize<-function(dtalist){
  mastermetid=metabolite_name=metlower=uid_01=cohorthmdb=foundhmdb=masterhmdb=NULL
  # Load processed UIDs file:
  dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
  masterfile <- file.path(dir, "compileduids.RData")
  load(masterfile)

  # rename metid to be the same as metabid
  colnames(mastermetid)[which(colnames(mastermetid)=="metid")]=dtalist$metabId

  # join by metabolite_id only keep those with a match
  harmlistg<-dplyr::inner_join(dtalist$metab,mastermetid,by=c(dtalist$metabId),suffix=c(".cohort",".comets"))

  # Loop through and try to join all the other columns (at each loop, combine matches and remove
  # non-unique entries
  for (i in setdiff(colnames(dtalist$metab),dtalist$metabId)) {
 	harmlistg<-rbind(harmlistg,
		dplyr::left_join(
			dplyr::anti_join(dtalist$metab,harmlistg,
        			by=c(dtalist$metabId)) %>%
		             dplyr::mutate(metlower=gsub("\\*$","",i)),
				mastermetid,by=c("metlower"=dtalist$metabId),suffix=c(".cohort",".comets")) %>%
		dplyr::select(-metlower)) #%>%
#		dplyr::mutate(multrows=grepl("#",uid_01),harmflag=!is.na(uid_01))
  }

  # join by metabolite_name only keep those with a match
#  harmlistc<-dplyr::left_join(dplyr::anti_join(dtalist$metab,mastermetid,
#        by=c(dtalist$metabId)) %>%
#          dplyr::mutate(metlower=gsub("\\*$","",tolower(metabolite_name))), # take out * in metabolite name
 #       mastermetid,by=c("metlower"=dtalist$metabId)) %>% dplyr::select(-metlower)

  # combine the 2 data frames
#  dtalist$metab<-rbind(harmlistg,harmlistc) %>%
#    dplyr::mutate(multrows=grepl("#",uid_01),harmflag=!is.na(uid_01))

# Reorder:
  myord <- as.numeric(lapply(dtalist$metab[,dtalist$metabId],function(x)
	which(harmlistg[,dtalist$metabId]==x)))
  finharmlistg <- harmlistg[myord,]

# routine for hmdb look-up for those without a match
  if (length(names(finharmlistg)[grepl("^hmdb",names(finharmlistg))])>=2){

    # first hmdb is from cohort metabolite metadata
    cohorthmdb <- names(finharmlistg)[grepl("^hmdb",names(finharmlistg))][1]

    # need to rename to hmdb_id so that it can be left_join match
    names(finharmlistg)<-gsub(cohorthmdb,"hmdb_id",names(finharmlistg))

    ###########################################################  
    # The following code fixes a bug in the code below it. 
    #   The select statement was throwing an error, 
    #   and the chemical_id column was sometimes numeric.
    ###########################################################

    # bring in the masterhmdb file to find further matches
    foundhmdb <- finharmlistg %>% filter(is.na(uid_01)) # only find match for unmatched metabolites
    foundhmdb <- foundhmdb[, 1:ncol(dtalist$metab), drop=FALSE] # keep only original columns before match
    foundhmdb <- foundhmdb %>% left_join(masterhmdb,suffix=c(".cohort",".comets"))
    foundhmdb[, "chemical_id"] <- as.character(foundhmdb[, "chemical_id"])

    # bring in the masterhmdb file to find further matches
    #foundhmdb<-finharmlistg %>%
    #  filter(is.na(uid_01)) %>% # only find match for unmatched metabolites
    #  select(1:ncol(dtalist$metab)) %>%  # keep only original columns before match
    #  left_join(masterhmdb,suffix=c(".cohort",".comets"))

    ##############################################################

    # rename back so we can combine
    names(foundhmdb)<-gsub("hmdb_id",cohorthmdb,names(foundhmdb))
    names(finharmlistg)<-gsub("hmdb_id",cohorthmdb,names(finharmlistg))

    #################################################################
    #finharmlistg <- finharmlistg %>% filter(!is.na(uid_01)) # take the ones with the previous match
    # The code below was giving an error because finharmlistg and 
    #   foundhmdb did not have the same columns. To fix, add column
    #   names to one or both:
    if (nrow(foundhmdb) && nrow(finharmlistg)) {
      c1   <- colnames(finharmlistg)
      c2   <- colnames(foundhmdb)
      add  <- c1[!(c1 %in% c2)]
      if (length(add)) foundhmdb[, add] <- NA
      c2   <- colnames(foundhmdb)
      add  <- c2[!(c2 %in% c1)]
      if (length(add)) finharmlistg[, add] <- NA

      # Check the type of column in each to prevent an error
      for (v in colnames(finharmlistg)) {
        type1 <- is.numeric(finharmlistg[, v, drop=TRUE])
        type2 <- is.numeric(foundhmdb[, v, drop=TRUE])
        if (type1 && !type2) finharmlistg[, v] <- as.character(finharmlistg[, v, drop=TRUE])
        if (type2 && !type1) foundhmdb[, v]    <- as.character(foundhmdb[, v, drop=TRUE])
      }
    }

    if (nrow(foundhmdb)) {
      finharmlistg<-finharmlistg %>%
        filter(!is.na(uid_01)) %>% # take the ones with the previous match
        union_all(foundhmdb)       # union with the non-matches
    }
    #################################################################

    # fix found hmdb name
    names(finharmlistg)<-gsub(".cohort.comets",".comets",names(finharmlistg))

  }

  if(all.equal(sort(finharmlistg[,dtalist$metabId]),sort(dtalist$metab[,dtalist$metabId]))) {
  	dtalist$metab <- finharmlistg
  	return(dtalist)
  }
  else {
    stop(msg_rci_22())
  }

}

# ---------------------------------------------------------------------------
# prdebug ---------------------------------------------------
# ---------------------------------------------------------------------------
#' debug by printing object with time time
#' @keywords internal
#' @param lab label of object
#' @param x object
#
prdebug<-function(lab,x){
  print(paste(lab," = ",x," Time: ",Sys.time()))
}

checkWhereVarInData <- function(var, colNames) {

  if (!(var %in% colNames)) {
    msg <- msg_rci_17(var)
    cat(msg)
    stop(msg)
  }
  NULL

} # END: checkWhereVarInData


#' Function that subsets input data based on "where variable"
#'
#' @param readData list from readComets or readData$subjdata
#' @param where users can specify which subjects to perform the analysis by specifying this parameter. 'where' expects a vector with a variable name, a comparison operator ("<", ">", "=","<=",">="), and a value.  For example, "where = c("Gender=Female")".
#' @return filtered list
#'
filterCOMETSinput <- function(readData,where=NULL) {

  # 2023-03-03 Change readData to also be readData$subjdata

  if (!length(where)) {
    warning(msg_rci_18())
    return(readData)
  }

  if ("subjdata" %in% names(readData)) {
    subflag  <- 0
    subjdata <- readData$subjdata
  } else {
    subflag  <- 1
    subjdata <- readData 
  }

  samplesToKeep <- c()
  myfilts       <- trimws(unlist(strsplit(where,",")))
  myfilts       <- myfilts[nchar(myfilts) > 0]  
  if (!length(myfilts)) {
    warning(msg_rci_19())
    return(subjdata)
  }
  cx <- colnames(subjdata)

  # create rules for each filter
  for (i in 1:length(myfilts)) {
    myrule <- myfilts[i]
    if (length(grep("<=",myrule))>0) {
      mysplit <- strsplit(myrule,"<=")[[1]]
      myvar = gsub(" ","",mysplit[1])
      checkWhereVarInData(myvar, cx)  
      samplesToKeep <- c(samplesToKeep,
                         which(as.numeric(as.character(subjdata[,myvar])) <= as.numeric(mysplit[2])) )
    } else if(length(grep(">=",myrule))>0) {
      mysplit <- strsplit(myrule,">=")[[1]]
      myvar = gsub(" ","",mysplit[1])
      checkWhereVarInData(myvar, cx)
      samplesToKeep <- c(samplesToKeep,
                         which(as.numeric(as.character(subjdata[,myvar])) >= as.numeric(mysplit[2])) )
    } else if(length(grep("<",myrule))>0) {
      mysplit <- strsplit(myrule,"<")[[1]]
      myvar = gsub(" ","",mysplit[1])
      checkWhereVarInData(myvar, cx)
      samplesToKeep <- c(samplesToKeep,
                         which(as.numeric(as.character(subjdata[,myvar])) < as.numeric(mysplit[2])) )
    } else if(length(grep(">",myrule))>0) {
      mysplit <- strsplit(myrule,">")[[1]]
      myvar = gsub(" ","",mysplit[1])
      checkWhereVarInData(myvar, cx)
      samplesToKeep <- c(samplesToKeep,
                         which(as.numeric(as.character(subjdata[,myvar])) > as.numeric(mysplit[2])) )
    } else if (length(grep("!=",myrule))>0) {
      tmp <- getSubsFromEqWhere(subjdata, myrule, notEqual=1)   
      samplesToKeep <- c(samplesToKeep, tmp)  
    } else if (length(grep("=",myrule))>0) {
      tmp <- getSubsFromEqWhere(subjdata, myrule, notEqual=0)   
      samplesToKeep <- c(samplesToKeep, tmp)  
    } else stop(msg_rci_20())
  }
  mycounts  <- as.numeric(lapply(unique(samplesToKeep),function(x)
                                 length(which(samplesToKeep==x))))
  fincounts <- which(mycounts == length(myfilts))
  subjdata  <- subjdata[unique(samplesToKeep)[fincounts],]
  
  if (subflag) {
    return(subjdata)
  } else {
    readData$subjdata <- subjdata
    return(readData)
  }

}

# Function to identify subjects from a != or == where condition
getSubsFromEqWhere <- function(data, myrule, notEqual=1) {

  if (notEqual) {
    op <- "!="
  } else {
    op <- "="
  }
  mysplit <- strsplit(myrule, op, fixed=TRUE)[[1]]
  tmp     <- nchar(trimws(mysplit)) > 0  # Takes care of cases == and =
  mysplit <- trimws(mysplit[tmp])
  myvar   <- mysplit[1]

  checkWhereVarInData(myvar, colnames(data))
  
  # Take missing values into account
  missFlag <- length(mysplit) < 2

  # Variable could be a character variable
  vec <- data[, myvar, drop=TRUE]
  if (is.factor(vec)) vec <- unfactor(vec)
  if (!missFlag) {
    if (is.character(vec)) {
      value <- mysplit[2]
    } else {
      value <- as.numeric(mysplit[2])
    }
    tmp <- vec %in% value
  } else {
    tmp <- is.na(vec)
  }
  if (notEqual) tmp <- !tmp
  ret <- which(tmp)

  ret

} # END: getSubsFromEqWhere

# Common code for adding metabolite info
addMetabInfo <- function(corrlong, modeldata, metabdata) {

  # Defining global variables to pass Rcheck()
  metabid = uid_01 = biochemical = outmetname = outcomespec = exposuren =
    exposurep = metabolite_id = c()
  cohortvariable = vardefinition = varreference = outcome = outcome_uid =
    exposure = exposure_uid = c()
  metabolite_name = expmetname = exposurespec = c()
  adjname = adjvars = adj_uid = c()


  # patch in metabolite info for exposure or outcome by metabolite id  ------------------------
  # Add in metabolite information for outcome
  # look in metabolite metadata match by metabolite id
  corrlong$outcomespec <- as.character(lapply(corrlong$outcomespec, function(x) {
	myind <- which(names(metabdata$dict_metabnames)==x)
	if(length(myind==1)) {x=metabdata$dict_metabnames[myind]}
	return(x) }))

  corrlong <- dplyr::left_join(
    corrlong,
    dplyr::select(
      metabdata$metab,
      metabid,
      outcome_uid = uid_01,
      outmetname = biochemical
    ),
    by = c("outcomespec" = metabdata$metabId)
  ) %>%
    dplyr::mutate(outcome_uid = ifelse(!is.na(outcome_uid), outcome_uid, outcomespec)) %>%
    dplyr::mutate(outcome = ifelse(!is.na(outmetname), outmetname, outcomespec)) %>%
    dplyr::select(-outmetname)

  # Add in metabolite information and exposure labels:
  # look in metabolite metadata
  corrlong$exposurespec <- as.character(lapply(corrlong$exposurespec, function(x) {
        myind <- which(names(metabdata$dict_metabnames)==x)
        if(length(myind==1)) {x=metabdata$dict_metabnames[myind]}
        return(x) }))
  corrlong <- dplyr::left_join(
    corrlong,
    dplyr::select(
      metabdata$metab,
      metabid,
      exposure_uid = uid_01,
      expmetname = biochemical
    ),
    by = c("exposurespec" = metabdata$metabId)
  ) %>%
    #dplyr::mutate(exposure = ifelse(!is.na(expmetname), expmetname, modeldata$ccovs)) %>%
    dplyr::mutate(exposure = ifelse(!is.na(expmetname), expmetname, exposurespec)) %>%
    dplyr::mutate(exposure_uid = ifelse(!is.na(exposure_uid), exposure_uid, exposurespec)) %>%
    dplyr::select(-expmetname)

  # Add in metabolite info for adjusted variables
  # This commented-out block of code does not work correctly
  	#corrlong$adjvars <- corrlong$adjspec <- 
	#      as.character(lapply(corrlong$adjspec, function(x) {
  	#      myind <- which(names(metabdata$dict_metabnames)==x)
  	#      if(length(myind==1)) {x=metabdata$dict_metabnames[myind]}
  	#      return(x) }))

  	corrlong <- dplyr::left_join(
  	  corrlong,
  	  dplyr::select(
  	    metabdata$metab,
  	    metabid,
  	    adj_uid = uid_01,
  	    adjname = biochemical
  	  ),
  	  by = c("adjspec" = metabdata$metabId)
  	) %>%
  	  dplyr::mutate(adj = ifelse(!is.na(adjname), adjname, adjvars)) %>%
  	  dplyr::mutate(adj_uid = ifelse(!is.na(adj_uid), adj_uid, adjvars)) %>%
  	  dplyr::select(-adjname) 

  # patch in variable labels for better display and cohortvariables------------------------------------------
  # look in varmap
  vmap <-
    dplyr::select(metabdata$vmap, cohortvariable, vardefinition, varreference) %>%
    mutate(
      cohortvariable = tolower(cohortvariable),
      vardefinition = ifelse(
        regexpr("\\(", vardefinition) > -1,
        substr(vardefinition, 0, regexpr("\\(", vardefinition) - 1),
        vardefinition
      )
    )

  # get good labels for the display of outcome and exposure
  if (modeldata$modelspec == getMode_interactive()) {
    # fill in outcome vars from varmap if not a metabolite:
    if(length(suppressWarnings(grep(corrlong$outcomespec,vmap$cohortvariable)) != 0)) {
    	corrlong <-
    	  dplyr::left_join(corrlong, vmap, by = c("outcomespec" = "cohortvariable")) %>%
    	  dplyr::mutate(
    	    outcome_uid = ifelse(!is.na(varreference), varreference, outcomespec),
    	    outcome = ifelse(
    	      !is.na(outcome),
    	      outcome,
    	      ifelse(!is.na(vardefinition), vardefinition, outcomespec)
    	    )
    	  ) %>%
    	  dplyr::select(-vardefinition, -varreference)
    }

    # fill in exposure vars from varmap if not a metabolite:
    if(length(suppressWarnings(grep(corrlong$exposurespec,vmap$cohortvariable)) != 0)) {
    	corrlong <-
    	  dplyr::left_join(corrlong, vmap, by = c("exposurespec" = "cohortvariable")) %>%
    	  dplyr::mutate(
    	    exposure_uid = ifelse(!is.na(varreference), varreference, exposurespec),
    	    exposure = ifelse(!is.na(vardefinition), vardefinition, exposurespec)
    	  ) %>%
    	  dplyr::select(-vardefinition, -varreference)
       }
  }
  else if (modeldata$modelspec == getMode_batch()) {
    # fill in outcome vars from varmap if not a metabolite
    if(length(suppressWarnings(grep(corrlong$outcomespec,vmap$cohortvariable)) != 0)) {
    	corrlong <-
    	  dplyr::left_join(corrlong, vmap, by = c("outcomespec" = "varreference")) %>%
    	  dplyr::mutate(
    	    outcome_uid = ifelse(is.na(outcome_uid), outcomespec, outcome_uid),
    	    outcome = ifelse(
    	      !is.na(outcome),
    	      outcome,
    	      ifelse(!is.na(vardefinition), vardefinition, outcomespec)
    	    ),
    	    outcomespec = ifelse(!is.na(cohortvariable), cohortvariable, outcomespec)
    	  ) %>%
    	  dplyr::select(-vardefinition, -cohortvariable)
    }

    # fill in exposure vars from varmap if not a metabolite:
    if(length(suppressWarnings(grep(corrlong$exposurespec,vmap$cohortvariable)) != 0)) {
    	corrlong <-
    	  dplyr::left_join(corrlong, vmap, by = c("exposurespec" = "varreference")) %>%
    	  dplyr::mutate(
    	    exposure_uid = exposurespec,
    	    exposure = ifelse(
    	      !is.na(exposure),
    	      exposure,
    	      ifelse(!is.na(vardefinition), vardefinition, exposurespec)
    	    ),
    	    exposure = ifelse(!is.na(vardefinition), vardefinition, exposurespec),
    	    exposurespec = ifelse(!is.na(cohortvariable), cohortvariable, exposurespec)
    	  ) %>%
    	  dplyr::select(-vardefinition, -cohortvariable)
   }
  }

  corrlong

} # END: addMetabInfo

