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
checkIntegrity <- function (dta.metab,dta.smetab, dta.sdata,dta.vmap,dta.models,dict_metabnames) {

    print("Running Integrity Check...")
    # get the cohort equivalent of metabolite_id and subject id
    metabid = tolower(dta.vmap$cohortvariable[tolower(dta.vmap$varreference) == "metabolite_id"])
    subjid = tolower(dta.vmap$cohortvariable[tolower(dta.vmap$varreference) == 'id'])
    subjid.smetab = names(dict_metabnames)[which(dict_metabnames==subjid)] # to access dta.smetab
    # add _ to all metabolites before splitting at blank
    allmodelparams=c(dta.models$outcomes,dta.models$exposure, dta.models$adjustment,dta.models$stratification)
    allmodelparams=gsub("All metabolites","All_metabolites",gsub("\\s+", " ", allmodelparams[!is.na(allmodelparams)]))
    print(paste(dta.models$ccovs,dta.models$scovs))

    # take out multiple blanks and add _ to all metabolites to avoid splitting
    allmodelparams=tolower(unique(unlist(stringr::str_split(allmodelparams," "))))
    outmessage = c()
    if (length(metabid) == 0) {
      stop("metabid is not found as a parameter in VarMap sheet!  Specify which column should be used for metabolite id")
    }
    else if (!is.na(dta.models$stratification) &&
		length(intersect(dta.models$adjustment,dta.models$stratification))>=1) {
	stop("Adjustment and stratification parameters are the same!  This is not allowed.")
    }
    else if (!is.na(dta.models$stratification) &&
		length(intersect(dta.models$exposure,dta.models$stratification))>=1) {
        stop("Exposure and stratification parameters are the same!  This is not allowed.")
    }
    else if (length(intersect(allmodelparams,
         tolower(c("All_metabolites",  dta.vmap$varreference)))) !=length(allmodelparams))
# tolower(c("All metabolites", colnames(dta.smetab), colnames(dta.sdata)))))!=length(allmodelparams))
{
         stop("Parameters in model data ('Models' sheet in input file) do not exist!  Check the naming!")
    }
    else if (length(subjid) == 0) {
        stop("id (for subject id) is not found as a parameter in VarMap sheet!  Specify which column should be used for subject id")
    }
    else if (length(intersect(subjid,colnames(dta.sdata))) != 1) {
        stop("The user input id in the 'COHORTVARIABLE' column of the Varmap Sheet is not found in the 'SubjectData' sheet. Check the input file.")
    }
    #else if (length(intersect(subjid,colnames(dta.smetab))) != 1) {
     else if (length(intersect(subjid,dict_metabnames)) !=1) {
        stop("The user input id in the 'COHORTVARIABLE' column of the Varmap Sheet is not found in the 'SubjectMetabolites' sheet. Check the input file.")
    }
    else if (length(intersect(metabid,colnames(dta.metab))) != 1) {
        stop("The user input metabolite_id in the 'COHORTVARIABLE' column of the Varmap Sheet is not found in the 'Metabolites' sheet. Check the input file.")
    }
    else {
      #print("Passed the checks")
      dta.metab[[metabid]] = tolower(dta.metab[[metabid]])
      dta.sdata[[subjid]] = tolower(dta.sdata[[subjid]])
      dta.smetab[[subjid.smetab]] = tolower(dta.smetab[[subjid.smetab]])
      if (length(grep(metabid,colnames(dta.metab))) == 0) {
          stop("Error: Metabolite ID from 'VarMap Sheet' (",metabid,") does not match column name from 'Metabolites Sheet'")
      }
      else if (length(grep(subjid,colnames(dta.sdata))) == 0) {
          stop("Error: Sample ID from 'VarMap Sheet' (",subjid,") does not match a column name in 'SubjectData Sheet'")
      }
      else if (length(unique(dta.sdata[,subjid])) != length(unique(dta.smetab[,subjid.smetab]))) {
        outmessage = c(
          outmessage,"Number of subjects in SubjectData sheet does not match number of subjects in SubjectMetabolites sheet"
        )
      }
      else if (length(unique(colnames(dta.smetab))) != ncol(dta.smetab)) {
        outmessage = c(
          outmessage,"Metabolite abundances sheet (SubjectMetabolites) contains duplicate columns (metabolite names)"
        )
      }
      else if (length(unique(unlist(dta.sdata[,subjid]))) != nrow(dta.sdata)) {
        outmessage = c(
          outmessage,"Warning: Sample Information sheet (SubjectData) contains duplicate ids"
        )
      }
      else if (length(unique(unlist(dta.metab[,metabid]))) != nrow(dta.metab)) {
        outmessage = c(
          outmessage,"Warning: Metabolite Information sheet (Metabolites) contains duplicate metabolite ids"
        )
      }
      else {
        nummetab = length(unique(colnames(dta.smetab)[-c(which(colnames(dta.smetab) ==
                                                                 subjid.smetab))]))
        numsamples = length(unique(dta.smetab[[subjid.smetab]]))
        if (length(intersect(as.character(unlist(dta.metab[,metabid])),colnames(dta.smetab)[-c(which(colnames(dta.smetab) ==
            subjid.smetab))])) == nummetab &&
            length(intersect(as.character(unlist(dta.sdata[,subjid])),dta.smetab[[subjid.smetab]])) ==
            numsamples) {
          outmessage = c(
            outmessage,"Passed all integrity checks, analyses can proceed. If you are part of COMETS, please download metabolite list below and submit to the COMETS harmonization group."
          )
        }
        else {
          # if (length(intersect(tolower(make.names(dta.metab[[metabid]])),tolower(colnames(dta.smetab)))) !=
          #    nummetab) {
	  if (length(intersect(names(dict_metabnames)[match(tolower(dta.metab[[metabid]]),dict_metabnames)],
		tolower(colnames(dta.smetab)))) != nummetab) {	
              stop("Error: Metabolites in SubjectMetabolites DO NOT ALL match metabolite ids in Metabolites Sheet")
          }
          if (length(intersect(dta.sdata[[subjid]],dta.smetab[[subjid.smetab]])) !=
              numsamples) {
              stop("Error: Sample ids in SubjectMetabolites DO NOT ALL match subject ids in SubjectData sheet")
          }
        }
      }
    }

   ########################################
   # Check that models are reasonable
   ########################################
   # Check that adjustment variables that at least two unique values
##   for (i in dta.models$adjustment) {
##        temp <- length(unique(dta.sdata[[i]]))
##	if(temp <= 1 && !is.na(i)) {
##		outmessage<-c(outmessage,paste("Error: one of your models specifies",i,"as an adjustment but that variable only has
##			one possible value"))
##   	}
##   }
##
##   # Check that stratification variables that at least two unique values
##   for (i in dta.models$stratification) {
##        temp <- length(unique(dta.sdata[[i]]))
##        if(temp <= 1 && !is.na(i)) {
##                outmessage<-c(outmessage,paste("Error: one of your models specifies",i,"as an stratification but that variable only has
##                        one possible value"))
##        }
##   }

    if (is.null(outmessage)) {
      outmessage = "Input data has passed QC (metabolite and sample names match in all input files)"
    }

    # rename subjid in dta.smetab sheet for merging later on
    colnames(dta.smetab)[which(colnames(dta.smetab)==subjid.smetab)] <- subjid

    return(
      list(
        dta.smetab = dta.smetab,dta.metab = dta.metab, dta.sdata = dta.sdata,outmessage =
          outmessage
      )
    )
  } # end checkIntegriy


# ---------------------------------------------------------------------------
# Harmonize ---------------------------------------------------
# ---------------------------------------------------------------------------
#' Harmonizes metabolites by looking up metabolites names from user input and finding the corresponding COMETS harmonized name.
#' @keywords internal
#' @param dtalist results of reading a CSV data sheet (with read_excel)

Harmonize<-function(dtalist){
  mastermetid=metabolite_name=metlower=uid_01=cohorthmdb=foundhmdb=masterhmdb=NULL
  # Load processed UIDs file:
  dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
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

    # bring in the masterhmdb file to find further matches
    foundhmdb<-finharmlistg %>%
      filter(is.na(uid_01)) %>% # only find match for unmatched metabolites
      select(1:ncol(dtalist$metab)) %>%  # keep only original columns before match
      left_join(masterhmdb,suffix=c(".cohort",".comets"))

    # rename back so we can combine
    names(foundhmdb)<-gsub("hmdb_id",cohorthmdb,names(foundhmdb))
    names(finharmlistg)<-gsub("hmdb_id",cohorthmdb,names(finharmlistg))

    finharmlistg<-finharmlistg %>%
      filter(!is.na(uid_01)) %>% # take the ones with the previous match
      union_all(foundhmdb)       # union with the non-matches

    # fix found hmdb name
    names(finharmlistg)<-gsub(".cohort.comets",".comets",names(finharmlistg))

  }


  if(all.equal(sort(finharmlistg[,dtalist$metabId]),sort(dtalist$metab[,dtalist$metabId]))) {
  	dtalist$metab <- finharmlistg
  	return(dtalist)
  }
  else {
	stop("Something went wrong with the harmonization")
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

#' Function that subsets input data based on "where variable"
#'
#' @param readData list from readComets
#' @param where users can specify which subjects to perform the analysis by specifying this parameter. 'where' expects a vector with a variable name, a comparison operator ("<", ">", "=","<=",">="), and a value.  For example, "where = c("Gender=Female")". Multiple where statements should be comma separated (a vector).
#' @return filtered list
#'
filterCOMETSinput <- function(readData,where=NULL) {
  if (!is.null(where)) {
	samplesToKeep=c()
	myfilts <- strsplit(where,",")
	# create rules for each filter
	for (i in 1:length(myfilts)) {
		myrule <- myfilts[[i]]
                if(length(grep("<=",myrule))>0) {
                        mysplit <- strsplit(myrule,"<=")[[1]]
			#myvar <- readData$vmap$cohortvariable[which(readData$vmap$varreference==gsub(" ","",mysplit[1]))]
                        myvar = gsub(" ","",mysplit[1])
			samplesToKeep <- c(samplesToKeep,
                           which(as.numeric(as.character(readData$subjdata[,myvar])) <= gsub(" ","",as.numeric(mysplit[2]))) )
                } else if(length(grep(">=",myrule))>0) {
                        mysplit <- strsplit(myrule,">=")[[1]]
			#myvar <- readData$vmap$cohortvariable[which(readData$vmap$varreference==gsub(" ","",mysplit[1]))]
                        myvar = gsub(" ","",mysplit[1])
			samplesToKeep <- c(samplesToKeep,
                           which(as.numeric(as.character(readData$subjdata[,myvar])) >= gsub(" ","",as.numeric(mysplit[2]))) )
                } else if(length(grep("<",myrule))>0) {
			mysplit <- strsplit(myrule,"<")[[1]]
			#myvar <- readData$vmap$cohortvariable[which(readData$vmap$varreference==gsub(" ","",mysplit[1]))]
			myvar = gsub(" ","",mysplit[1])
               		samplesToKeep <- c(samplesToKeep,
                           which(as.numeric(as.character(readData$subjdata[,myvar])) < gsub(" ","",as.numeric(mysplit[2]))) )
        	} else if(length(grep(">",myfilts[i]))>0) {
	        	mysplit <- strsplit(myrule,">")[[1]]
			#myvar <- readData$vmap$cohortvariable[which(readData$vmap$varreference==gsub(" ","",mysplit[1]))]
			myvar = gsub(" ","",mysplit[1])
                        samplesToKeep <- c(samplesToKeep,
                           which(as.numeric(as.character(readData$subjdata[,myvar])) > as.numeric(gsub(" ","",mysplit[2]))) )
		} else if (length(grep("=",myfilts[i]))>0) {
			mysplit <- strsplit(myrule,"=")[[1]]
			# myvar <- readData$vmap$cohortvariable[which(readData$vmap$varreference==gsub(" ","",mysplit[1]))]
                        myvar = gsub(" ","",mysplit[1])
			samplesToKeep <- c(samplesToKeep,
                           which(as.numeric(as.character(readData$subjdata[,myvar])) == gsub(" ","",as.numeric(mysplit[2]))) )
        	} else
                stop("Make sure your 'where' filters contain logicals '>', '<', or '='")
        }
	mycounts <- as.numeric(lapply(unique(samplesToKeep),function(x)
		length(which(samplesToKeep==x))))
	fincounts <- which(mycounts == length(myfilts))
        readData$subjdata <- readData$subjdata[unique(samplesToKeep)[fincounts],]
  }
  else {(warning("No filtering was performed because 'where' parameter is NULL"))}
return(readData)
}


#' Ensures that models will run without errors.  Preprocesses design matrix for 
#' zero variance, linear combinations, and dummies.
#' @keywords internal
#' @param modeldata (output of function getModelData())
#' @return modeldata after checks are performed
checkModelDesign <- function (modeldata=NULL) {
	if(is.null(modeldata)) {
		stop("Please make sure that modeldata is defined")
	}
 errormessage=warningmessage=c()
 # Check that there are at least 25 samples (n>=25) reference https://link.springer.com/content/pdf/10.1007%2FBF02294183.pdf
  if (nrow(modeldata$gdta)<25){
    if (!is.null(modeldata$scovs)){
      #warning(paste("Data has < 25 observations for strata in",modeldata$scovs))
      mycorr=data.frame()
      attr(mycorr,"ptime")="Processing time: 0 sec"
      return(mycorr)
    } else{
      stop(paste(modeldata$modlabel," has less than 25 observations and will not be run."))
    }
  }

   # Check that adjustment variable that at least two unique values
   for (i in modeldata$acovs) {
        temp <- length(unique(modeldata$gdta[[i]]))
        if(temp <= 1 && !is.na(i)) {
               warning(paste("One of your models specifies",i,"as an adjustment value but that variable only has one possible value.",
               "Model will run without",i,"as an adjustment"))
               modeldata$acovs <- setdiff(modeldata$acovs,i)
        }
   }

  	metabid=uid_01=biochemical=outmetname=outcomespec=exposuren=exposurep=metabolite_id=c()
  	cohortvariable=vardefinition=varreference=outcome=outcome_uid=exposure=exposure_uid=c()
  	metabolite_name=expmetname=exposurespec=c()

  	# column indices of row/outcome covariates
  	col.rcovar <- match(modeldata[["rcovs"]],names(modeldata[["gdta"]]))

  	# column indices of column/exposure covariates
  	col.ccovar <- match(modeldata[["ccovs"]],names(modeldata[["gdta"]]))

  	# column indices of adj-var
  	col.adj <- match(modeldata[["acovs"]],names(modeldata[["gdta"]]))

  	# Defining global variable to remove R check warnings
  	corr=c()
  	loadNamespace("caret") #need this to avoid problem of not finding contr.ltfr

  	# check if any of the exposure and adjustments are factors
  	ckfactor<-sapply(dplyr::select(modeldata$gdta,dplyr::one_of(modeldata$rcovs,modeldata$acovs)),class)
  	hasfactor<-NULL
  	if (length(ckfactor[ckfactor=="factor"])==0){
  	  hasfactor<-FALSE
  	} else {
  	  hasfactor<-TRUE
  	}

	# If all vs all, only check variance is zero for covars and allvars
	if(modeldata$allvsall | hasfactor==FALSE) {
		print("No factors found,  only performing near zero variance check for all covariates.")
		nonzeroc <- caret::nearZeroVar(modeldata$gdta[,modeldata$ccovs],freqCut = 95/5)
		if(length(nonzeroc)>0) {
			#modeldata$ccovs <- modeldata$ccovs[-nonzeroc]
			warningmessage <- c(warningmessage,
			                    paste0("Removed ",length(nonzeroc),"exposure(s):",
			#paste(colnames(modeldata$gdta)[unique(nonzeroc)],
			paste(modeldata$dict_metabnames[colnames(modeldata$gdta[,modeldata$ccovs])[unique(nonzeroc)]],
			                                            collapse=","),
			                           " because of zero-variance",collapse=""))

			temp <- modeldata$ccovs[nonzeroc]
			modeldata$ccovs <- modeldata$ccovs[-nonzeroc]
			nonzeroc <- temp
		}
		nonzeror <- caret::nearZeroVar(modeldata$gdta[,modeldata$rcovs],freqCut = 95/5)
		if(length(nonzeror)>0) {
		  #modeldata$rcovs <- modeldata$rcovs[-nonzeror]
		  warningmessage <- c(warningmessage,
		                      paste0("Removed ",length(nonzeror)," outcome(s): ",
					#paste(colnames(modeldata$gdta)[unique(nonzeror)],
			paste(modeldata$dict_metabnames[colnames(modeldata$gdta[,modeldata$rcovs])[unique(nonzeror)]],
		                                              collapse=","),
		                             " because of zero-variance",collapse=""))
		  temp <- modeldata$rcovs[nonzeror]
		  modeldata$rcovs <- modeldata$rcovs[-nonzeror]
		  nonzeror <- temp
		}
		if (length(modeldata$acovs)>0){
		nonzeroa <- caret::nearZeroVar(modeldata$gdta[,modeldata$acovs],freqCut = 95/5)
		if(length(nonzeroa)>0) {
		  # modeldata$acovs <- modeldata$acovs[-nonzeroa]
		  warningmessage <- c(warningmessage,
		                      paste0("Removed ",length(nonzeroa),"adjustment(s):",
					#paste(colnames(modeldata$gdta)[unique(nonzeroa)],
					paste(modeldata$dict_metabnames[colnames(modeldata$gdta[,modeldata$acovs])[unique(nonzeroa)]],
		                                              collapse=","),
		                             " because of zero-variance",collapse=""))
		  temp <- modeldata$acovs[nonzeroa]
		  modeldata$acovs <- modeldata$acovs[-nonzeroa]
		  nonzeroa <- temp
		}
		if (length(unique(c(nonzeroa,nonzeroc,nonzeror)))>0){
		  # Get indices before modifying gdta.  They need to be converted to names above or else
		  # the wrong indices are getting removed (because the nearZeroVar function returns indices
		  # based on input, and the input is not the entire gdta data frame)
		  myind <- as.numeric(lapply(unique(c(nonzeroa,nonzeroc,nonzeror)), function(x) {
			which(colnames(modeldata$gdta)==x)}))
		  #modeldata$gdta <- modeldata$gdta[,-unique(c(nonzeroa,nonzeroc,nonzeror))]
		  modeldata$gdta <- modeldata$gdta[,-myind]
		}
		}

			 return(list(warningmessage=warningmessage,errormessage=errormessage,
				modeldata=modeldata))

        } else {

  # Create models to run for each ccovar
	# Create dummy variables
	myformula <- paste0("`",colnames(modeldata$gdta)[col.rcovar], "` ~ ",
		paste0("`",colnames(modeldata$gdta)[c(col.ccovar, col.adj)],"`",collapse = " + "))
	dummies <- caret::dummyVars(myformula, data = modeldata$gdta,fullRank = TRUE)
	mydummies <- stats::predict(dummies, newdata = modeldata$gdta)
	# Rename variables if they are returned in mydummies
	tempccovs <- as.character(unlist(sapply(modeldata$ccovs,
		function(x) grep(x,colnames(mydummies),value=TRUE,fixed=TRUE))))
	if(length(tempccovs)>0) {
		modeldata$ccovs <- tempccovs
	}
	# Check if adjusted covariates are present or else grep will return "" and will
	# always return something
	if(!is.null(modeldata$acovs)) {
		tempacovs <- as.character(unlist(sapply(modeldata$acovs,
			function(x) grep(x,colnames(mydummies),value=TRUE,fixed=TRUE))))
		if(length(tempacovs)>0) {
			modeldata$acovs <- tempacovs
		}
	}

	# Now perform all the checks on the design matrix (if there are adjusted covariates)
	if(!is.null(mydummies)) {
		# Check for zero-variance predictors (e.g. a stratified group that only has 1 value)
		nonzero <- caret::nearZeroVar(mydummies,freqCut = 95/5)
		if(length(nonzero)>0) {
		        filtdummies <- mydummies[,-nonzero]
			if(is.null(ncol(filtdummies))) {
				filtdummies <- as.matrix(filtdummies)
				colnames(filtdummies) <- colnames(mydummies)[-nonzero]
			}
			warningmessage <- c(warningmessage,
				paste0("Removed ",paste(colnames(mydummies)[unique(nonzero)],collapse=","),
					" because of zero-variance",collapse=""))
		} else {
		        filtdummies <- mydummies
		}

		  # Check for correlated predictors (this will remove the first "factor" that is highly
		  # correlated with another
#		  if(is.numeric(filtdummies)) { # meaning there is only one column retained and it's now a vector
#			errormessage <- c(errormessage,"Covariates failed design model check (zero variance). Model will not be run")
#			return(list(warningmessage=warningmessage,errormessage=errormessage,modeldata=modeldata))
#		  }

		  if (!is.null(ncol(filtdummies)) && ncol(filtdummies)>1){
			cors <- caret::findCorrelation(stats::cor(as.data.frame(filtdummies),method="spearman"), cutoff = .97)
		 		if(length(cors)>0) {
		       		  filtdummies2 <- filtdummies[,-cors]
				  warningmessage <- c(warningmessage,
        	       	          paste("Removed ",paste(colnames(mydummies)[unique(cors)],collapse=","),
					" because of correlation > 0.97 with other covariates",collapse=""))
		  		} else {
			  filtdummies2=filtdummies
		  		}
			#Check for linear dependencies
			ldeps <- caret::findLinearCombos(filtdummies2)
			if(length(ldeps$remove)>0) {
			        findummies <-filtdummies2[,-ldeps$remove]
				warningmessage <- c(warningmessage,
        	                paste0("Removed ",paste(colnames(mydummies)[unique(ldeps$remove)],collapse=","),
					" because of linear dependencies",collapse=""))
			} else {
		        	findummies=filtdummies2
			}
		    } else {
		  	findummies=filtdummies
		    }

		  # check for ill conditioned square matrix for cor - on hold but consider trim.matrix in subselect package
	  	  ckqr<-subselect::trim.matrix(cor(findummies,method = "spearman"))
		  if (length(ckqr$names.discarded)>0) {
		  	findummies<-findummies[,-match(ckqr$names.discarded,colnames(findummies))]
		  	warningmessage <- c(warningmessage,paste("Removed ill-conditioned covariate(s) removed:",ckqr$names.discarded,collapse = ", "))
		  }
	  }
	# Now run check on outcome
	# check for variance near 0 for rcovs (outcome)
	outcwvar<-caret::nearZeroVar(modeldata$gdta[,modeldata$rcovs],freqCut = 95/5)
	if (length(outcwvar)>0){
	  	warningmessage <- c(warningmessage,paste("Near zero variance for these outcome(s) removed:",paste(modeldata$rcovs[outcwvar],collapse = ", ")))
		outcwvar<-modeldata$rcovs[-outcwvar]
	} else {
		outcwvar<-modeldata$rcovs
	}
	if(length(outcwvar)>0) {
		newdat <- cbind(modeldata$gdta[,outcwvar],findummies)
		colnames(newdat)[1:length(outcwvar)]=outcwvar
		# Remove "`" that were introduced when building the formula
		colnames(newdat)=gsub("^`","",gsub("`$","",colnames(newdat)))
		colnames(findummies)=gsub("^`","",gsub("`$","",colnames(findummies)))
		if(!is.null(modeldata$acovs)) {
			modeldata$acovs <- as.character(unlist(sapply(modeldata$acovs,
				function(x) grep(x,setdiff(colnames(findummies),c(modeldata$ccovs,modeldata$rcovs,modeldata$scovs)),value=TRUE,fixed=TRUE))))
		}
		modeldata$ccovs <- as.character(unlist(sapply(modeldata$ccovs,
			function(x) grep(x,colnames(findummies),value=TRUE,fixed=TRUE))))
		modeldata$rcovs <- outcwvar
		modeldata$gdta <- newdat
	} else {
		errormessage <- "All outcomes have near-zero variance, model(s) will not be run."
	}
     #print(warningmessage)
     #print(errormessage)
     return(list(warningmessage=warningmessage,errormessage=errormessage,modeldata=modeldata))
     }
}


#--------------------------------------------------------
#' Check model design and calculate correlation matrix. This is an internal function.  Use runCorr() instead (which is a wrapper to this function).
#'
#' @param modeldata list from function getModelData
#' @param metabdata metabolite data list
#' @param cohort cohort label (e.g DPP, NCI, Shanghai)
#'
#' @return data frame with each row representing the correlation for each combination of outcomes and exposures represented as specified in the
#' model (*spec), label (*lab), and universal id (*_uid)
#' with additional columns for n, pvalue, method of model specification (Interactive or Batch), universal id for outcomes () and exposures
#' name of the cohort and adjustment variables. Attribute of dataframe includes ptime for processing time of model
#' run.
calcCorr <- function(modeldata, metabdata, cohort = "") {
  .Machine$double.eps <- 1e-300

  # Defining global variables to pass Rcheck()
  #ptm <- proc.time() # start processing time
  metabid = uid_01 = biochemical = outmetname = outcomespec = exposuren =
    exposurep = metabolite_id = c()
  cohortvariable = vardefinition = varreference = outcome = outcome_uid =
    exposure = exposure_uid = c()
  metabolite_name = expmetname = exposurespec = c()

  # column indices of row/outcome covariates
  col.rcovar <-
    match(modeldata[["rcovs"]], names(modeldata[["gdta"]]))

  # column indices of column/exposure covariates
  col.ccovar <-
    match(modeldata[["ccovs"]], names(modeldata[["gdta"]]))

  # column indices of adj-var
  col.adj <- match(modeldata[["acovs"]], names(modeldata[["gdta"]]))

  # Defining global variable to remove R check warnings
  corr = c()

  # Check model design
  designcheck <- checkModelDesign(modeldata)
  if (length(names(designcheck)) == 0) {
    return(designcheck)
  }

  newmodeldata <- designcheck$modeldata
  #print(designcheck$warningmessage)
  if (length(designcheck$warningmessage) > 0) {
    print(designcheck$warningmessage)
  }
  if (length(designcheck$errormessage) > 0) {
    print(designcheck$errormessage)
    return(NULL)
  }

  # readjust exposure and adjustment covariates
  col.adj <-
    match(newmodeldata[["acovs"]], colnames(newmodeldata[["gdta"]]))
  col.ccovar <-
    match(newmodeldata[["ccovs"]], colnames(newmodeldata[["gdta"]]))
  col.rcovar <-
    match(newmodeldata[["rcovs"]], colnames(newmodeldata[["gdta"]]))



  if (length(col.adj) == 0) {
    print("running unadjusted")

    data <- newmodeldata$gdta[ , unique(c(col.rcovar, col.ccovar))]
    # calculate unadjusted spearman correlation matrix
    corrhm <- Hmisc::rcorr(as.matrix(data), type = "spearman")

    # Need to transpose pval and corrhm if there's only one row bc data.frame will switch the rows
    # and columns when there's only one row. 
    if(length(newmodeldata$rcovs)==1) {
	corr <- t(data.frame(corrhm$r[newmodeldata$rcovs,newmodeldata$ccovs]))
	n<-t(data.frame(corrhm$n[newmodeldata$rcovs,newmodeldata$ccovs]))
    } else {
    	corr <- data.frame(corrhm$r[newmodeldata$rcovs,newmodeldata$ccovs])
        n <- data.frame(corrhm$n[newmodeldata$rcovs,newmodeldata$ccovs])
    }
    #    pval <- as.data.frame(corrhm$P[1:length(col.rcovar),-(1:length(col.rcovar))])
    # Calculate p-values by hand to ensure that enough precision is printed:
    	ttval <- sqrt(n--2) * corr / sqrt(1 - corr ** 2)
    # From this t-statistic, loop through and calculate p-values
    pval <- ttval
    for (i in 1:length(newmodeldata$ccovs)) {
      pval[, i] <-
        as.vector(stats::pt(
          as.matrix(abs(ttval[, i])),
          df = n[, i] - 2,
          lower.tail = FALSE
        ) * 2)
    }

    #colnames(corr) <- colnames(corrhm$r)[-(1:length(col.rcovar))]
    # Fix rownames when only one outcome is considered:
    if (length(col.rcovar) == 1) {
      rownames(corr) <- newmodeldata$rcovs
      rownames(pval) <- newmodeldata$rcovs
    }
    if (length(col.ccovar) == 1) {
      colnames(corr) <- newmodeldata$ccovs
      colnames(pval) <- newmodeldata$ccovs
    }
    # no longer needed
    #colnames(n) <- colnames(corrhm$n)[-(1:length(col.rcovar))]
    #colnames(pval) <- colnames(corrhm$P)[-(1:length(col.rcovar))]


    # If there are more than one exposure, then need to transpose - not sure why???
#    if (length(col.ccovar) > 1 && length(col.rcovar) == 1) {
#      corr = as.data.frame(t(corr))
#      pval = as.data.frame(t(pval))
#    }

  }  else {
    # calculate partial correlation matrix
    print("running adjusted")

    # Loop through and calculate cor, n, and p-values
    pval <- corr <- n <- matrix(nrow = length(newmodeldata$rcovs),
             ncol = length(newmodeldata$ccovs))
    rownames(pval) = rownames(n) = rownames(corr) = newmodeldata$rcovs
    colnames(pval) = paste0(newmodeldata$ccovs, ".p")
    colnames(n) = paste0(newmodeldata$ccovs, ".n")
    colnames(corr) = newmodeldata$ccovs
    for (i in 1:length(newmodeldata$rcovs)) {
    #  print(newmodeldata$rcovs[i])
      for (j in 1:length(newmodeldata$ccovs)) {
        if (newmodeldata$rcovs[i]!=newmodeldata$ccovs[j]) {
        temp <- ppcor::pcor.test(newmodeldata$gdta[, newmodeldata$rcovs[i]],
                                 newmodeldata$gdta[, newmodeldata$ccovs[j]],
                                 newmodeldata$gdta[, newmodeldata$acovs],
                                 method = "spearman")
        pval[i, j] <- round(temp$p.value, digits = 20)
        corr[i, j] <- round(temp$estimate, digits = 20)
        n [i, j] <- temp$n
        } else{
          pval[i, j] <- 0
          corr[i, j] <- 1
          n[i, j] <- sum(!is.na(newmodeldata$gdta[, newmodeldata$ccovs[j]]))
        }

      }
    }
    pval <- as.data.frame(pval)
    n <- as.data.frame(n)
    corr <- as.data.frame(corr)

    # Rename the adjusted covariate now to original (without dummies)
    #modeldata$acovs=oldcol.adj
    print("finished adjustment")

  } # End else adjusted mode (length(col.adj) is not zero)

  # create long data with pairwise correlations  ----------------------------------------------------
  mycols <- 1:length(col.ccovar)
  corr.togather <- cbind(corr, outcomespec = rownames(corr))
  if(class(corr.togather)=="matrix") {corr.togather<-as.data.frame(corr.togather)}
  corrlong <-
    suppressWarnings(fixData(
      data.frame(
        cohort = cohort,
        spec = modeldata$modelspec,
        model = modeldata$modlabel,
        tidyr::gather(corr.togather,
                      "exposurespec", "corr", -outcomespec),
        tidyr::gather(as.data.frame(n), "exposuren", "n", colnames(n)[mycols]),
        tidyr::gather(as.data.frame(pval), "exposurep", "pvalue", colnames(pval)[mycols]),
        adjspec = ifelse(
          length(col.adj) == 0,
          "None",
          paste(newmodeldata$acovs, collapse = " ")
        ),
        adjvars = ifelse(
          length(col.adj) == 0,
          "None",
          paste(modeldata$acovs, collapse = " ")
        )
      )
    ) %>%
    dplyr::select(-exposuren,-exposurep))

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
  	corrlong$adjvars <- corrlong$adjspec <- 
	      as.character(lapply(corrlong$adjspec, function(x) {
  	      myind <- which(names(metabdata$dict_metabnames)==x)
  	      if(length(myind==1)) {x=metabdata$dict_metabnames[myind]}
  	      return(x) }))
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
  if (modeldata$modelspec == "Interactive") {
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
  else if (modeldata$modelspec == "Batch") {
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

  # Stop the clock
  #  ptm <- base::proc.time() - ptm
  #  print(paste("My ptm:", ptm))
  #  attr(corrlong,"ptime") = paste("Processing time:",round(ptm[3],digits=6),"sec")

  return(corrlong)
}

