#---------------------------------------------------------
# Get Model data and model components --------------------
#---------------------------------------------------------
#' Get data ready for the model runs in interactive or batch mode as specified in the input
#'
#' @param readData list from readComets
#' @param modelspec How model is specified (Interactive or Batch)
#' @param modbatch  if batch, chosen model specified by batch mode
#' @param rowvars   if Interactive, outcome variables (usually metabolites rendered in rows, default is All metabolites)
#' @param colvars   if Interactive, exposure variables (usually covariates rendered in columns)
#' @param adjvars   If Interactive, adjustment covariates
#'
#' @return a list comprising:
#'
#' 1: subset data: gdta
#'
#' 2: column variables: ccovs
#'
#' 3: row variables: rcovs
#'
#' 4: adjustment variables: acovs
#'
#' @examples
#'
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modbatch="1.1 Unadjusted")
#'
#' @export

getModelData <-  function(readData,
           modelspec = "Batch",
           modbatch  = "",
           rowvars   = "All metabolites",
           colvars   = "",
           adjvars   = NULL) {

    if(is.na(match(modelspec,c("Interactive","Batch")))) {
	stop("modelspec is not an allowable value.  Use 'Interactive' or 'Batch'")
    }

    if(modelspec == "Interactive" && modbatch != "") {
	print("Warning: Interactive mode is set yet modbatch is also assigned.  modbatch is ignored and model is assumed to be in Interactive mode")
    }

    # figure out the model specification based on type (Interactive or Batch)
    if (modelspec == "Interactive") {

      # rename the variables (Assumed to be 'All metabolites' by default)
      if (!is.na(match("All metabolites",rowvars))) {
        print("Analysis will run on 'All metabolites'")
        rcovs <-
          unique(c(rowvars[rowvars != "All metabolites"],c(readData$allMetabolites)))
      }  else {
        rcovs <- unlist(strsplit(rowvars," "))
      }

      # rename the exposure variables
      if (!is.na(match("All metabolites",colvars))) {
        ccovs <-
          unique(c(colvars[colvars != "All metabolites"],c(readData$allMetabolites))) } else {
        ccovs <- unlist(strsplit(colvars," ")) }

      # rename the covariables
      if (!is.null(adjvars)) {
        acovs <- unlist(strsplit(adjvars," ")) } else {
        acovs<-adjvars }

      # Throw error if an ajdusted covariate is also an exposure and there is only 1 exposure
      if (length(intersect(adjvars,ccovs))>0 && length(ccovs)==1) {
	stop("ERROR: one of the adjusted covariates is also an exposure and there is only one exposure!!
		Please make sure adjusted covariates are not exposures.")
     }

      # Throw a warning if an ajdusted covariate is also an exposure and there is more than 1 exposure
      if (length(intersect(adjvars,ccovs))>0 && length(ccovs)>length(adjvars)) {
	vartoremove=intersect(adjvars,ccovs)
        print(paste0("WARNING: one of the adjusted covariates is also an exposure!!\n",
		"The variable ",vartoremove," will be dropped from the list of exposures"))
	ccovs=setdiff(ccovs,adjvars)
     }

      # Throw error if an adjusted covariate is also an outcome and there is only 1 outcome
      if (length(intersect(adjvars,rcovs))>0 && length(rcovs)==1) {
        stop("ERROR: one of the adjusted covariates is also an outcome and there is only 1 outcome!!
                Please make sure adjusted covariates are not outcomes.")
      }

      # Throw a warning if an adjusted covariate is also an outcome amd there is more than 1 outcome
      if (length(intersect(adjvars,rcovs))>0 && length(rcovs)>length(adjvars)) {
	vartoremove=intersect(adjvars,rcovs)
        print(paste0("WARNING: one of the adjusted covariates is also an outcome!!\n",
                "The variable(s) ",vartoremove," will be dropped from the list of outcomes"))
	rcovs=setdiff(rcovs,adjvars)
      }

    } # end if modelspec is "Interactive"

    else if (modelspec == "Batch") {
      # here we need to get the covariates defined from the excel sheet
      # step 1. get the chosen model first

      if(modbatch=="") {
	stop("modelspec is set to 'Batch' yet model batch (modbatch) is empty.  Please set modbatch.")
      }

      # defining global variable to remove Rcheck warnings
      model=c()
      mods<-dplyr::filter(as.data.frame(readData[["mods"]]),model==modbatch)
      if(nrow(mods)==0) {
	stop("The model batch (modbatch) provided does not exist in the input Excell file")
      }
      if (length(mods)>0 & mods$outcomes=="All metabolites")
        rcovs<-c(readData[[2]])
      else
        rcovs<-as.vector(strsplit(mods$outcomes," ")[[1]])

      if (length(mods)>0 & mods$exposure=="All metabolites")
        ccovs<-c(readData[[2]])
      else
        ccovs<-as.vector(strsplit(mods$exposure," ")[[1]])

      if (!is.na(mods$adjustment))
        acovs<-as.vector(strsplit(mods$adjustment," ")[[1]])
      else acovs<-NULL

    } # end if modelspec == "Batch"

  # Keep only needed variables for the data
    if (is.null(acovs)) {
      gdta <-dplyr::select(readData[[1]], one_of(c(ccovs, rcovs)))
    }
    else {
      gdta <-
        dplyr::select(readData[[1]], one_of(c(acovs,ccovs, rcovs)))
    }

    # list for subset data
    # 1: subset data: gdta
    # 2: column variables: ccovs
    # 3: row variables: rcovs
    # 4: adjustment variables: acovs
#    if (dobug)
#      prdebug("End of getdata:", dim(gdta))
    list(
      gdta = gdta,
      ccovs = ccovs,
      rcovs = rcovs,
      acovs = acovs
    )

  }

