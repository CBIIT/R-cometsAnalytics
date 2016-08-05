

#---------------------------------------------------------
# Get Model data and model components --------------------
#---------------------------------------------------------
#' Get data ready for the model runs in interactive or batch mode as specified in the input
#'
#' @param readData list from readComets
#' @param modelspec How model is specified (Interactive or Batch)
#' @param modbatch  if batch, chosen model specified by batch mode
#' @param rowvars   if Interactive, outcome variables (usually metabolites and rendered in rows default in All metabolites)
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
#' csvfile <- file.path(dir, "cometsInput.xlsx")
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

    # figure out the model specification based on type (Interactive or Batch)
    if (modelspec == "Interactive") {
      # adjust the variable names
      # found all metabolites
      if (!is.na(match("All metabolites",rowvars)))
        rcovs <-
          unique(c(rowvars[rowvars != "All metabolites"],c(readData[[2]])))
      else
        rcovs <- rowvars


      if (!is.na(match("All metabolites",colvars)))
        ccovs <-
          unique(c(colvars[colvars != "All metabolites"],c(readData[[2]])))
      else
        ccovs <- colvars

      acovs <- adjvars
    }
    else if (modelspec == "Batch") {
      # here we need to get the covariates defined from the excel sheet
      # step 1. get the chosen model first


      mods<-dplyr::filter(as.data.frame(readData[["mods"]]),model==modbatch)
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

    }


    # Keep only needed variables for the data
    if (length(acovs) == 0) {
      gdta <-
        subset(as.data.frame(readData[[1]]), select = c(ccovs, rcovs))
    }
    else {
      gdta <-
        subset(as.data.frame(readData[[1]]), select = c(acovs, ccovs, rcovs))
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


