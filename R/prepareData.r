



#---------------------------------------------------------
# Get Model data -----------------------------------------
#---------------------------------------------------------
#' Get data ready for the model runs in interactive or batch mode as specified in the input
#'
#' @param readData list from readComets
#' @param rowvars   Outcome variables (usually metabolites and rendered in rows default in All metabolites)
#' @param colvars   Exposure variables (usually covariates rendered in columns)
#' @param adjvars   Adjustment covariates
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
#' modeldata <- getModelData(readData(),modbatch="Interactive",colvars="age")
#'
#' @export

getModelData <-
  function(readData,
           rowvars   = "All metabolites",
           colvars   = "",
           adjvars   = NULL) {

      # adjust the variable names
      # found all metabolites
      if (!is.na(match("All metabolites", rowvars)))
        rcovs <-
          unique(c(rowvars[rowvars != "All metabolites"], c(readData[[2]])))
      else
        rcovs <- rowvars


      if (!is.na(match("All metabolites", colvars)))
        ccovs <-
          unique(c(colvars[colvars != "All metabolites"], c(readData[[2]])))
      else
        ccovs <- colvars

      acovs <- adjvars

    # merge smetab
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
