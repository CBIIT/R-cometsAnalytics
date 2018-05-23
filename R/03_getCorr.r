#--------------------------------------------------------
# calcCorr: get correlation matrix ------------------------
#---------------------------------------------------------
#' Calculate correlation matrix
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
#'
#' @examples
#' \dontrun{
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modlabel="1 Gender adjusted",
#' 	rowvars=c("lactose","lactate"))
#' corrmatrix <-calcCorr(modeldata,exmetabdata, "DPP")
#' }
#' @export
calcCorr <- function(modeldata, metabdata, cohort = "") {
  .Machine$double.eps <- 1e-300

  # only run getcorr for n>15
  #  if (nrow(modeldata$gdta)<15){
  #    if (!is.null(modeldata$scovs)){
  #      #warning(paste("Data has < 15 observations for strata in",modeldata$scovs))
  #      mycorr=data.frame()
  #      attr(mycorr,"ptime")="Processing time: 0 sec"
  #      return(mycorr)
  #    } else{
  #      stop(paste(modeldata$modlabel," has less than 15 observations."))
  #    }
  #  }
  #
  #   # Check that adjustment variables that at least two unique values
  #   for (i in modeldata$acovs) {
  #        temp <- length(unique(modeldata$gdta[[i]]))
  #        if(temp <= 1 && !is.na(i)) {
  #                warning(paste("Warning: one of your models specifies",i,"as an adjustment
  #		but that variable only has one possible value.
  #		Model will run without",i,"adjusted."))
  #		modeldata$acovs <- setdiff(modeldata$acovs,i)
  #        }
  #   }
  #   if (length(modeldata$acovs)==1) {modeldata$acovs=NULL}
  # Check that stratification variables that at least two unique values
  #   for (i in modeldata$scovs) {
  #        temp <- length(unique(modeldata$gdta[[i]]))
  #        if(temp <= 1 && !is.na(i)) {
  #                warning(paste("Warning: one of your models specifies",i,"as a stratification
  #		but that variable only has one possible value.
  #		Model will run without",i,"stratified"))
  #		modeldata$scovs <- setdiff(modeldata$scovs,i)
  #        }
  #   }
  #   if (length(modeldata$scovs)==1) {modeldata$scovs=NULL}

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
  designcheck <- checkModelDesign(modeldata, createDummies = TRUE)
  if (length(names(designcheck)) == 0) {
    return(designcheck)
  }

  newmodeldata <- designcheck$modeldata
  print(designcheck$warningmessage)
  if (length(designcheck$warningmessage) > 0) {
    print(designcheck$warningmessage)
  }
  if (length(designcheck$errormessage) > 0) {
    print(designcheck$errormessage)
    return(NULL)
  }

  # readjust exposure and adjustment covariates
  col.adj <-
    match(newmodeldata[["acovs"]], names(newmodeldata[["gdta"]]))
  col.ccovar <-
    match(newmodeldata[["ccovs"]], names(newmodeldata[["gdta"]]))
  col.rcovar <-
    match(newmodeldata[["rcovs"]], names(newmodeldata[["gdta"]]))


  if (length(col.adj) == 0) {
    print("running unadjusted")

    # Check model design
    #    designcheck <- checkModelDesign(modeldata,createDummies=FALSE)
    #    if(length(names(designcheck))==0) {
    #        return(designcheck)
    #    }


    data <- newmodeldata$gdta[, c(col.rcovar, col.ccovar)]
    # calculate unadjusted spearman correlation matrix
    #       names(data)<-paste0("v",1:length(names(data)))
    #    assign('gdata',data,envir=.GlobalEnv)

    corrhm <- Hmisc::rcorr(as.matrix(data), type = "spearman")

    corr <-
      data.frame(corrhm$r[1:length(col.rcovar), -(1:length(col.rcovar))])
    n <-
      as.data.frame(corrhm$n[1:length(col.rcovar), -(1:length(col.rcovar))])
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

    colnames(corr) <- colnames(corrhm$r)[-(1:length(col.rcovar))]
    # Fix rownames when only one outcome is considered:
    if (length(col.rcovar) == 1) {
      rownames(corr) <- colnames(corrhm$r)[(1:length(col.rcovar))]
    }
    colnames(n) <- colnames(corrhm$n)[-(1:length(col.rcovar))]
    colnames(pval) <- colnames(corrhm$P)[-(1:length(col.rcovar))]


    # If there are more than one exposure, then need to transpose - not sure why???
    if (length(col.ccovar) > 1 && length(col.rcovar) == 1) {
      corr = as.data.frame(t(corr))
      pval = as.data.frame(t(pval))
    }

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
      print(newmodeldata$rcovs[i])
      for (j in 1:length(newmodeldata$ccovs)) {
        temp <- ppcor::pcor.test(newmodeldata$gdta[, newmodeldata$rcovs[i]],
                                 newmodeldata$gdta[, newmodeldata$ccovs[j]],
                                 newmodeldata$gdta[, newmodeldata$acovs],
                                 method = "spearman")
        pval[i, j] <- round(temp$p.value, digits = 20)
        corr[i, j] <- round(temp$estimate, digits = 20)
        n [i, j] <- temp$n
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
  corrlong <-
    fixData(
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
    dplyr::select(-exposuren,-exposurep)

  # patch in metabolite info for exposure or outcome by metabolite id  ------------------------
  # Add in metabolite information for outcome
  # look in metabolite metadata match by metabolite id
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
    dplyr::mutate(outcome = ifelse(!is.na(outmetname), outmetname, outcomespec)) %>%
    dplyr::select(-outmetname)


  # Add in metabolite information and exposure labels:
  # look in metabolite metadata
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
    dplyr::mutate(exposure = ifelse(!is.na(expmetname), expmetname, modeldata$ccovs)) %>%
    dplyr::select(-expmetname)




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
    # fill in outcome vars from varmap
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


    # fill in exposure vars from varmap
    corrlong <-
      dplyr::left_join(corrlong, vmap, by = c("exposurespec" = "cohortvariable")) %>%
      dplyr::mutate(
        exposure_uid = ifelse(!is.na(varreference), varreference, exposurespec),
        exposure = ifelse(!is.na(vardefinition), vardefinition, exposurespec)
      ) %>%
      dplyr::select(-vardefinition, -varreference)
  }
  else if (modeldata$modelspec == "Batch") {
    # fill in outcome vars from varmap
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


    # fill in exposure vars from varmap
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

  # Stop the clock
  #  ptm <- base::proc.time() - ptm
  #  print(paste("My ptm:", ptm))
  #  attr(corrlong,"ptime") = paste("Processing time:",round(ptm[3],digits=6),"sec")

  return(corrlong)


}


#---------------------------------------------------------
# runCorr: stratified correlation analysis -------------
#' Calculate correlation matrix for each strata specified if stratification is specified in the model tab or in interactive mode
#'
#' @param modeldata list from function getModelData
#' @param metabdata metabolite data list
#' @param cohort cohort label (e.g DPP, NCI, Shanghai)
#'
#' @return data frame with each row representing the correlation for each combination of outcomes and exposures represented as specified in the
#' model (*spec), label (*lab), and universal id (*_uid)
#' with additional columns for n, pvalue, method of model specification (Interactive or Batch), universal id for outcomes (outcome_uid) and exposures (exposure_uid)
#' name of the cohort, adjustment (adjvars) and stratification (stratavar,strata)  variables. Attribute of dataframe includes ptime for processing time of model
#' run.
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modlabel="1 Gender adjusted",
#' 	rowvars=c("lactose","lactate"))
#' corrmatrix <- runCorr(modeldata,exmetabdata, "DPP")
#' @export
runCorr <- function(modeldata, metabdata, cohort = "") {
  # start the clock
  if (nrow(modeldata$gdta) == 0) {
    warning("The number of samples for this model is zero so the model will not be run")
    scorr <- data.frame()
    attr(scorr, "ptime") <-
      "No time elapsed because model cannot run (no samples are input with given criteria)"
    return(scorr)
  }  else{
    ptm <- base::proc.time() # start processing time

    if (is.null(modeldata$scovs)) {
      # drop unused levels for analyses
      for (mycol in colnames(modeldata$gdta)) {
        if (length(levels(modeldata$gdta[, mycol])) > 0) {
          modeldata$gdta[, mycol] = droplevels(modeldata$gdta[, mycol])
        } else {
          next
        }
      }
      scorr <- calcCorr(modeldata, metabdata, cohort = cohort)
    }  else {
      # initialize to avoid globalv errors
      stratlist = holdmod = holdcorr = scorr = NULL

      stratlist <- unique(modeldata$gdta[, modeldata$scovs])

      # Gross check to see whether the stratification variable may not be categorical
      if (length(stratlist) > 10) {
        stop(
          paste(
            "The stratification variable ",
            modeldata$scovs,
            " contains more than 10 unique values, which is too many for our software.  Please check your stratification variable"
          )
        )
      }
      for (i in seq(along = stratlist)) {
        print(paste("Running analysis on subjects stratified by ",modeldata$scovs," ", stratlist[i]))
        holdmod <- modeldata
        holdmod[[1]] <-
          dplyr::filter_(modeldata$gdta,
                         paste(modeldata$scovs, " == ", stratlist[i])) %>%
          dplyr::select(-dplyr::one_of(modeldata$scovs))
        print(dim(holdmod$gdta))
        # Need to reset levels in case one of the levels is dropped (e.g. due to stratification)
        for (mycol in colnames(holdmod$gdta)) {
          if (length(levels(holdmod$gdta[, mycol])) > 0) {
            holdmod$gdta[, mycol] = droplevels(holdmod$gdta[, mycol])
          } else {
            next
          }
        }

        holdcorr  <- calcCorr(holdmod, metabdata, cohort = cohort)
        if (length(holdcorr) != 0) {
          holdcorr$stratavar <- as.character(modeldata$scovs)
          holdcorr$strata <- stratlist[i]
          #scorr<-dplyr::bind_rows(scorr,holdcorr)
        }    else {
          warning(
            paste(
              "Warning: strata (",
              as.character(modeldata$scovs),
              "=",
              stratlist[i],
              " could not be run because model check failed",
              sep = ""
            )
          )
        }
        scorr <- dplyr::bind_rows(scorr, holdcorr)

      } # end for loop
    } # end else run stratified analysis


    if (is.null(scorr)) {
      scorr <- data.frame()
    }

    # Stop the clock
    ptm <- base::proc.time() - ptm
    attr(scorr, "ptime") = paste("Processing time:", round(ptm[3], digits =
                                                             3), "sec")
    #print(paste0("Processing time: ",ptm))
    return(scorr)
  }
}
