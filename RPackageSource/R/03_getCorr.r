#---------------------------------------------------------
#' Calculate correlation matrix for input model. This function will check for strata and if present, it will run the correlation matrix within each strata. Each model design is checked for validity (correlation between predictors, zero variance, etc.).
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
