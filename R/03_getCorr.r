#---------------------------------------------------------
# getCorr: get correlation matrix ------------------------
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
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modbatch="1.1 Unadjusted")
#' corrmatrix <-getCorr(modeldata,exmetabdata, "DPP")
#' @export
getCorr <- function (modeldata,metabdata,cohort=""){
  # Defining global variables to pass Rcheck()
  ptm <- proc.time() # start processing time
  metabid=uid_01=biochemical=outmetname=outcomespec=exposuren=exposurep=metabolite_id=c()
  metabolite_name=expmetname=exposurespec=c()

  # column indices of row/outcome covariates
  col.rcovar <- match(modeldata[["rcovs"]],names(modeldata[["gdta"]]))

  # column indices of column/exposure covariates
  col.ccovar <- match(modeldata[["ccovs"]],names(modeldata[["gdta"]]))

  # column indices of adj-var
  col.adj <- match(modeldata[["acovs"]],names(modeldata[["gdta"]]))

  # Defining global variable to remove R check warnings
  corr=c()

  if (length(col.adj)==0) {
    print("running unadjusted")
    data<-modeldata[[1]][,c(col.rcovar,col.ccovar)]
    # calculate unadjusted spearman correlation matrix
    #       names(data)<-paste0("v",1:length(names(data)))
    #    assign('gdata',data,envir=.GlobalEnv)

    corrhm<-Hmisc::rcorr(as.matrix(data),type = "spearman")

    corr <- data.frame(corrhm$r[1:length(col.rcovar),-(1:length(col.rcovar))])
    n <- as.data.frame(corrhm$n[1:length(col.rcovar),-(1:length(col.rcovar))])
    pval <- as.data.frame(corrhm$P[1:length(col.rcovar),-(1:length(col.rcovar))])

    colnames(corr)<-colnames(corrhm$r)[-(1:length(col.rcovar))]
    colnames(n)<-colnames(corrhm$n)[-(1:length(col.rcovar))]
    colnames(pval)<-colnames(corrhm$P)[-(1:length(col.rcovar))]


    # If there are more than one exposure, then need to transpose - not sure why???
    if(length(col.ccovar)>1 && length(col.rcovar)==1) {
      corr=as.data.frame(t(corr))
      pval=as.data.frame(t(pval))
    }

  }
  else {
    # calculate partial correlation matrix
    print("running adjusted")

    data<-modeldata[[1]][,c(col.adj,col.rcovar,col.ccovar)]
    spearcorr <- Hmisc::rcorr(as.matrix(data),type = "spearman")

    # get coordinates for outcomes and exposures for input into partial.r:
    myind=c(match(modeldata$rcovs,colnames(spearcorr$r)),
	  match(modeldata$ccovs,colnames(spearcorr$r)))
    corr <-psych::partial.r(spearcorr$r,myind,col.adj)
    # get coordinates of outcomes for output corr:
    xcorr=match(modeldata$rcovs,colnames(corr))
    ycorr=match(modeldata$ccovs,colnames(corr))
    corr=as.data.frame(corr[xcorr,ycorr])

    n <- data.frame(spearcorr$n[xcorr,ycorr])

    ttval<-sqrt(n-length(col.adj)-2)*corr/sqrt(1-corr**2)
    pval<-stats::pt(as.matrix(abs(ttval)),df=n-length(col.adj)-2,lower.tail=FALSE)*2
    colnames(pval) <- paste(as.character(modeldata[[2]]),".p",sep = "")

    # If there are more than one exposure, then need to transpose - not sure why???
    #if(length(col.ccovar)>1 && length(col.rcovar)==1) {corr=as.data.frame(t(corr))}

#   corr=cbind(corr,n)

  } # End else adjusted mode (length(col.adj) is not zero)

  corrlong <-
    fixData(data.frame(
      tidyr::gather(cbind(corr, outcomespec = rownames(corr)),
                    "exposurespec","corr",1:length(col.ccovar)
      ),
      tidyr::gather(as.data.frame(n),"exposuren", "n", 1:length(col.ccovar)),
      tidyr::gather(as.data.frame(pval),"exposurep","pvalue",1:length(col.ccovar)),
      cohort = cohort,
      adjvars = ifelse(length(col.adj) == 0, "None", paste(modeldata[[4]], collapse = " ")) )) %>%
    dplyr::select(-exposuren, -exposurep)

  # Add in metabolite information and outcome labels:
  # look in metabolite metadata
  corrlong<-dplyr::left_join(corrlong,
                             dplyr::select(metabdata$metab,metabid,outcome_uid=uid_01,outmetname=biochemical),
                             by=c("outcomespec"=metabdata$metabId)) %>%
           dplyr::mutate(outcome=ifelse(!is.na(outmetname),outmetname,outcomespec)) %>%
           dplyr::select(-outmetname)
  # look in varmap
  vmap<-dplyr::select(metabdata$vmap,cohortvariable,vardefinition,varreference) %>%
    mutate(cohortvariable=tolower(cohortvariable))

  # fill in outcome var from varmap
  corrlong<-dplyr::left_join(corrlong,vmap,by=c("outcomespec"="cohortvariable")) %>%
    dplyr::mutate(outcome=ifelse(!is.na(vardefinition),vardefinition,outcome),
                  outcome_uid=ifelse(!is.na(varreference),varreference,outcome_uid)) %>%
    dplyr::select(-vardefinition,-varreference)

  # fill in exposure var from varmap
  corrlong<-dplyr::left_join(corrlong,vmap,by=c("exposurespec"="cohortvariable")) %>%
    dplyr::mutate(exposure=ifelse(!is.na(vardefinition),vardefinition,exposure),
                  exposure_uid=ifelse(!is.na(varreference),varreference,exposure_uid)) %>%
    dplyr::select(-vardefinition,-varreference)


  # fill in exposure from varmap
  corrlong<-dplyr::left_join(corrlong,
                             dplyr::select(metabdata$metab,metabid,uid_01,expmetname=metabolite_name),
                             by=c("exposurespec"=metabdata$metabId)) %>%
    dplyr::mutate(exposure=ifelse(!is.na(expmetname),expmetname,exposure),
                  exposure_uid=ifelse(!is.na(expmetname),uid_01,exposure_uid)) %>%
    dplyr::select(-expmetname,-uid_01)

  # add in cohort reference variable info to output file.



  # Stop the clock
  ptm <- proc.time() - ptm
  attr(corrlong,"ptime") = paste("Processing time:",round(ptm[3],digits=6),"sec")

return(corrlong)
}


#---------------------------------------------------------
# stratCorr: stratified correlation analysis -------------
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
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modbatch="1.1 Unadjusted")
#' corrmatrix <-getCorr(modeldata,exmetabdata, "DPP")
#' @export
stratCorr<- function(modeldata,metabdata,cohort=""){
  # start the clock
  ptm <- proc.time() # start processing time

  # initialize to avoid globalv errors
  stratlist=holdmod=holdcorr=scorr=NULL

  stratlist <- unique(modeldata$gdta[modeldata$scovs])
  for (i in seq(along=stratlist[,1])) {
    holdmod <- modeldata
    holdmod[[1]] <- dplyr::filter_(modeldata$gdta,paste(modeldata$scovs," == ",stratlist[i,1])) %>%
      select(-dplyr::one_of(modeldata$scovs))
    holdcorr  <- COMETS::getCorr(holdmod,metabdata,cohort=cohort)
    holdcorr$stratavar<-as.character(modeldata$scovs)
    holdcorr$strata<-stratlist[i,1]
    scorr<-dplyr::bind_rows(scorr,holdcorr)
  }
  # Stop the clock
  ptm <- proc.time() - ptm
  attr(scorr,"ptime") = paste("Processing time:",round(ptm[3],digits=6),"sec")

  return(scorr)
}

