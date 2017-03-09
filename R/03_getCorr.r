#---------------------------------------------------------
# Get correlation matrix -----------------------------------------
#---------------------------------------------------------
#' Calculate correlation matrix
#'
#' @param modeldata list from function getModelData
#' @param metabdata metabolite data list
#' @param cohort cohort label (e.g DPP, NCI, Shanghai)
#'
#' @return data frame with each row representing the correlation for each combination of outcomes and exposures with additional columns for n, pvalue, metabolite_id, method of model specification (Interactive or Batch), name of the cohort and adjustment variables. Attribute of dataframe includes ptime for processing time of model run.
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modbatch="1.1 Unadjusted")
#' corrmatrix <-getCorr(modeldata,exmetabdata, "DPP")
#' @export
getCorr <- function (modeldata,metabdata,cohort=""){
  # Defining global variables to pass Rcheck()
  ptm <- proc.time() # start processing time
  metabid=uid_01=biochemical=outmetname=outcomespec=exposuren=exposurep=metabolite_id=c()
  metabolite_name=expmetname=exposurespec=c()

  # column indices of row covariates
  col.rcovar <- match(modeldata$rcovs,names(modeldata$gdta))

  # column indices of column covariates
  col.ccovar <- match(modeldata$ccovs,names(modeldata$gdta))

  # column indices of adj-var
  col.adj <- match(modeldata$acovs,names(modeldata$gdta))

  # Defining global variable to remove R check warnings
  corr=c()

  if (length(col.adj)==0) {
    print("running unadjusted")
    data<-modeldata[[1]][,c(col.rcovar,col.ccovar)]
    # calculate unadjusted spearman correlation matrix
    #       names(data)<-paste0("v",1:length(names(data)))
    #    assign('gdata',data,envir=.GlobalEnv)
    corr<-stats::cor(data,method = "spearman",use="pairwise.complete.obs")

    corr <- data.frame(corr[1:length(col.rcovar),-(1:length(col.rcovar))])

    # If there are more than one exposure, then need to transpose
    if(length(col.ccovar)>1 && length(col.rcovar)==1) {corr=as.data.frame(t(corr))}

    # calculate complete cases matrix
    n  <-
      matrix(NA,nrow = length(col.rcovar),ncol = length(col.ccovar))
    for (i in 1:length(col.rcovar)) {
      for (j in 1:length(col.ccovar)) {
        n[i,j] <- sum(stats::complete.cases(data[,c(col.rcovar[i],col.ccovar[j])]))
      }
    }
  }
  else {
    # calculate partial correlation matrix
    print("running adjusted")
    #      corr <-devtools::load_all(".")

    #        pcor.test(data[,col.rcovar],data[,col.ccovar], data[,col.adj],method="spearman")
    #dtarank<-as.data.frame(apply(modeldata$gdta[-incommon,2,rank))

    #filter columns with 0 variance
    # take out indices with - variance
    #which(apply(dtarank,2,var)==0,arr.ind = T)

    # Added the unique below so if exposure and outcome are equal, it's not run
    # (partial.r actually throws an error in this case)

    data<-modeldata[[1]][,c(col.adj,col.rcovar,col.ccovar)]
    spearcorr <- stats::cor(data,method = "spearman",use="pairwise.complete.obs")
    # get coordinates for outcomes and exposures for input into partial.r:
    myind=c(match(modeldata$rcovs,colnames(spearcorr)),
	match(modeldata$ccovs,colnames(spearcorr)))
    corr <-psych::partial.r(spearcorr,myind,col.adj)
    # get coordinates of outcomes for output corr:
    xcorr=match(modeldata$rcovs,colnames(corr))
    ycorr=match(modeldata$ccovs,colnames(corr))
    corr=as.data.frame(corr[xcorr,ycorr])

#    corr<-as.data.frame(corr[1:length(col.rcovar),-(1:length(col.rcovar))])
    # If there are more than one exposure, then need to transpose
    if(length(col.ccovar)>1 && length(col.rcovar)==1) {corr=as.data.frame(t(corr))}

#    colnames(corr)=names(dtarank)[col.rcovar]

    #corr <-corr.p(data,c(col.rcovar,col.ccovar), col.adj,method="spearman")
    #corr<-corr$estimate[1:length(col.rcovar),-(1:length(col.rcovar))]
    # calculate complete cases matrix
    n  <-
      matrix(NA,nrow = length(col.rcovar),ncol = length(col.ccovar))
    for (i in 1:length(col.rcovar)) {
      for (j in 1:length(col.ccovar)) {
        n[i,j] <- sum(stats::complete.cases(data[,c(col.rcovar[i],col.ccovar[j],col.adj)]))
      }
    }
#   corr=cbind(corr,n)

  } # End else adjusted mode (length(col.adj) is not zero)

  # need to explicitely get rownmames if there's only one row
  if(nrow(corr)==1) {rownames(corr)=as.character(modeldata[[3]])}

  colnames(corr) <- as.character(modeldata[[2]])
  rownames(corr) <- as.character(modeldata[[3]])
  colnames(n) <- paste(as.character(modeldata[[2]]),".n",sep = "")
  ttval<-sqrt(n-length(col.adj)-2)*corr/sqrt(1-corr**2)
  pval<-stats::pt(as.matrix(abs(ttval)),df=n-length(col.adj)-2,lower.tail=FALSE)*2
  colnames(pval) <- paste(as.character(modeldata[[2]]),".p",sep = "")

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
  corrlong<-dplyr::left_join(corrlong,
                             dplyr::select(metabdata$metab,metabid,outcome_uid=uid_01,outmetname=biochemical),
                             by=c("outcomespec"=metabdata$metabId)) %>%
           dplyr::mutate(outcome=ifelse(!is.na(outmetname),outmetname,outcomespec)) %>%
           dplyr::select(-outmetname)

  # Add in metabolite information and exposure labels:
  corrlong<-dplyr::left_join(corrlong,
                             dplyr::select(metabdata$metab,metabid,exposure_uid=uid_01,expmetname=metabolite_name),
                             by=c("exposurespec"=metabdata$metabId)) %>%
    dplyr::mutate(exposure=ifelse(!is.na(expmetname),expmetname,exposurespec)) %>%
    dplyr::select(-expmetname)



  #corrlong <- dplyr::select(inner_join(corrlong,metabdata$metab,by=c("metabolite_id"=metabdata$metabId)),-metabolite_id)
  # Stop the clock
  ptm <- proc.time() - ptm
  attr(corrlong,"ptime") = paste("Processing time:",round(ptm[3],digits=6),"sec")

return(corrlong)
}


#---------------------------------------------------------
# showHeatmap -----------------------------------------

