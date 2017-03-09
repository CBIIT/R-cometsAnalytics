#--------------------------------------------------------
# getCorr: get lmelation matrix ------------------------
#---------------------------------------------------------
#' Calculate lmelation matrix
#'
#' @param modeldata list from function getModelData
#' @param metabdata metabolite data list
#' @param cohort cohort label (e.g DPP, NCI, Shanghai)
#'
#' @return data frame 
#'
# #' @examples
# #' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
# #' csvfile <- file.path(dir, "cometsInputAge.xlsx")
# #' exmetabdata <- readCOMETSinput(csvfile)
# #' modeldata <- getModelData(exmetabdata,colvars="age",modbatch="1.1 Unadjusted")
# #' lmmatrix <- getLM(modeldata,exmetabdata, "DPP")


#source("http://silicovore.com/OOMPA/oompaLite.R")
#oompaLite()
#library(ClassComparison)


getLM <- function (modeldata,metabdata,cohort=""){
  # Defining global variables to pass Rcheck()
  ptm <- proc.time() # start processing time
  
  # column indices of row/outcome covariates
  col.rcovar <- match(modeldata[["rcovs"]],names(modeldata[["gdta"]]))

  # column indices of column/exposure covariates
  col.ccovar <- match(modeldata[["ccovs"]],names(modeldata[["gdta"]]))

  # column indices of adj-var
  col.adj <- match(modeldata[["acovs"]],names(modeldata[["gdta"]]))

  # Defining global variable to remove R check warnings
  lm=c()

  if (length(col.adj)==0) {
    print("running unadjusted")
    data<-modeldata[[1]][,c(col.rcovar,col.ccovar)]
   
    exposure <- data.frame(modeldata[['gdta']][,c(col.ccovar)])
    colnames(exposure) <- modeldata$ccovs 
    mlm <- ClassComparison::MultiLinearModel(form = Y ~ .,
             clindata = exposure, 
             arraydata = t(modeldata[['gdta']][,c(col.rcovar)]))

    coef <- data.frame(mlm@coefficients[colnames(exposure),])
    colnames(coef)=paste0(modeldata$ccovs,"_Beta")
    pval <- data.frame(mlm@p.values)
    colnames(pval)=paste0(modeldata$ccovs,"_pval")
  }
  else {
    # calculate partial lmelation matrix
    print("running adjusted")

    exposure <= data.frame(modeldata[['gdta']][,c(col.ccovar,col.adj)])
    mlm <- ClassComparison::MultiLinearModel(form = Y ~ .,
             clindata = exposure,
             arraydata = t(modeldata[['gdta']][,c(col.rcovar)]))

    coef <- data.frame(t(mlm@coefficients[-1,]))
    pval <- data.frame(mlm@p.values)
    colnames(pval)=paste0(modeldata$ccovs,"_pval")
  } # End else adjusted mode

  lmres <- cbind(coef,pval)

  # Stop the clock
  ptm <- proc.time() - ptm
  attr(lmlong,"ptime") = paste("Processing time:",round(ptm[3],digits=6),"sec")

return(lmres)
}


