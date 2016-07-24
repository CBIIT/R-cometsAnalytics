#---------------------------------------------------------
# Get correlation matrix -----------------------------------------
#---------------------------------------------------------
#' Calculate correlation matrix
#'
#' @param modeldata list from function getModelData
#' @param metabdata metabolite data list
#' @param cohort cohort label (e.g DPP, NCI, Shanghai)
#'
#' @return a correlation matrix with outcomes as rows and exposures in columns with additional columns for n, pvalue, metabolite_id, method of model specification (Interactive or Batch), name of the cohort and adjustment variables.
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' modeldata <- getModelData(readCSV(csvfile))
#' corrmatrix <-getCorr(modeldata)
#' @export
getCorr <- function (modeldata,metabdata,cohort=""){
  # get correlation matrix
  col.rcovar <- match(modeldata[[3]],names(modeldata[[1]]))

  # column indices of column covariates
  col.ccovar <- match(modeldata[[2]],names(modeldata[[1]]))


  # column indices of adj-var
  col.adj <- match(modeldata[[4]],names(modeldata[[1]]))

  if (length(col.adj)==0) {
    print("running unadjusted")
    data<-modeldata[[1]][,c(col.rcovar,col.ccovar)]
    # calculate unadjusted spearman correlation matrix
    #       names(data)<-paste0("v",1:length(names(data)))
    #    assign('gdata',data,envir=.GlobalEnv)
    corr<-cor(data,method = "spearman",use="pairwise.complete.obs")

    corr <- data.frame(corr[1:length(col.rcovar),-(1:length(col.rcovar))])
    # calculate complete cases matrix
    n  <-
      matrix(NA,nrow = length(col.rcovar),ncol = length(col.ccovar))
    for (i in 1:length(col.rcovar)) {
      for (j in 1:length(col.ccovar)) {
        n[i,j] <- sum(complete.cases(data[,c(col.rcovar[i],col.ccovar[j])]))
      }
    }
  }
  else {
    # calculate partial correlation matrix
    print("running adjusted")
    #      corr <-devtools::load_all(".")

    #        pcor.test(data[,col.rcovar],data[,col.ccovar], data[,col.adj],method="spearman")
    dtarank<-as.data.frame(apply(modeldata[[1]],2,rank))

    #filter columns with 0 variance
    # take out indices with - variance
    #which(apply(dtarank,2,var)==0,arr.ind = T)


    corr <-psych::partial.r(dtarank,c(col.rcovar,col.ccovar),col.adj)
    corr<-as.data.frame(corr[1:length(col.rcovar),-(1:length(col.rcovar))])
    #corr <-corr.p(data,c(col.rcovar,col.ccovar), col.adj,method="spearman")
    #corr<-corr$estimate[1:length(col.rcovar),-(1:length(col.rcovar))]
    # calculate complete cases matrix
    n  <-
      matrix(NA,nrow = length(col.rcovar),ncol = length(col.ccovar))
    n  <-
      matrix(NA,nrow = length(col.rcovar),ncol = length(col.ccovar))
    for (i in 1:length(col.rcovar)) {
      for (j in 1:length(col.ccovar)) {
        n[i,j] <- sum(complete.cases(dtarank[,c(col.rcovar[i],col.ccovar[j],col.adj)]))
      }
    }

  }
  colnames(corr) <- as.character(modeldata[[2]])
  colnames(n) <- paste(as.character(modeldata[[2]]),".n",sep = "")
  ttval<-sqrt(n-length(col.adj)-2)*corr/sqrt(1-corr**2)
  pval<-pt(as.matrix(abs(ttval)),df=n-length(col.adj)-2,lower.tail=FALSE)*2
  colnames(pval) <- paste(as.character(modeldata[[2]]),".p",sep = "")


  # combine the two matrices together as data frame
  corr <- fixData(data.frame(round(corr,digits=3),
                             n,
                             pval,
                             metabolite_id=rownames(ttval),
                             cohort=cohort,
                             adjvars=ifelse(length(col.adj)==0,"None",paste(modeldata[[4]],collapse = " "))))



  ccorrmat <- dplyr::select(inner_join(corr,metabdata$metab,by=c("metabolite_id"=metabdata$metabId)),-metabolite_id)
}








#---------------------------------------------------------
# showHeatmap -----------------------------------------
#---------------------------------------------------------
#' Show interactive heatmap using plot_ly
#'
#' @param ccorrmat correlation matrix
#' @param rowsortby How row labels are sorted
#'
#' @return a heatmap with outcomes as rows and exposures in columns.
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' modeldata <- getModelData(readCSV(csvfile))
#' corrmatrix <-getCorr(modeldata)
#' @export

showHeatmap <- function (ccorrmat, rowsortby = "metasc",plothgt=700) {
  # order the rows according to sort by
  if (rowsortby == "metasc") {
    ccorrmat$metabolite_name <-
      factor(ccorrmat$metabolite_name, levels =
               ccorrmat$metabolite_name[rev(order(unlist(ccorrmat["metabolite_name"])))])
  } else {
    ccorrmat$metabolite_name <-
      factor(ccorrmat$metabolite_name, levels = ccorrmat$metabolite_name[order(unlist(ccorrmat[rowsortby]))])
  }

  # stack the correlations together
  ccorrmat <- ccorrmat[order(ccorrmat$metabolite_name),]
  # Number of columns identified by suffix of .n

tidyr::gather(ccorrmat,"covariate","corr",1:length(grep("\\.n$",names(ccorrmat))))%>%
  plotly::plot_ly(z = corr,
          x = covariate, y = metabolite_name,
          type = "heatmap",
          colorbar = list(title = "Correlation")) %>%
  plotly::layout(height=plothgt,
         margin = list(l = 200),
         title = " ",      # layout's title: /r/reference/#layout-title
         xaxis = list(           # layout's xaxis is a named list.
           title = " ",       # xaxis's title: /r/reference/#layout-xaxis-title
           showgrid = F          # xaxis's showgrid: /r/reference/#layout-xaxis
         ),
         yaxis = list(           # layout's yaxis is a named list.
           title = " "        # yaxis's title: /r/reference/#layout-yaxis-title
         )
         ,
         legend = list(           # layout's yaxis is a named list.
           title = "Correlation"        # yaxis's title: /r/reference/#layout-yaxis-title
         )
         ,
         autosize = TRUE)
}
