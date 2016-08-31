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
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,colvars="age",modbatch="1.1 Unadjusted")
#' corrmatrix <-getCorr(modeldata,exmetabdata, "DPP")
#' @export
getCorr <- function (modeldata,metabdata,cohort=""){
  # Defining global variables to pass Rcheck()
  exposuren=exposurep=metabolite_id=c()

  # get correlation matrix
  col.rcovar <- match(modeldata[[3]],names(modeldata[[1]]))

  # column indices of column covariates
  col.ccovar <- match(modeldata[[2]],names(modeldata[[1]]))

  # column indices of adj-var
  col.adj <- match(modeldata[[4]],names(modeldata[[1]]))

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
    dtarank<-as.data.frame(apply(modeldata[[1]],2,rank))

    #filter columns with 0 variance
    # take out indices with - variance
    #which(apply(dtarank,2,var)==0,arr.ind = T)


    corr <-psych::partial.r(dtarank,c(col.rcovar,col.ccovar),col.adj)
    corr<-as.data.frame(corr[1:length(col.rcovar),-(1:length(col.rcovar))])
    # If there are more than one exposure, then need to transpose
    if(length(col.ccovar)>1 && length(col.rcovar)==1) {corr=as.data.frame(t(corr))}

    #corr <-corr.p(data,c(col.rcovar,col.ccovar), col.adj,method="spearman")
    #corr<-corr$estimate[1:length(col.rcovar),-(1:length(col.rcovar))]
    # calculate complete cases matrix
    n  <-
      matrix(NA,nrow = length(col.rcovar),ncol = length(col.ccovar))
    n  <-
      matrix(NA,nrow = length(col.rcovar),ncol = length(col.ccovar))
    for (i in 1:length(col.rcovar)) {
      for (j in 1:length(col.ccovar)) {
        n[i,j] <- sum(stats::complete.cases(dtarank[,c(col.rcovar[i],col.ccovar[j],col.adj)]))
      }
    }

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
      tidyr::gather(cbind(corr, metabolite_name = rownames(corr)),
                    "exposure","corr",1:length(col.ccovar)
      ),
      tidyr::gather(as.data.frame(n),"exposuren", "n", 1:length(col.ccovar)),
      tidyr::gather(as.data.frame(pval),"exposurep","pvalue",1:length(col.ccovar)),
      cohort = cohort,
      adjvars = ifelse(length(col.adj) == 0, "None", paste(modeldata[[4]], collapse = " ")) )) %>% select(-exposuren, -exposurep)

  #corrlong <- dplyr::select(inner_join(corrlong,metabdata$metab,by=c("metabolite_id"=metabdata$metabId)),-metabolite_id)

return(corrlong)
}

#---------------------------------------------------------
# showCorr
#---------------------------------------------------------
#' Function that returns top N lines of the getCorr() output
#' @param corr COMETScorr class (S3) from getCorr() output
#' @param nlines number of lines to return (default 50)
#' @return first 50 lines of output
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,modbatch="1.1 Unadjusted")
#' corrmatrix <-getCorr(modeldata,exmetabdata,"DPP")
#' showCorr(corrmatrix)
#' @export 
showCorr <- function(corr, nlines=50) {
	return(utils::head(as.data.frame(corr),nlines))
}

#---------------------------------------------------------
# showHeatmap -----------------------------------------
#---------------------------------------------------------
#' Show interactive heatmap using plot_ly
#'
#' @param ccorrmat correlation matrix (output of getCorr())
#' @param rowsortby How row labels are sorted
#' @param plothgt Plot height default 700
#' @param plotwid Plot width default 800
#' @param colscale colorscale, can be custom or named ("Hots","Greens","Blues","Greys","Purples") see \url{https://plot.ly/ipython-notebooks/color-scales/}
#'
#' @return a heatmap with outcomes as rows and exposures in columns.
#'
#' @references For colorscale reference: \url{https://plot.ly/ipython-notebooks/color-scales/}
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,modbatch="1.1 Unadjusted")
#' corrmatrix <-getCorr(modeldata,exmetabdata,"DPP")
#' showHeatmap(corrmatrix)
#' @export

showHeatmap <- function (ccorrmat, 
       rowsortby = "corr",
       plothgt=700,
       plotwid=800,
       colscale="RdYlBu") {

  exmetabdata=corr=exposure=metabolite_name=c()

  # order the rows according to sort by
  if (rowsortby == "metasc") {exmodeldata <- COMETS::getModelData(exmetabdata,modbatch="1.1 Unadjusted")

    ccorrmat$metabolite_name <- suppressWarnings(
      factor(ccorrmat$metabolite_name, levels =
               ccorrmat$metabolite_name[rev(order(unlist(ccorrmat["metabolite_name"])))]))
  } else {
    ccorrmat$metabolite_name <- suppressWarnings(
      factor(ccorrmat$metabolite_name, levels = ccorrmat$metabolite_name[order(unlist(ccorrmat[rowsortby]))]))
  }

  # stack the correlations together
  ccorrmat <- ccorrmat[order(ccorrmat$metabolite_name),]
  # Number of columns identified by suffix of .n

  # plotly will not plot if there is only one row (so quick fix is to duplicate data)
  if(nrow(ccorrmat)==1) {
	ccorrmat=rbind(ccorrmat,ccorrmat)
  }

  ccorrmat %>%
  plotly::plot_ly(z = signif(corr),x = exposure, y = metabolite_name,
          type = "heatmap",
          colorscale=colscale,
          colorbar = list(title = "Correlation")) %>%
  plotly::layout(height=plothgt,
         width=plotwid,
         margin = list(l = 200),
         title = " ",      # layout's title: /r/reference/#layout-title
         xaxis = list(           # layout's xaxis is a named list.
           title = " ",       # xaxis's title: /r/reference/#layout-xaxis-title
           showgrid = F,          # xaxis's showgrid: /r/reference/#layout-xaxis
           ticks=""
         ),
         yaxis = list(           # layout's yaxis is a named list.
           title = " ",        # yaxis's title: /r/reference/#layout-yaxis-title,
           ticks=""
         )
         ,
         legend = list(           # layout's yaxis is a named list.
           title = "Correlation"        # yaxis's title: /r/reference/#layout-yaxis-title
         )
         ,
         autosize = TRUE)
}





#---------------------------------------------------------
# showHClust -----------------------------------------
#---------------------------------------------------------
#' Show interactive heatmap using d3heatmap with hierarchical clustering
#'
#' @description
#' This function outputs a heatmap with hierarchical clustering.  It thus requires you to have at least 2 outcome and 2 exposure variables in your models.
#'
#' @param ccorrmat correlation matrix
#' @param clust Show hierarchical clustering
#' @param colscale colorscale, can be custom or named ("Hots","Greens","Blues","Greys","Purples") see \url{https://plot.ly/ipython-notebooks/color-scales/}
#'
#' @return a heatmap with outcomes as rows and exposures in columns.
#'
#' @references For colorscale reference: \url{https://plot.ly/ipython-notebooks/color-scales/}
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata, modelspec="Interactive",colvars=c("age","bmi"))
#' corrmatrix <-getCorr(modeldata,exmetabdata,"DPP")
#' showHClust(corrmatrix)
#' @export
showHClust <- function (ccorrmat,
                        clust = TRUE,
                        colscale = "RdYlBu") {
 metabolite_name=exposure=corr=c()
  excorr <-
    ccorrmat %>% dplyr::select(metabolite_name, exposure, corr) %>% tidyr::spread(exposure, corr)
  rownames(excorr) <- excorr[, 1]

  ncols <- ncol(excorr)
  if(ncols <= 2) 
	stop("Cannot run heatmap because there is only one exposure variable")
  d3heatmap::d3heatmap(excorr[, 2:ncols],
            colors = colscale,
            dendrogram = if (clust)
              "both"
            else
              "none")
}
