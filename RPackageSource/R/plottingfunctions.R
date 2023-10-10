#' Plot the variance distribution of transformed metabolite abundances
#'
#' @param cometsdata output of readCOMETSinput function
#' @param title main title for the plot (default is "Distribution of Variance")
#' @param xlabel x-axis label (default is "Variance of transformed metabolite abundances")
#' @param ylabel y-axis label (default is "Frequency")
#' @param titlesize size of title (default, 20)
#' @param xylabelsize size of x and y labels (default=8)
#'
#' @return a distribution plot
#'
#' @examples
#' dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' plotVar(exmetabdata)
#'
#' @export
plotVar <- function(cometsdata,
                    title = "Distribution of Variance",
                    titlesize=16,
                    xlabel = "Variance of transformed metabolite abundances",
                    ylabel = "Frequency",
                    xylabelsize = 12) {
   if (is.null(cometsdata$metab$var)) {
     stop("The input data is not in the correct format.  Make sure it is the output of the readCOMETSinput function")
   }
   if (titlesize > 16) warning("The plot title may not display properly with titlesize > 16.")

   toplot <- cometsdata$metab$var
   font1 <- list(size=4,color="red")

   p <- plot_ly(
     x=toplot,
     type="histogram") %>%
   layout(title=title,titlefont=list(size=titlesize),
     xaxis = list(title=xlabel,titlefont=list(size=xylabelsize)),
     yaxis = list(title=ylabel,titlefont=list(size=xylabelsize)),
     bargap=0.3)
   return(p)
} 

#' Plot the distribution of the number of missing values for each metabolite
#'
#' @param cometsdata output of readCOMETSinput function
#' @param title main title for the plot (default is "Distribution of the Number/Missing Values")
#' @param xlabel x-axis label (default is "Number of minimum/missing values")
#' @param ylabel y-axis label (default is "Frequency")
#' @param titlesize size of title (default, 20)
#' @param xylabelsize size of x and y labels (default=8)
#'
#' @return a distribution plot
#'
#' @examples
#' dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' plotMinvalues(exmetabdata)
#'
#' @export

plotMinvalues <- function(cometsdata,
                    title = "Distribution of the Number/Missing Values",
                    xlabel = "Number of minimum/missing values",
                    ylabel = "Frequency",
                    xylabelsize = 12,
                    titlesize=16) {

   if(is.null(cometsdata$metab$num.min)) {
        stop("The input data is not in the correct format.  Make sure it is the output of the
            readCOMETSinput function")
   }

   toplot <- cometsdata$metab$num.min
   p <- plot_ly(
     x=toplot,
     type="histogram") %>%
   layout(title=title,titlefont=list(size=titlesize),
     xaxis = list(title=xlabel,titlefont=list(size=xylabelsize)),
     yaxis = list(title=ylabel,titlefont=list(size=xylabelsize)),
     bargap=0.3)
   return(p)
}

#---------------------------------------------------------
# showCorr
#---------------------------------------------------------
#' Function that returns top N lines of the \code{\link{runCorr}} output
#' @param corr  returned object from \code{\link{runCorr}} 
#' @param nlines number of lines to return (default 50)
#' @return first 50 lines of output
#' @examples
#' dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,modlabel="1 Age")
#' corrmatrix <- runCorr(modeldata,exmetabdata,"DPP")
#' showCorr(corrmatrix)
#' @export
showCorr <- function(corr, nlines=50) {

  corr <- get_ccorrmat(corr) 
  if (!length(corr)) stop("ERROR: corr must be an object returned from runCorr")
  
  nr <- min(nrow(corr), nlines)
  if (nr) {
    ret <- corr[1:nr, , drop=FALSE]
  } else {
    ret <- corr
  }

  ret
}

#---------------------------------------------------------
# showModel
#---------------------------------------------------------
#' Function that displays the first N rows of each data frame 
#'  in the the \code{\link{runModel}} output.
#' @param obj  returned object from \code{\link{runModel}} 
#' @param nlines number of lines to display (default 10)
#' @return NULL
#' @examples
#' dir         <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
#' csvfile     <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata   <- getModelData(exmetabdata,modlabel="1 Age")
#' result      <- runModel(modeldata,exmetabdata, cohortLabel="DPP")
#' showModel(result)
#' @export
showModel <- function(obj, nlines=10) {
  
  if (!length(obj)) return(NULL)
  if (is.data.frame(obj) || is.matrix(obj)) obj <- list(output=obj)
  if (is.list(obj)) {
    nms <- names(obj)
    if (!length(nms)) return(NULL)
    for (i in 1:length(nms)) {
      nm  <- nms[i]
      str <- paste0("\n", nm, ":\n")
      cat(str)
      x   <- obj[[nm, exact=TRUE]] 
      if (!is.data.frame(x)) stop("ERROR: obj must be an object returned from runModel")
      nr <- min(nrow(x), nlines)
      if (nr) {
        print(x[1:nr, , drop=FALSE])
      } else {
        print(x) # Could be an empty data frame
      } 
    }
  } else if ("try-error" %in% class(obj)) {
    print(obj)
  } else {
    stop("ERROR: obj must be an object returned from runModel")
  }
  NULL
}

#---------------------------------------------------------
#' Show interactive heatmap using plot_ly
#'
#' @param ccorrList correlation object (output of \code{\link{runCorr}})
#' @param strata Only valid if ccorrList is from a stratified analysis. If NULL,
#'    then results from the first stratum will be used in the plot. 
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
#' \dontrun{
#' dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,modlabel="1 Age")
#' corrmatrix <-runCorr(modeldata,exmetabdata,"DPP")
#' showHeatmap(corrmatrix)
#' }
#' @export

showHeatmap <- function (ccorrList, strata=NULL,
       rowsortby = "estimate",
       plothgt=700,
       plotwid=800,
       colscale="RdYlBu") {

  exmetabdata=corr=exposure=outcome=metabolite_name=c()

  ccorrmat <- get_ccorrmat(ccorrList)
  if (!length(ccorrmat)) stop("ERROR with ccorrList")  
    
  # Check if from a stratified analysis
  ccorrmat <- subset_ccorrmat(ccorrmat, strata, print=1)

  # order the rows according to sort by
  if (rowsortby == "metasc") {
	exmodeldata <- RcometsAnalytics::getModelData(exmetabdata,modlabel="1 Age")

    ccorrmat$exposurespec <- suppressWarnings(
      factor(ccorrmat$exposurespec, levels =
               unique(ccorrmat$exposurespec[rev(order(unlist(ccorrmat["exposurespec"])))])))
  } else {
    ccorrmat$exposurespec <- suppressWarnings(
      factor(ccorrmat$exposurespec, levels = unique(ccorrmat$exposurespec[order(unlist(ccorrmat[rowsortby]))])))
  }

  # stack the correlations together
  ccorrmat <- ccorrmat[order(ccorrmat$exposurespec),]
  # Number of columns identified by suffix of .n

  # plotly will not plot if there is only one row (so quick fix is to duplicate data)
  if(nrow(ccorrmat)==1) {
        ccorrmat=rbind(ccorrmat,ccorrmat)
  }

  corrv <- getCorrEstNameFromDF(ccorrmat)
  plotly::plot_ly(z = signif(ccorrmat[[corrv]],2), x = ccorrmat$exposurespec,
	   y = ccorrmat$outcomespec,
	   type="heatmap", colorscale = colscale,
	   colorbar = list(title = "Correlation"),
	   width=plotwid,
	   height=plothgt) %>%
  plotly::layout(
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
#' Show interactive heatmap using heatmaply_cor with hierarchical clustering
#'
#' @description
#' This function outputs a heatmap with hierarchical clustering.  It thus requires you to have at least 2 outcome and 2 exposure variables in your models.
#'
#' @param ccorrList correlation object (output of \code{\link{runCorr}})
#' @param strata Only valid if ccorrList is from a stratified analysis. 
#'   If NULL, then results from the first stratum will be used in the plot. 
#' @param clust TRUE or FALSE to show hierarchical clustering. The default is TRUE.
#' @param colscale colorscale, can be custom or named ("Hots","Greens","Blues","Greys","Purples") see \code{\link[heatmaply]{RColorBrewer_colors}}
#' @param showticklabels TRUE or FALSE to show axis labels. The default is TRUE.
#'
#' @return a heatmap with outcomes as rows and exposures in columns.
#'
#' @references For colorscale reference: \code{\link[heatmaply]{RColorBrewer_colors}}
#'
#' @examples
#' dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata, modelspec="Interactive",
#'	exposures=c("age","bmi_grp"))
#' corrmatrix <-runCorr(modeldata,exmetabdata,"DPP")
#' showHClust(corrmatrix)
#' @export
showHClust <- function (ccorrList, strata=NULL,
                        clust = TRUE, colscale = "RdYlBu",
                        showticklabels=TRUE) {

  ccorrmat <- get_ccorrmat(ccorrList)
  if (!length(ccorrmat)) stop("ERROR with ccorrList")  
  if (!length(colscale)) colscale <- "RdYlBu"  
  
  # Check if from a stratified analysis and remove missing values
  ccorrmat <- subset_ccorrmat(ccorrmat, strata, print=1)

  # Get the column names we need
  tname <- getEffectsTermName()
  if (!(tname %in% colnames(ccorrmat))) tname <- "exposurespec"
  oname <- getEffectsOutcomespecName()
  #cname <- getEffectsCorrEstName()
  cname <- getCorrEstNameFromDF(ccorrmat)

  # Note, using outcome spec, not outcome because multiple outcomespec can map to 
  # the same outcome (which is the harmonized id)
  excorr <-
    ccorrmat %>% dplyr::select(all_of(oname), all_of(tname), all_of(cname)) %>% 
	tidyr::spread(tname, cname)
  rownames(excorr) <- excorr[, 1]
  ncols <- ncol(excorr)
  if (ncols <= 2) stop("Cannot run heatmap because there is only one exposure variable")
 
  # Get 10 colors
  if (length(colscale) < 2) {
    colors <- rev(eval(parse(text=paste("heatmaply::", colscale, "(10)", sep=""))))
  } else {
    colors <- colscale
  }

  # For dendrogram
  if (clust) {
    dend <- "both"
  } else {
    dend <- "none"
  }

  heatmaply::heatmaply(excorr[, 2:ncols],
            colors=colors, show_grid=FALSE, dendrogram=dend,
            showticklabels=showticklabels)


}

check_strata.number <- function(strata.number) {
  if (length(strata.number) > 1) stop("strata must be length 1")
}

subset_ccorrmat <- function(ccorrmat, strata.number, print=1) {

  sv <- runModel.getStrataColName()
  if (sv %in% colnames(ccorrmat)) {
    check_strata.number(strata.number)
    svn <- runModel.getStrataNumColName()
    if (!length(strata.number)) strata.number <- ccorrmat[1, svn]
    tmp      <- ccorrmat[, svn] %in% strata.number
    ccorrmat <- ccorrmat[tmp, , drop=FALSE]   
    if (!length(ccorrmat)) stop("ERROR with strata")
    if (print) {
      msg <- paste("Displaying plot for stratum: ", ccorrmat[1, sv], "\n", sep="")
      cat(msg)
    }
  }

  corrv    <- getCorrEstNameFromDF(ccorrmat)
  tmp      <- is.finite(ccorrmat[, corrv])
  ccorrmat <- ccorrmat[tmp, , drop=FALSE]
  if (!nrow(ccorrmat)) stop("ERROR: correlation matrix contains all non-finite values")

  ccorrmat
}

getCorrEstNameFromDF <- function(x) {

  cx  <- colnames(x)
  ret <- getEffectsCorrEstName()
  if (ret %in% cx) return(ret)
  ret <- getEffectsCorrEstOldName()
  if (ret %in% cx) return(ret)
  stop("ERROR: correlation column not found")

} # END: getCorrEstNameFromDF

get_ccorrmat <- function(obj) {
  
  ret <- NULL
  if (!length(obj)) return(ret)
  if (is.data.frame(obj) || is.matrix(obj)) {
    ret <- obj
  } else if (is.list(obj)) {
    ret <- obj[[getEffectsName(), exact=TRUE]]
  }
  class(ret) <- "data.frame"

  ret

} 
