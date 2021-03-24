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
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
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
   if(is.null(cometsdata$metab$var)) {
	stop("The input data is not in the correct format.  Make sure it is the output of the 
            readCOMETSinput function")
   }

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
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
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
#' Function that returns top N lines of the runCorr() output
#' @param corr COMETScorr class (S3) from runCorr() output
#' @param nlines number of lines to return (default 50)
#' @return first 50 lines of output
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,modlabel="1 Gender adjusted",
#' 	rowvars=c("lactose","lactate"))
#' corrmatrix <-runCorr(modeldata,exmetabdata,"DPP")
#' showCorr(corrmatrix)
#' @export
showCorr <- function(corr, nlines=50) {
        return(utils::head(as.data.frame(corr),nlines))
}

#---------------------------------------------------------
#' Show interactive heatmap using plot_ly
#'
#' @param ccorrmat correlation matrix (output of runCorr())
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
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,modlabel="1 Gender adjusted",
#' 	rowvars=c("lactose","lactate"))
#' corrmatrix <-runCorr(modeldata,exmetabdata,"DPP")
#' showHeatmap(corrmatrix)
#' }
#' @export

showHeatmap <- function (ccorrmat,
       rowsortby = "corr",
       plothgt=700,
       plotwid=800,
       colscale="RdYlBu") {

  exmetabdata=corr=exposure=outcome=metabolite_name=c()

  # order the rows according to sort by
  if (rowsortby == "metasc") {
	exmodeldata <- COMETS::getModelData(exmetabdata,modlabel="1 Gender adjusted")

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

#  ccorrmat %>%
#  plotly::plot_ly(z = signif(as.numeric(corr)), x = exposure, 
#	  y = metabolite_name,
#          type = "heatmap",
#          colorscale=colscale,
#          colorbar = list(title = "Correlation")) %>%

  plotly::plot_ly(z = data.matrix(signif(ccorrmat$corr,2)), x = ccorrmat$exposure,
	   y = ccorrmat$outcome,
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
#' Show interactive heatmap using heatmaply with hierarchical clustering
#'
#' @description
#' This function outputs a heatmap with hierarchical clustering.  It thus requires you to have at least 2 outcome and 2 exposure variables in your models.
#'
#' @param ccorrmat correlation matrix (output of runCorr())
#' @param clust Show hierarchical clustering
#' @param colscale colorscale, can be custom or named ("Hots","Greens","Blues","Greys","Purples") see \code{\link[heatmaply]{RColorBrewer_colors}}
#'
#' @return a heatmap with outcomes as rows and exposures in columns.
#'
#' @references For colorscale reference: \code{\link[heatmaply]{RColorBrewer_colors}}
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata, modelspec="Interactive",
#'	colvars=c("age","bmi_grp"),rowvars=c("lactose","lactate"))
#' corrmatrix <-runCorr(modeldata,exmetabdata,"DPP")
#' showHClust(corrmatrix)
#' @export
showHClust <- function (ccorrmat,
                        clust = TRUE,
                        colscale = "RdYlBu") {
 outcome=metabolite_name=exposure=corr=outcomespec=c()
# Note, using outcome spec, not outcome because muultiple outcomespec can map to 
# the same outcome (which is the harmonized id)
  excorr <-
    ccorrmat %>% dplyr::select(outcomespec, exposure, corr) %>% 
	tidyr::spread(exposure, corr)
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
            showticklabels=TRUE)

}
