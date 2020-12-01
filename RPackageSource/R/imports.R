#' @docType package
#' @name COMETS
#' @title COMETS Analytics R package
#' @description This R package supports all cohort-specific analyses of
#'        the COMETS consortium \url{https://www.comets-analytics.org/}. 
#' Data are not saved in the system but
#'        output must be downloaded and submitted for meta-analyses.
#'  import only functions needed
#' @importFrom readxl read_excel
#' @importFrom plyr mapvalues
#' @import dplyr
#' @importFrom psych partial.r describe
#' @import plotly
#' @import caret
#' @import heatmaply
#' @importFrom stats model.matrix glm.fit lm cor as.formula cov cov2cor pnorm pt glm.control pchisq
#' @importFrom tidyr gather
#' @importFrom broom glance tidy
#' @importFrom stats cor
#' @importFrom MASS ginv
#' @importFrom subselect trim.matrix
#' @details
#' \bold{Functions for analysis:} \cr
#' \code{\link{runCorr}} (correlation analysis) \cr
#' \code{\link{runModel}} (correlation, glm or lm) \cr
#' \code{\link{runAllModels}} (run models in batch mode from models sheet) \cr
#' \bold{Functions for graphics:} \cr
#' \code{\link{plotVar}} (metabolite variance distribution plot) \cr
#' \code{\link{plotMinvalues}} (distribution of missing values) \cr
#' \code{\link{showHeatmap}} (heat map of metabolite correlations) \cr
#' \code{\link{showHClust}} (interactive heat map with hierarchical clustering) \cr
#' \bold{Functions for saving results to files:} \cr
#' \code{\link{OutputCSVResults}} (write to .csv file) \cr
#' \code{\link{OutputXLSResults}} (write to excel file) \cr
#' \code{\link{OutputListToExcel}} (write list of data frames to excel file with multiple sheets) \cr
NULL
