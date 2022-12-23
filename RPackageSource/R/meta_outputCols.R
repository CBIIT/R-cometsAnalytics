#' Results table 
#'  
#' The Results data frame contains the estimates
#'  from the meta-analyses.
#' Depending on the model run and options specified, 
#' all the below names may not appear in the data frame.
#' \itemize{
#' \item{\code{outcome_uid}}{ The harmonized outcome variable}
#' \item{\code{exposure_uid}}{ The harmonized exposure variable}
#' \item{\code{term}}{ For a continuous exposure, this will be the same 
#'   as \code{exposure_uid}. For a categorical exposure, it will be the
#'   dummy variable for the exposure.}
#' \item{\code{n.cohort}}{ The number of included cohorts for the estimates. }
#' \item{\code{n.sub}}{ Total number of subjects. }
#' \item{\code{fixed.pvalue}}{ P-value for the fixed-effects model. }
#' \item{\code{random.pvalue}}{ P-value for the random-effects model. }
#' \item{\code{fixed.estimate}}{ Estimate for the fixed-effects model. }
#' \item{\code{fixed.std.error}}{ Estimated standard error of \code{fixed.estimate}.
#'                                This will not appear for correlation models. }
#' \item{\code{random.estimate}}{ Estimate for the random-effects model. }
#' \item{\code{random.std.error}}{ Estimated standard error of \code{random.estimate}.
#'                                This will not appear for correlation models. }
#' \item{\code{fixed.estimate.L}}{ Lower 95% confidence limit for \code{fixed.estimate}. }
#' \item{\code{fixed.estimate.U}}{ Upper 95% confidence limit for \code{fixed.estimate}. }
#' \item{\code{random.estimate.L}}{ Lower 95% confidence limit for \code{random.estimate}. }
#' \item{\code{random.estimate.U}}{ Upper 95% confidence limit for \code{random.estimate}. }
#' \item{\code{het.pvalue}}{ P-value for Cochran's Q test of heterogeneity. }
#' \item{\code{stratavar}}{ Stratum variable(s)}
#' \item{\code{strata}}{ Stratum level(s)}
#' }
#'
#' @details 
#'  Correlation models will not contain columns for \code{fixed.std.error} and
#'           \code{random.std.error}. Instead, there will be columns for 
#'           lower and upper confidence limits of \code{fixed.estimate} and
#'           \code{random.estimate}.
#'  
#'
NULL
