#' The ModelSummary data frame contains one row of model
#' summary results for each exposure/outcome combination.
#' Depending on the model run, 
#' all the below names may not appear in the data frame.
#' \itemize{
#' \item{\code{adjspec}}{ Original adjustment variables specified}
#' \item{\code{adjvars}}{ Adjustment variables included in the model}
#' \item{\code{adjvars.removed}}{ Adjustment variables removed from the model}
#' \item{\code{adj_uid}}{ Adjustment variable universal ids}
#' \item{\code{adj.r.squared}}{ Adjusted R-squared}
#' \item{\code{aic}}{ Akaike information criterion}
#' \item{\code{bic}}{ Bayesian information criterion}
#' \item{\code{cohort}}{ String passed into \code{\link{runModel}}}
#' \item{\code{converged}}{ TRUE or FALSE for model convergence}
#' \item{\code{deviance}}{ Deviance of the fitted model}
#' \item{\code{df.null}}{ NULL model degrees of freedom}
#' \item{\code{df.residual}}{ Residual degrees of freedom}
#' \item{\code{exposure_uid}}{ Exposure universal id}
#' \item{\code{exposurespec}}{ Exposure variable}
#' \item{\code{loglik}}{ Log-likelihood of the fitted model}
#' \item{\code{message}}{ Error message produced from the modeling function}
#' \item{\code{model}}{ Model label from \code{\link{getModelData}}}
#' \item{\code{nobs}}{ Number of observations used}
#' \item{\code{null.deviance}}{ Deviance of the NULL model}
#' \item{\code{outcome}}{ Outcome variable}
#' \item{\code{outcomespec}}{ Outcome variable}
#' \item{\code{outcome_uid}}{ Outcome universal id}
#' \item{\code{run}}{ Run number that can be used to link with the 
#'                    \code{\link{Effects}} table}
#' \item{\code{r.squared}}{ R-squared, the fraction of variance explained by the model}
#' \item{\code{sigma}}{ Square root of the estimated variance of the random error}
#' \item{\code{spec}}{ "Batch" or "Interactive"}
#' \item{\code{strata}}{ Stratum label}
#' \item{\code{strata.num}}{ Stratum number}
#' \item{\code{wald.pvalue}}{ P-value from the Wald test of the exposure variable. 
#'                           Note that this test may be a multi-df test if the
#'                           exposure is categorical.}
#' }
#'
#' @name ModelSummary
#' @title ModelSummary table 
#' @details Missing values will appear if a model did not converge, produced an error,
#'    or not run because of too many missing values in the outcome. 
#' 
NULL

#' The Effects data frame contains the estimates
#'  for each exposure, and  will contain multiple rows
#'  for categorical exposure variables.
#' Depending on the model run, 
#' all the below names may not appear in the data frame.
#' \itemize{
#' \item{\code{corr}}{ The correlation between \code{term} and the outcome}
#' \item{\code{estimate}}{ The regression coefficient of \code{term}}
#' \item{\code{exposurespec}}{ Exposure variable}
#' \item{\code{outcomespec}}{ Outcome variable}
#' \item{\code{p.value}}{ The p-value of the test}
#' \item{\code{run}}{ Run number that can be used to link with the
#'                    \code{\link{ModelSummary}} table}
#' \item{\code{statistic}}{ The test statistic for \code{term}}
#' \item{\code{std.error}}{ The standard error of \code{estimate}}
#' \item{\code{strata}}{ Stratum label}
#' \item{\code{strata.num}}{ Stratum number}
#' \item{\code{term}}{ Exposure variable or dummy variable name}
#' }
#'
#' @name Effects
#' @title Effects table 
#' @details Missing values will appear if a model did not converge, produced an error,
#'    or not run because of too many missing values in the outcome. 
#' 
NULL

#' Columns in the Errors_Warnings table. 
#' \itemize{
#' \item{\code{type}}{ WARNING or ERROR}
#' \item{\code{object}}{ The object that produced the warning or error. This is
#'                       typically a variable or a particular stratum.}
#' \item{\code{message}}{ Message describing the warning or error}
#' }
#'
#' @name Errors_Warnings
#' @title Errors and Warnings table 
#' @details The kinds of warnings and errors stored in this matrix are ones that
#'    apply to all models or all outcomes for an exposure variable. An error
#'    message for a particular exposure-outcome pair will be stored in the
#'    \code{message} column of the \code{\link{ModelSummary}} table.
#' 
NULL

