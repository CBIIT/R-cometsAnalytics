
#' @name meta_options
#' @title Meta-analysis options
#' @description A list of options for the \code{\link{runMeta}} function
#' @details
#' \itemize{
#' \item{\code{min.n.cohort}}{ Minimum number of cohorts to include when meta-analyzing
#'                             each metabolite.
#'                     The default is 2.}
#' \item{\code{min.nsub.cohort}}{ Minimum number of subjects in a cohort for a metabolite 
#'                                to be included from that cohort.
#'                     The default is 25.}
#' \item{\code{min.nsub.total}}{ Minimum number of subjects in all cohorts for a metabolite
#'                               to be meta-analyzed.
#'                     The default is 50.}
#' \item{\code{cohorts.include}}{ Character vector of cohorts to include.
#'                      The default is NULL.}
#' \item{\code{cohorts.exclude}}{ Character vector of cohorts to exclude.
#'                      The default is NULL.}
#' \item{\code{output.type}}{ Type of output file, either "xlsx" for an Excel worksheet
#'                            or "rda" for an R object file created with the \code{save()} function.
#'                      The default is "xlsx".}
#' \item{\code{strata.exclude.het.test}}{ A list of stratification levels to be excluded from
#'                      the test for heterogeneity.
#'      This list has the form list(var1=vec1, var2=vec2, ...), where var1, var2, ... are stratification
#'      variables, and vec1, vec2, ... are vectors of stratification levels to be removed
#'      from the test.
#'                      The default is NULL.}
#' \item{\code{dups.allow}}{ TRUE or FALSE to allow for duplicated metabolite results in a file.
#'                      If TRUE, the result with the largest \code{nobs} will be used.
#'                      The default is FALSE.}
#' \item{\code{stopOnFileError}}{ TRUE or FALSE to stop processing when a problem with any
#'                                   one of the input files is encountered. If FALSE, then
#'                           the files containing errors will be removed from the analysis.
#'                      The default is TRUE.}
#' \item{\code{oneModelCheck}}{ TRUE or FALSE to check for consistent files when each file
#'                              consists of results from a single model. If TRUE, then each
#'                              file must have the same model name, model function, exposure
#'                              (or outcome) and for a categorical exposure variable, the same
#'                              reference value.
#'                      The default is TRUE.}
#' }
#'
NULL


getValidGlobalMetaOps <- function() {

  ops.char    <- c(getOutTypeOpName(), "MODEL")
  ops.charVec <- c(metaOp_cohorts.include(), metaOp_cohorts.exclude())
  ops.num     <- c(metaOp_minNcohortName(), metaOp_cohortMinSubs(), metaOp_totalMinSubs(),
                   "DEBUG")
  ops.log     <- c(metaOp_oneModelCheck(), metaOp_dups.allow(), metaOp_stopOnFileError())
  ops.list    <- metaOp_strataToExcludeFromHetTest()
  default     <- list(cohorts.include=NULL, cohorts.exclude=NULL, DEBUG=0, MODEL="")
  default[[metaOp_minNcohortName()]] <- metaOp_minNcohortDefault()
  default[[metaOp_cohortMinSubs()]]  <- metaOp_cohortMinSubsDefault()
  default[[metaOp_totalMinSubs()]]   <- metaOp_totalMinSubsDefault()
  default[[getOutTypeOpName()]]      <- getOutTypeOpDefault()
  default[metaOp_strataToExcludeFromHetTest()] <- list(NULL)
  default[[metaOp_oneModelCheck()]]            <- metaOp_oneModelCheckDefault()
  default[[metaOp_dups.allow()]]               <- metaOp_dups.allowDefault()
  default[[metaOp_stopOnFileError()]]          <- metaOp_stopOnFileErrorDefault()

  valid <- names(default)

  list(ops.character=ops.char, ops.numeric=ops.num, ops.logical=ops.log,
       valid=valid, default=default, ops.charVec=ops.charVec, ops.list=ops.list)

} 

meta_check_op <- function(op, name="op") {

  if (!length(op)) op <- list()
  tmp        <- getValidGlobalMetaOps()
  # Add in models, which is not a global option in opfile
  valid      <- c(tmp$valid, metaOp_models())
  def        <- c(tmp$def, list(NULL))
  names(def) <- valid

  check.list(op, name, valid)

  op <- checkGlobalOpList(op, name=name, meta=1)

  #op <- default.list(op, valid, def)
  op
}


