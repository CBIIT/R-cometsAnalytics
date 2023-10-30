
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
#' \item{\code{stopOnFileError}}{ TRUE or FALSE to stop processing when a problem with any
#'                                   one of the input files is encountered. If FALSE, then
#'                           the files containing errors will be removed from the analysis.
#'                      The default is TRUE.}
# \item{\code{oneModelCheck}}{ TRUE or FALSE to check for consistent files when each file
#                              consists of results from a single model. If TRUE, then each
#                              file must have the same model name, model function, exposure
#                              (or outcome) and for a categorical exposure variable, the same
#                              reference value.
#                      The default is TRUE.}
#' \item{\code{merge.cohort.files}}{ TRUE or FALSE to merge files with the same cohort name.
#'                     This will take the union of all metabolites in the files. A metabolite
#'                     that occurs in multiple files will be chosen from the file with the
#'                     maximum sample size for that metabolite. 
#'                      The default is FALSE.}
#' \item{\code{add.cohort.names}}{ TRUE or FALSE to add binary columns (one for each cohort) to
#'       the data frame of results to show which cohorts contributed to the meta-analysis
#        results for each metabolite. The names of the binary columns will be the cohort names.
#'       The value will be 1 if the cohort contributed to the result or 0 if the cohort did not.
#'                      The default is TRUE.}
#' \item{\code{add.cohort.cols}}{ Vector of column names to add to the output for each cohort.
#'       These must be column names from the ModelSummary or Effects
#'       tables in the meta-analysis input files.
#'       For example, if \code{add.cohort.cols = "pvalue"}, then the p-values from each 
#'       cohort will be added to the output, and the columns will be of the form
#'       <cohort name>.pvalue. A missing value will appear if the cohort did not
#'       contribute to the result.
#'                      The default is NULL.}
#' }
#'
NULL


getValidGlobalMetaOps <- function() {

  ops.char    <- c(getOutTypeOpName(), "MODEL")
  ops.charVec <- c(metaOp_cohorts.include(), metaOp_cohorts.exclude(),
                   metaOp_addCohortCols())
  ops.num     <- c(metaOp_minNcohortName(), metaOp_cohortMinSubs(), metaOp_totalMinSubs(),
                   metaOp_dups.method(), "DEBUG")
  ops.log     <- c(metaOp_oneModelCheck(),  metaOp_stopOnFileError(),
                   metaOp_addCohortNames(), metaOp_mergeCohortFiles())
  ops.list    <- metaOp_strataToExcludeFromHetTest()
  default     <- list(cohorts.include=NULL, cohorts.exclude=NULL, DEBUG=0, MODEL="")
  default[[metaOp_minNcohortName()]]           <- metaOp_minNcohortDefault()
  default[[metaOp_cohortMinSubs()]]            <- metaOp_cohortMinSubsDefault()
  default[[metaOp_totalMinSubs()]]             <- metaOp_totalMinSubsDefault()
  default[[getOutTypeOpName()]]                <- getOutTypeOpDefault()
  default[metaOp_strataToExcludeFromHetTest()] <- list(NULL)
  default[[metaOp_oneModelCheck()]]            <- metaOp_oneModelCheckDefault()
  default[[metaOp_dups.method()]]              <- metaOp_dups.methodDefault()
  default[[metaOp_stopOnFileError()]]          <- metaOp_stopOnFileErrorDefault()
  default[[metaOp_mergeCohortFiles()]]         <- metaOp_mergeCohortFilesDefault()
  default <- addNamedValueToList(default, metaOp_addCohortCols(), metaOp_addCohortColsDefault())
  default <- addNamedValueToList(default, metaOp_addCohortNames(), metaOp_addCohortNamesDefault())

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


