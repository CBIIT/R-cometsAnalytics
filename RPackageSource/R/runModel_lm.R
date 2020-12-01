#' A list of 4:
#' \itemize{
#' \item{\code{weights}}{ A variable name to specify weights. The default is NULL.}
#' \item{\code{offset}}{ A variable name to specify an offset. The default is NULL.}
#' \item{\code{tol}}{ See \code{\link[stats]{lm}}. The default is 1e-7.}
#' \item{\code{singular.ok}}{ See \code{\link[stats]{lm}}. The default is TRUE.}
#' }
#'
#' @name lm.options
#' @title options list for lm
#' 
#' @examples 
#' model.options <- list(weights="weightVarInData")
NULL

runModel.getDefaultLmOptions <- function() {

  ret   <- list(weights=NULL, offset=NULL, tol=1e-7, singular.ok=TRUE)
  ops.c <- c("weights", "offset")
  ops.l <- "singular.ok"

  list(default=ret, ops.character=ops.c, ops.logical=ops.l)

} # END: runModel.getDefaultLmOptions

runModel.checkLmOpList <- function(op, modeldata, name="lm.options") {

  ret <- runModel.checkGlmOpList(op, modeldata, name=name, model="lm") 
  ret

} # END: runModel.checkLmOpList

runModel.defRetObj.lm <- function(dmatCols0) {

  vec               <- c("term", "estimate", "std.error", "statistic", "p.value")
  coef.names        <- vec
  fit.names         <- c("r.squared", "adj.r.squared", "sigma", "statistic",
                         "p.value", "df", "logLik", "AIC",
                         "BIC", "deviance", "df.residual", "nobs")
  coef.stats        <- matrix(data=NA, nrow=1, ncol=length(coef.names))
  colnames(coef.stats) <- coef.names
  fit.stats         <- rep(NA, length(fit.names))
  names(fit.stats)  <- fit.names
  adj               <- runModel.getVarStr(dmatCols0[-1])

  list(converged=FALSE, coef.stats=coef.stats, fit.stats=fit.stats, 
       msg="", adj=adj, adj.rem="", wald.pvalue=NA)

} # END: runModel.defRetObj.lm


runModel.callLM <- function(x, y, op) {

  # first column in design matrix will be for response
  x[, 1]      <- y
  cx          <- colnames(x)
  yvar        <- "...y"
  cx[1]       <- yvar
  colnames(x) <- cx
  mop         <- op[[getModelOpsName()]]
  form        <- as.formula(runModel.getFormulaStr(yvar, cx[-1]))
  fit         <- lm(form, data=as.data.frame(x), 
                    weights=mop[["weights.vec", exact=TRUE]],
                    offset=mop[["offset.vec", exact=TRUE]],
                    singular.ok=mop$singular.ok, tol=mop$tol)

  fit

} # END: runModel.callLM
