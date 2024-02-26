
#---------------------------------------------------------
#' Chemical Class Enrichment
#'
#' @param df The "Effects" data frame returned from function \code{\link{runModel}}
#' @param metabdata Return object from \code{\link{readCOMETSinput}}
#' @param db.version The RaMP SQLite database version to use.
#'                   The default is the most recent version.
#' @param pvalue.adj The maximum BH-adjusted p-value for a metabolite
#'                   to be included in the analysis.
#'
#' @return A data frame
#'
#' @details The \bold{RaMP} R package must be installed prior to calling
#' this function. An attempt to load RaMP will be done through the 
#' \code{\link[base]{requireNamespace}} function.
#'  
#' The set of all possible metbolite id types are:
#' LIPIDMAPS, pubchem, hmdb, chemspider, chebi, CAS, wikidata, swisslipids, 
#' kegg, lipidbank, plantfa, and kegg_glycan.
#' The \bold{METABOLITES} sheet in the input Excel file will need to have at least
#' one of the above id types as a column (case-insensitive). If the \bold{METABOLITES} sheet
#' contains more than one id type, then the metabolite ids passed into 
#' \code{\link[RaMP]{chemicalClassEnrichment}} depends on the order of the id type
#' columns. For example, if the \bold{METABOLITES} sheet contains columns HMDB (column F)
#' and pubchem (column C), then the metbolite ids will taken from the pubchem column and
#' any missing ids from the pubchem column will be assigned from the HMDB column.
#'
#' @examples
#' dir <- system.file("extdata", package="RcometsAnalytics", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' modeldata <- getModelData(exmetabdata,exposures="age", modelspec="Interactive",
#' 	outcomes=c("lactose","lactate"))
#' obj <- runModel(modeldata,exmetabdata, cohortLabel="DPP")
#' # ret <- chemClassEnrichment(obj$Effects, exmetabdata, pvalue.adj=1)
#' @export

chemClassEnrichment <- function(df, metabdata, db.version=NULL, pvalue.adj=0.05) {

  ramp_checkForRampPackage()
  ramp_check_DataFrame(df)
  ramp_check_Metabdata(metabdata) 
  ramp_check_db.version(db.version)
  ramp_check_pvalue.adj(pvalue.adj)

  op  <- list(db.version=db.version, pvalue.adj=pvalue.adj, DEBUG=0)
  ret <- ramp_chemClassEnrichment(df, metabdata, op)
  ret
}

ramp_checkForRampPackage <- function() {
  if (!requireNamespace("RaMP", quietly = TRUE)) stop(msg_ramp_package())
  NULL
}

ramp_chemClassEnrichment <- function(df, metabdata, op) {

  DEBUG <- op$DEBUG
 
  # Get metabolites of interest
  rampids <- ramp_getMetabsOfInterest(df, metabdata, op)
  if (DEBUG) print(sort(rampids))
  
  tmp <- op[["db.version", exact=TRUE]]
  if (!length(tmp) || !nchar(tmp)) {
    rampDB <- RaMP::RaMP()
  } else {
    rampDB <- RaMP::RaMP(tmp)
  }

  # Call main function
  ret <- RaMP::chemicalClassEnrichment(db=rampDB, mets=rampids)

  # Set return
  ret <- ramp_setReturn(ret)
  ret

} # END: ramp_chemClassEnrichment

ramp_check_DataFrame <- function(x, name="df") {

  pv   <- getEffectsPvalueName()
  yv   <- getEffectsOutcomespecName()
  ev   <- getEffectsExposurespecName()
  cols <- c(pv, yv, ev)
  rc   <- nonEmptyDfHasCols(x, cols)
  if (!rc) {
    cstr <- getQuotedVarStr(cols)
    msg  <- msg_arg_colsNotFound(c(name, cstr))
    stop(msg)
  }
  NULL
}

ramp_check_Metabdata <- function(x, name="metabdata") {

  if (!is.list(x)) stop(msg_arg_notList(name))
  req <- c("metab", "metabId")
  checkRequiredListNames(x, req, name) 
  if (!nonEmptyDfHasCols(x$metab, x$metabId)) {
    msg <- msg_arg_varNotFound(c(x$metabId, "metab data"))
    stop(msg)
  }
  idtypes <- ramp_getMetabIdTypes()
  rc      <- nonEmptyDfHasCols(x$metab, idtypes, allcols=0, ignoreCase=1)
  if (!rc) {
    tmp <- getQuotedVecStr(idtypes)
    msg <- msg_ramp_idtypes(tmp)
    stop(msg) 
  }
  NULL
}

ramp_check_db.version <- function(x, name="db.version") {

  if (length(x)) {
    #valid <- c("2.4.0", "2.3.2", "2.3.1")
    #check.string(x, valid, name)
    if (!isString(x)) stop(msg_ramp_dbversion(name))
  }
  NULL
}

ramp_check_pvalue.adj <- function(x, name="pvalue.adj") {

  check.range(x, name, 0, 1, upper.inc=TRUE)
  
  NULL
}

ramp_getSigMetabIds <- function(df, metabdata, op) {

  DEBUG <- op$DEBUG
  ret   <- NULL
  pv    <- getEffectsPvalueAdjName()
  maxp  <- op[["pvalue.adj", exact=TRUE]]
  if (DEBUG) print(paste0("max adj p-value = ", maxp))
  if (!length(maxp)) stop("INTERNAL CODING ERROR 1")
  tmp  <- df[, pv, drop=TRUE] <= maxp
  tmp[is.na(tmp)] <- FALSE
  df <- df[tmp, , drop=FALSE]
  if (!nrow(df)) stop(msg_ramp_no_padj(maxp))
  if (DEBUG) print(paste0(nrow(df), " rows left after applying p-value condition"))

  yv  <- getEffectsOutcomespecName()
  ev  <- getEffectsExposurespecName()
  mv  <- metabdata$metabId
  ret <- trimws(toupper(unique(c(df[, yv, drop=TRUE], df[, ev, drop=TRUE]))))
  all <- trimws(toupper((metabdata$metab)[, mv, drop=TRUE]))
  tmp <- ret %in% all
  ret <- ret[tmp]
  if (!length(ret)) stop(msg_ramp_no_metab())
  ret

} # END: ramp_getSigMetabIds

ramp_getMetabIdTypes <- function() {

  ret <- c("hmdb", "pubchem", "LIPIDMAPS", "chemspider", "chebi", "CAS", 
           "wikidata", "swisslipids", "kegg", "lipidbank", "plantfa", 
           "kegg_glycan")
  ret
}

ramp_getMetabsOfInterest <- function(df, metabdata, op) {

  # Id types for metabolites: (case-sensitive)
  # LIPIDMAPS, pubchem, hmdb, chemspider, chebi, CAS, wikidata, swisslipids, 
  # kegg, lipidbank, plantfa, kegg_glycan

  metabids <- ramp_getSigMetabIds(df, metabdata, op) 
  ret      <- ramp_getIdsForRaMP(metabids, metabdata$metab, metabdata$metabId, op) 
  ret      <- ret[nchar(ret) > 0]
  if (!length(ret)) stop(msg_ramp_no_metab2())

  ret
}

ramp_getIdsForRaMP <- function(metabids, metabDF, metabDF.idvar, op) {

  cx <- toupper(colnames(metabDF))
  # Check for HMDB_ID, HMDB_ID.COHORT, HMDB_ID.COMETS
  vv <- c("HMDB_ID", "HMDB_ID.COHORT", "HMDB_ID.COMETS")
  for (v in vv) {
    if ((v %in% cx) && !("HMDB" %in% cx)) {
      tmp     <- cx %in% v
      cx[tmp] <- "HMDB"
      break
    }
  } 
  colnames(metabDF) <- cx

  # Get the id types in the data
  types  <- ramp_getMetabIdTypes()
  utypes <- toupper(types)
  ind    <- match(cx, utypes)
  ind    <- ind[!is.na(ind)]
  if (!length(ind)) stop("INTERNAL CODING ERROR 1") 
  types  <- types[ind]
  utypes <- toupper(types)
  ntypes <- length(types)

  metabDF.idvar <- toupper(metabDF.idvar)
  mids          <- trimws(toupper(metabDF[, metabDF.idvar, drop=TRUE]))
  tmp           <- mids %in% trimws(toupper(metabids))   
  if (!any(tmp)) stop("INTERNAL CODING ERROR 2")
  miss          <- c("", "NA", NA)
  metabDF       <- metabDF[tmp, , drop=FALSE]
  vec           <- trimws(toupper(metabDF[, utypes[1], drop=TRUE]))
  tmp           <- vec %in% miss
  ids           <- paste(types[1], vec, sep=":")
  #all           <- ids[!tmp]
  if (any(tmp)) ids[tmp] <- ""

  if (ntypes > 1) {  
    for (i in 2:ntypes) {
      tmp0 <- ids %in% miss
      if (!any(tmp0)) break 
      vec  <- trimws(toupper(metabDF[, utypes[i], drop=TRUE]))
      id2  <- paste(types[i], vec, sep=":")
      tmpm <- vec %in% miss 
      tmp  <- tmp0 & !tmpm
      if (any(tmp)) ids[tmp] <- paste(types[i], vec[tmp], sep=":")
      #all  <- c(all, id2[!tmpm])
    }
  }

  ids
}

ramp_setReturn <- function(obj) {

  n <- length(obj)
  if (!n) return(NULL)
  ret <- NULL
  for (i in 1:n) {
    x  <- obj[[i]]
    nr <- nrow(x)
    if (is.null(nr)) nr <- 0
    if (nr) ret <- df.rbind.all(ret, x)
  }
  ret

}
