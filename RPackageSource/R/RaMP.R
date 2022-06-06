#' A list of 6:
#' \itemize{
#' \item{\code{dbname}}{ The default value is "ramp".}
#' \item{\code{username}}{ The default value is "root".}
#' \item{\code{conpass}}{ The default is "".}
#' \item{\code{host}}{ The default is "localhost".}
#' \item{\code{socket}}{ The default is "".}
#' \item{\code{pvalue.adj}}{ The maximum BH-adjusted p-value for a metabolite
#'                          to be included in the analysis.
#'                    The default is 0.05.}
#' }
#'
#' @name ramp_options
#' @title RaMP options list
#' @details See \code{\link[RaMP]{setConnectionToRaMP}} for details on the options
#'    \code{dbname}, \code{username}, \code{conpass}, \code{host},
#'    and \code{socket}.
#' 
#' @examples 
NULL

#---------------------------------------------------------
#' Chemical Class Enrichment
#'
#' @param df the "Effects" data frame returned from function \code{\link{runModel}}
#' @param metabdata metabolite data list from \code{\link{readCOMETSinput}}
#' @param op list of options when running in \code{Interactive} mode (see \code{\link{ramp_options}}).
#'
#' @return A data frame
#'
#' @details The set of all possible metbolite id types are:
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
#' modeldata <- getModelData(exmetabdata,exposures="age",modlabel="1 Gender adjusted",
#' 	outcomes=c("lactose","lactate"))
#' obj <- runModel(modeldata,exmetabdata, cohortLabel="DPP")
#' # ret <- chemClassEnrichment(obj$Effects, exmetabdata, op=list(pvalue.adj=1))
#' @export

chemClassEnrichment <- function(df, metabdata, op=NULL) {

  ramp_checkDataFrame(df)
  ramp_checkMetabdata(metabdata) 
  op <- ramp_checkOptions(op)

  ret <- ramp_chemClassEnrichment(df, metabdata, op)
  ret
}

ramp_chemClassEnrichment <- function(df, metabdata, op) {

  DEBUG <- op$DEBUG
  if (DEBUG) print(paste0("exists('pkg.globals') = ", exists("pkg.globals")))

  connectFlag <- 1
  if (exists("pkg.globals") && ("environment" %in% class(pkg.globals))) connnectFlag <- 0
  if (DEBUG) print(paste0("connectFlag = ", connectFlag))

  # pkg.globals must be global
  if (connectFlag) {
    pkg.globals <- setConnectionToRaMP(dbname=op$dbname, username=op$username, 
                     conpass=op$conpass, host=op$host, socket=op$socket)
    assign("pkg.globals", pkg.globals, envir = .GlobalEnv)
  }

  # Get metabolites of interest
  rampids <- ramp_getMetabsOfInterest(df, metabdata, op)
  if (DEBUG) print(sort(rampids))
  
  # Call main function
  ret <- RaMP::chemicalClassEnrichment(mets=rampids)

  # Set return
  ret <- ramp_setReturn(ret)
  ret

} # END: ramp_chemClassEnrichment

ramp_checkDataFrame <- function(x, name="df") {

  pv   <- getEffectsPvalueName()
  yv   <- getEffectsOutcomespecName()
  ev   <- getEffectsExposurespecName()
  cols <- c(pv, yv, ev)
  rc   <- nonEmptyDfHasCols(x, cols)
  if (!rc) {
    msg <- paste0(cols, collapse=", ")
    msg <- paste0("ERROR: ", name, " must contain columns ", msg)
    stop(msg)
  }
  NULL
}

ramp_checkOptions <- function(op, name="op") {

  if (is.null(op)) op <- list()
  if (!is.list(op)) stop("ERROR: op must be NULL or a list")

  valid <- c("dbname", "username", "conpass", "host", "socket",
             getRampPvalOpName(), "DEBUG")
  def   <- list("ramp", "root", "", "localhost", "",
                getRampPvalOpDefault(), 0)
  if (length(op)) check.list(op, name, valid)
  op <- default.list(op, valid, def)

  op

} # END: ramp_checkOptions

ramp_checkMetabdata <- function(x, name="metabdata") {

  if (!is.list(x)) stop(paste0("ERROR: ", name, " must be a list"))
  req <- c("metab", "metabId")
  checkRequiredListNames(x, req, name) 
  if (!nonEmptyDfHasCols(x$metab, x$metabId)) {
    msg <- paste0("ERROR: ", x$metabId, " variable not found in metab data")
    stop(msg)
  }
  idtypes <- ramp_getMetabIdTypes()
  rc      <- nonEmptyDfHasCols(x$metab, idtypes, allcols=0, ignoreCase=1)
  if (!rc) {
    msg <- paste0(idtypes, collapse=", ")
    msg <- paste0("ERROR: the metab data must contain one of the id types ", msg)
    stop(msg) 
  }
  NULL
}

ramp_getSigMetabIds <- function(df, metabdata, op) {

  DEBUG <- op$DEBUG
  ret   <- NULL
  pv    <- getEffectsPvalueName()
  maxp  <- op[[getRampPvalOpName(), exact=TRUE]]
  if (DEBUG) print(paste0("max adj p-value = ", maxp))
  if (!length(maxp)) stop("INTERNAL CODING ERROR 1")
  df   <- subsetDfByPvalue(df, pv, maxp, adj=getRampPvalAdjMethod())
  if (!length(df)) stop(paste0("No metabolites with adjusted p-value <= ", maxp))
  if (DEBUG) print(paste0(nrow(df), " rows left after applying p-value condition"))

  yv  <- getEffectsOutcomespecName()
  ev  <- getEffectsExposurespecName()
  mv  <- metabdata$metabId
  ret <- trimws(toupper(unique(c(df[, yv, drop=TRUE], df[, ev, drop=TRUE]))))
  all <- trimws(toupper((metabdata$metab)[, mv, drop=TRUE]))
  tmp <- ret %in% all
  ret <- ret[tmp]
  if (!length(ret)) stop("No outcomes/exposures match with the metabolite ids")
  ret

} # END: ramp_getSigMetabIds

ramp_getMetabIdTypes <- function() {

  ret <- c("LIPIDMAPS", "pubchem", "hmdb", "chemspider", "chebi", "CAS", 
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
  ret
}

ramp_getIdsForRaMP <- function(metabids, metabDF, metabDF.idvar, op) {

  cx                <- toupper(colnames(metabDF))
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
