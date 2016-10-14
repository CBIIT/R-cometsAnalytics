## ---------------------------------------------------------------------------
## processUID function ----------------------------------------------------------
## ---------------------------------------------------------------------------
##' Code that creates the "compileuids.RData file"
##' @keywords internal
#
#
#processUID <- function() {
#
#  # initialize parameters:
#  metid=uid_01=uidsource=hmdb_id1=hmdb_id2=hmdb=super_pathway=c()
#
#  dir <- system.file("extdata", package = "COMETS", mustWork = TRUE)
#  xlsfile <- file.path(dir, "uid.xls")
#  masteruids = fixData(readxl::read_excel(xlsfile,1))
#  library(dplyr)
#  library(tidyr)
#
#  # Compile indices of name columns
#  uidsource.ind <- grep("_name", names(masteruids))
#
#  # get master cohortlist list
#  mastercname <-
#    stats::aggregate(
#      uidsource ~ metid + uid_01 + biochemical ,
#      masteruids %>%
#        tidyr::gather("uidsource", "metid", c(
#          min(uidsource.ind):max(uidsource.ind)
#        )) %>%
#        dplyr::filter(metid != "") %>%
#        dplyr::distinct(metid, uid_01, uidsource) %>%
#        dplyr::mutate(metid = tolower(metid)) %>%
#        dplyr::arrange(metid) %>%
#        dplyr::filter(uidsource != ""),
#      paste,
#      collapse = ";"
#    )
#
#
#  # get master HMDB list
#  masterhmdb <- stats::aggregate(
#    hmdb ~ metid + uid_01,
#    masteruids %>%
#      tidyr::gather("uidsource", "metid", c(min(uidsource.ind):max(uidsource.ind))) %>%
#      dplyr::filter(metid != "") %>%
#      tidyr::gather("hmdbsrc", "hmdb", hmdb_id1, hmdb_id2) %>%
#      dplyr::select(metid, uid_01, hmdb) %>%
#      dplyr::distinct(metid, uid_01, hmdb) %>%
#      dplyr::mutate(metid = tolower(metid)) %>%
#      dplyr::arrange(metid) %>%
#      dplyr::filter(hmdb != ""),
#    paste,
#    collapse = ";",
#    na.action = stats::na.pass
#  )
#
#
#  #use this table for harmonization:
#  masteruidl <-
#    dplyr::full_join(mastercname, masterhmdb) %>% dplyr::mutate(hmdb = ifelse(is.na(hmdb), " ", hmdb)) %>% dplyr::arrange(metid)
#
#
#  # make unique metid and concatenate rows
#  mastermetid <-
#    stats::aggregate(
#      cbind(uid_01, hmdb, uidsource, biochemical) ~ metid,
#      data = masteruidl,
#      paste,
#      collapse = "#",
#      na.action = stats::na.pass
#    )
#
#  # bring in super-pathway
#  mastermetid<-dplyr::left_join(mastermetid,dplyr::select(masteruids,uid_01,super_pathway),by=c("uid_01"="uid_01"))
#  mastermetid$metid<-gsub("\\*$","",mastermetid$metid)
#
#  rm(
#    uidsource.ind,
#    mastercname,
#    masterhmdb,
#    masteruids,
#    masteruidl,
#    xlsfile,
#    fixData,
#    dir
#  )
#
#}
##'
##'
#
#
## ---------------------------------------------------------------------------
## processcohorts function ----------------------------------------------------------
## ---------------------------------------------------------------------------
##' Code that creates the cohorts dataframe file
##' @keywords internal
#processCOHORTS <- function() {
#  dir <- system.file("extdata", package = "COMETS", mustWork = TRUE)
#  xlsfile <- file.path(dir, "cohorts.xlsx")
#  cohorts = readxl::read_excel(xlsfile,1)
#  rm(dir,xlsfile)
#}
#
