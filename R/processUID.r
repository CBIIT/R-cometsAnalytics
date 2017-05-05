## ---------------------------------------------------------------------------
## processUID function ----------------------------------------------------------
## ---------------------------------------------------------------------------
##' Code that creates the "compileuids.RData file"
##' @keywords internal
#
# uncomment to run
# processUID <- function() {
#
# source('~/Comets/Git-RComets/R/internalFunctions.r')
#  # initialize parameters:
#  metid=uid_01=uidsource=hmdb=main_class=c()
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
#  # get master list of all metabolite_id and attribute to source
#  mastercname <-
#    stats::aggregate(
#      uidsource ~ metid + uid_01 ,
#      masteruids %>%
#        tidyr::gather("uidsource", "metid", c(
#          min(uidsource.ind):max(uidsource.ind)
#        )) %>%
#        dplyr::filter(metid != "") %>%
#        dplyr::mutate(metid=gsub("\\*$","",metid)) %>%
#        dplyr::distinct(metid, uid_01, uidsource) %>%
#        dplyr::mutate(metid = tolower(metid)) %>%
#        dplyr::arrange(metid) %>%
#        dplyr::filter(uidsource != ""),
#      paste,
#      collapse = ";"
#    )
#
#
#  # bring in super-pathway and other metabolite metadata and clean up the NA and # for blank values
#  mastermetid<-dplyr::left_join(mastercname,dplyr::select(masteruids,uid_01,main_class,chemical_id,comp_id,hmdb_id,biochemical),by=c("uid_01"="uid_01")) %>%
#      dplyr::mutate(hmdb_id=trimws(hmdb_id)) %>%
#      dplyr::mutate(comp_id=gsub(".000000","",comp_id)) %>%
#      dplyr::mutate(hmdb_id=gsub("$#","",hmdb_id)) %>%
#      dplyr::mutate(hmdb_id=gsub("^#","",hmdb_id)) %>%
#      dplyr::mutate(hmdb_id=gsub("$NA","",hmdb_id)) %>%
#      dplyr::mutate(comp_id=trimws(comp_id)) %>%
#      dplyr::mutate(comp_id=gsub("$#","",comp_id)) %>%
#      dplyr::mutate(comp_id=gsub("^#","",comp_id)) %>%
#      dplyr::mutate(comp_id=gsub("$NA","",comp_id)) %>%
#      dplyr::mutate(chemical_id=trimws(chemical_id)) %>%
#      dplyr::mutate(chemical_id=gsub("$#","",chemical_id)) %>%
#      dplyr::mutate(chemical_id=gsub("^#","",chemical_id)) %>%
#      dplyr::mutate(chemical_id=gsub("$NA","",chemical_id)) %>%
#      dplyr::mutate(main_class=trimws(main_class)) %>%
#      dplyr::mutate(main_class=gsub("$#","",main_class)) %>%
#      dplyr::mutate(main_class=gsub("^#","",main_class)) %>%
#      dplyr::mutate(main_class=gsub("$NA","",main_class)) %>%
#      dplyr::mutate(main_class=gsub("^#NA","",main_class)) %>%
#      dplyr::mutate(main_class=ifelse(trimws(main_class) == "NA","",main_class)) %>%
#      dplyr::mutate(main_class=ifelse(trimws(main_class) == "-","",main_class)) %>%
#      dplyr::arrange(uid_01)
#
#
#    # make unique metid and concatenate all rows for values of the other 7 columns
#  mastermetid<-aggregate(cbind(uidsource,main_class,chemical_id,comp_id,hmdb_id,biochemical)~metid+uid_01,
#                         data=mastermetid,
#                         paste,
#                         collapse="#",
#                         na.action=na.pass)%>%
#             dplyr::mutate(main_class=ifelse(trimws(main_class) == "NA","",main_class)) %>%
#             dplyr::mutate(main_class=ifelse(trimws(main_class) == "NA#NA","",main_class)) %>%
#             dplyr::mutate(hmdb_id=ifelse(trimws(hmdb_id) == "NA#NA","",hmdb_id)) %>%
#             dplyr::mutate(comp_id=ifelse(trimws(comp_id) == "NA","",comp_id)) %>%
#             dplyr::mutate(chemical_id=ifelse(trimws(chemical_id) == "NA","",chemical_id)) %>%
#             dplyr::mutate(hmdb_id=ifelse(trimws(hmdb_id) == "NA","",hmdb_id))
#
#    #final make unique
#    # testing duplicated mastermetid
# mastermetid<-aggregate(cbind(uid_01,uidsource,main_class,chemical_id,comp_id,hmdb_id,biochemical)~metid,
#                            data=mastermetid,
#                            paste,
#                            collapse="#",
#                            na.action=na.pass)%>%
#   dplyr::mutate(main_class=ifelse(trimws(main_class) == "NA","",main_class)) %>%
#   dplyr::mutate(main_class=ifelse(trimws(main_class) == "NA#NA","",main_class)) %>%
#   dplyr::mutate(hmdb_id=ifelse(trimws(hmdb_id) == "NA#NA","",hmdb_id)) %>%
#   dplyr::mutate(comp_id=ifelse(trimws(comp_id) == "NA","",comp_id)) %>%
#   dplyr::mutate(chemical_id=ifelse(trimws(chemical_id) == "NA","",chemical_id)) %>%
#   dplyr::mutate(hmdb_id=ifelse(trimws(hmdb_id) == "NA","",hmdb_id))
#
#  rm(
#    uidsource.ind,
#    mastercname,
#    masteruids,
#    xlsfile,
#    fixData,
#    dir,
#    hmdb,
#    metid,
#    main_class,
#    uid_01,
#    uidsource,
#    checkIntegrity,
#    Harmonize,
#    prdebug
#
#  )
# #
# }
# # do not run
# processUID()
##'
##'
#
#
## ---------------------------------------------------------------------------
## processcohorts function ----------------------------------------------------------
## ---------------------------------------------------------------------------
##' Code that creates the cohorts dataframe file, just run the internal code within functionr
##' @keywords internal
# processCOHORTS <- function() {
# dir <- system.file("extdata", package = "COMETS", mustWork = TRUE)
# xlsfile <- file.path(dir, "cohorts.xlsx")
# cohorts = readxl::read_excel(xlsfile,1)
# rm(dir,xlsfile)
# save updated rdata from the global env
# }
#
