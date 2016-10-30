## ---------------------------------------------------------------------------
## processUID function ----------------------------------------------------------
## ---------------------------------------------------------------------------
##' Code that creates the "compileuids.RData file"
##' @keywords internal
#
#
processUID <- function() {

source('~/Comets/Git-RComets/R/internalFunctions.r')
 # initialize parameters:
 metid=uid_01=uidsource=hmdb=super_pathway=c()

 dir <- system.file("extdata", package = "COMETS", mustWork = TRUE)
 xlsfile <- file.path(dir, "uid.xls")
 masteruids = fixData(readxl::read_excel(xlsfile,1))
 library(dplyr)
 library(tidyr)

 # Compile indices of name columns
 uidsource.ind <- grep("_name", names(masteruids))

 # get master list of all metabolite_id and attribute to source
 mastercname <-
   stats::aggregate(
     uidsource ~ metid + uid_01 ,
     masteruids %>%
       tidyr::gather("uidsource", "metid", c(
         min(uidsource.ind):max(uidsource.ind)
       )) %>%
       dplyr::filter(metid != "") %>%
       dplyr::distinct(metid, uid_01, uidsource) %>%
       dplyr::mutate(metid = tolower(metid)) %>%
       dplyr::arrange(metid) %>%
       dplyr::filter(uidsource != ""),
     paste,
     collapse = ";"
   )


 # bring in super-pathway
 mastermetid<-dplyr::left_join(mastercname,dplyr::select(masteruids,uid_01,main_class,chemical_id,comp_id,hmdb_id),by=c("uid_01"="uid_01")) %>%
     dplyr::mutate(metid=gsub("\\*$","",metid)) %>%
     dplyr::mutate(comp_id=gsub(".000000","",comp_id)) %>%
     dplyr::mutate(hmdb_id=trimws(hmdb_id)) %>%
     dplyr::mutate(hmdb_id=gsub("$#","",hmdb_id)) %>%
     dplyr::mutate(hmdb_id=gsub("^#","",hmdb_id)) %>%
     dplyr::arrange(uid_01)



 rm(
   uidsource.ind,
   mastercname,
   masteruids,
   xlsfile,
   fixData,
   dir,
   hmdb,
   metid,
   super_pathway,
   uid_01,
   uidsource,
   checkIntegrity,
   Harmonize,
   prdebug

 )

}
# do not run
processUID()
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
