# ---------------------------------------------------------------------------
# processUID function ----------------------------------------------------------
# ---------------------------------------------------------------------------
#' Code that creates the "compileuids.RData file"
#' @keywords internal


processUID <- function() {
  dir <- system.file("extdata", package = "COMETS", mustWork = TRUE)
  xlsfile <- file.path(dir, "uid.xls")
  masteruids = fixData(readxl::read_excel(xlsfile,1))
  library(dplyr)
  library(tidyr)

  # Compile indices of name columns
  uidsource.ind <- grep("_name", names(masteruids))

  # get master cohortlist list
  mastercname <-
    aggregate(
      uidsource ~ metid + uid_01 + biochemical ,
      masteruids %>%
        gather("uidsource", "metid", c(
          min(uidsource.ind):max(uidsource.ind)
        )) %>%
        filter(metid != "") %>%
        distinct(metid, uid_01, uidsource) %>%
        mutate(metid = tolower(metid)) %>%
        arrange(metid) %>%
        filter(uidsource != ""),
      paste,
      collapse = ";"
    )


  # get master HMDB list
  masterhmdb <- aggregate(
    hmdb ~ metid + uid_01,
    masteruids %>%
      gather("uidsource", "metid", c(min(uidsource.ind):max(uidsource.ind))) %>%
      filter(metid != "") %>%
      gather("hmdbsrc", "hmdb", hmdb_id1, hmdb_id2) %>%
      select(metid, uid_01, hmdb) %>%
      distinct(metid, uid_01, hmdb) %>%
      mutate(metid = tolower(metid)) %>%
      arrange(metid) %>%
      filter(hmdb != ""),
    paste,
    collapse = ";",
    na.action = na.pass
  )


  #use this table for harmonization:
  masteruidl <-
    dplyr::full_join(mastercname, masterhmdb) %>% dplyr::mutate(hmdb = ifelse(is.na(hmdb), " ", hmdb)) %>% dplyr::arrange(metid)


  # make unique metid and concatenate rows
  mastermetid <-
    aggregate(
      cbind(uid_01, hmdb, uidsource, biochemical) ~ metid,
      data = masteruidl,
      paste,
      collapse = "#",
      na.action = na.pass
    )

  # bring in super-pathway
  mastermetid<-dplyr::left_join(mastermetid,dplyr::select(masteruids,uid_01,super_pathway),by=c("uid_01"="uid_01"))
  mastermetid$metid<-gsub("\\*$","",mastermetid$metid)

  rm(
    uidsource.ind,
    mastercname,
    masterhmdb,
    masteruids,
    masteruidl,
    xlsfile,
    fixData,
    dir
  )

}
#'
#'


# ---------------------------------------------------------------------------
# processcohorts function ----------------------------------------------------------
# ---------------------------------------------------------------------------
#' Code that creates the cohorts dataframe file
#' @keywords internal
processCOHORTS <- function() {
  dir <- system.file("extdata", package = "COMETS", mustWork = TRUE)
  xlsfile <- file.path(dir, "cohorts.xlsx")
  cohorts = readxl::read_excel(xlsfile,1)
  rm(dir,xlsfile)
}

