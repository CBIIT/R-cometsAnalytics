## ---------------------------------------------------------------------------
## processUID function ----------------------------------------------------------
## ---------------------------------------------------------------------------
##' Code that creates the "compileuids.RData file"
##' @keywords internal
##' assumes uid file is in your working directorys
#
# uncomment to run
processUID <- function() {

dir <- system.file("R", package = "COMETS", mustWork = TRUE)
library(tidyverse)
source(paste0(getwd(),"/R/internalFunctions.r"))
# initialize parameters:
metid=uid_01=uidsource=hmdb=main_class=c()


## dir <- system.file("extdata", package = "COMETS", mustWork = TRUE)
xlsfile <- file.path(getwd(), "/inst/extdata/uid.xlsx")
masteruids = fixData(rio::import(xlsfile,sheet="UID")) %>%
  mutate(comp_id=comp_id1, # create official vars
         biochemical=biochemical_name_ims,
         hmdb_id=hmdb_id1,
         chemical_id=chemical_id1)

# Compile indices of name columns
uidsource.ind <- intersect(grep("_name", names(masteruids)),
                           grep("biochemical_", names(masteruids),invert = TRUE))

 # get master list of all metabolite_id and attribute to source
 mastercname <-
   stats::aggregate(
     uidsource ~ metid + uid_01 ,
     masteruids %>%
       tidyr::gather("uidsource", "metid", c(uidsource.ind)) %>%
       dplyr::filter(metid != "") %>%
       dplyr::mutate(metid=gsub("\\*$","",metid)) %>%
       dplyr::distinct(metid, uid_01, uidsource) %>%
       dplyr::mutate(metid = tolower(metid)) %>%
       dplyr::arrange(metid) %>%
       dplyr::filter(uidsource != ""),
     paste,
     collapse = ";"
   )


 # bring in super-pathway and other metabolite metadata and clean up the NA and # for blank values
 mastermetid<-dplyr::left_join(mastercname,dplyr::select(masteruids,uid_01,main_class,chemical_id,comp_id,hmdb_id,biochemical),by=c("uid_01"="uid_01")) %>%
     dplyr::mutate(hmdb_id=trimws(hmdb_id)) %>%
     dplyr::mutate(comp_id=gsub(".000000","",comp_id)) %>%
     dplyr::mutate(hmdb_id=gsub("$#","",hmdb_id)) %>%
     dplyr::mutate(hmdb_id=gsub("^#","",hmdb_id)) %>%
     dplyr::mutate(hmdb_id=gsub("$NA","",hmdb_id)) %>%
     dplyr::mutate(comp_id=trimws(comp_id)) %>%
     dplyr::mutate(comp_id=gsub("$#","",comp_id)) %>%
     dplyr::mutate(comp_id=gsub("^#","",comp_id)) %>%
     dplyr::mutate(comp_id=gsub("$NA","",comp_id)) %>%
     dplyr::mutate(chemical_id=trimws(chemical_id)) %>%
     dplyr::mutate(chemical_id=gsub("$#","",chemical_id)) %>%
     dplyr::mutate(chemical_id=gsub("^#","",chemical_id)) %>%
     dplyr::mutate(chemical_id=gsub("$NA","",chemical_id)) %>%
     dplyr::mutate(main_class=trimws(main_class)) %>%
     dplyr::mutate(main_class=gsub("$#","",main_class)) %>%
     dplyr::mutate(main_class=gsub("^#","",main_class)) %>%
     dplyr::mutate(main_class=gsub("$NA","",main_class)) %>%
     dplyr::mutate(main_class=gsub("^#NA","",main_class)) %>%
     dplyr::mutate(main_class=ifelse(trimws(main_class) == "NA","",main_class)) %>%
     dplyr::mutate(main_class=ifelse(trimws(main_class) == "-","",main_class)) %>%
     dplyr::arrange(uid_01)


   # make unique metid and concatenate all rows for values of the other 7 columns
 mastermetid<-aggregate(cbind(uidsource,main_class,chemical_id,comp_id,hmdb_id,biochemical)~metid+uid_01,
                        data=mastermetid,
                        paste,
                        collapse="#",
                        na.action=na.pass)%>%
            dplyr::mutate(main_class=ifelse(trimws(main_class) == "NA","",main_class)) %>%
            dplyr::mutate(main_class=ifelse(trimws(main_class) == "NA#NA","",main_class)) %>%
            dplyr::mutate(hmdb_id=ifelse(trimws(hmdb_id) == "NA#NA","",hmdb_id)) %>%
            dplyr::mutate(comp_id=ifelse(trimws(comp_id) == "NA","",comp_id)) %>%
            dplyr::mutate(chemical_id=ifelse(trimws(chemical_id) == "NA","",chemical_id)) %>%
            dplyr::mutate(hmdb_id=ifelse(trimws(hmdb_id) == "NA","",hmdb_id))

   #final make unique
   # testing duplicated mastermetid
mastermetid<-aggregate(cbind(uid_01,uidsource,main_class,chemical_id,comp_id,hmdb_id,biochemical)~metid,
                           data=mastermetid,
                           paste,
                           collapse="#",
                           na.action=na.pass)%>%
  dplyr::mutate(main_class=ifelse(trimws(main_class) == "NA","",main_class)) %>%
  dplyr::mutate(main_class=ifelse(trimws(main_class) == "NA#NA","",main_class)) %>%
  dplyr::mutate(hmdb_id=ifelse(trimws(hmdb_id) == "NA#NA","",hmdb_id)) %>%
  dplyr::mutate(comp_id=ifelse(trimws(comp_id) == "NA","",comp_id)) %>%
  dplyr::mutate(chemical_id=ifelse(trimws(chemical_id) == "NA","",chemical_id)) %>%
  dplyr::mutate(hmdb_id=ifelse(trimws(hmdb_id) == "NA","",hmdb_id))


# compile  master hmdb------------------
# take only unique hmdb (those with only 1 hmdb in uid_01)
 onlyhmdb<-rbind(dplyr::filter(masteruids,hmdb_id1!="") %>% select(-hmdb_id,-hmdb_id2,-hmdb_id3) %>% rename(hmdb_id=hmdb_id1),
                 dplyr::filter(masteruids,hmdb_id2!="") %>% select(-hmdb_id,-hmdb_id1,-hmdb_id3) %>% rename(hmdb_id=hmdb_id2),
                 dplyr::filter(masteruids,hmdb_id3!="") %>% select(-hmdb_id,-hmdb_id1,-hmdb_id2) %>% rename(hmdb_id=hmdb_id3)) %>%
  filter(!grepl("_",uid_01)) %>%
   dplyr::distinct(hmdb_id,.keep_all=TRUE)


hmdb<-onlyhmdb %>%
  tidyr::gather("uidsource", "metid", uidsource.ind)

hmdb1<-stats::aggregate(uidsource ~ hmdb_id + uid_01 ,hmdb,paste,collapse = ";")

# bring in super-pathway and other metabolite metadata
masterhmdb <- hmdb1 %>%
  filter(!grepl("_",uid_01)) %>% # keep only single hmdb identifiers
  left_join(dplyr::filter(onlyhmdb,!grepl("_",uid_01))%>%dplyr::select(main_class,chemical_id,comp_id,hmdb_id,biochemical))



## ---------------------------------------------------------------------------
## processcohorts function ----------------------------------------------------------
## ---------------------------------------------------------------------------
#dir <- system.file("extdata", package = "COMETS", mustWork = TRUE)
xlsfile <- file.path(getwd(), "/inst/extdata/cohorts.xlsx")
cohorts = readxl::read_excel(xlsfile,1)


## ---------------------------------------------------------------------------
## update rdata for package ----------------------------------------------------------
## ---------------------------------------------------------------------------
# save updated rdata from the global env
rdfile <- file.path(paste0(getwd(),"/inst/extdata"), "compileduids.RData")
save(cohorts,masterhmdb,mastermetid,file=rdfile)
msgret<-paste(c("Compiled using the following names:",names(masteruids)[uidsource.ind]))
rmlist<-setdiff(ls(),c("cohorts","masterhmdb","mastermetid","msgret"))
rm(list=rmlist,rmlist)
return (msgret)

}
# do not run

#' \dontrun{
#'  processUID()
#' }


}





# ---------------------------------------------------------------------------
# fixData function ----------------------------------------------------------
# ---------------------------------------------------------------------------
#' Fixes input CSV data (e.g. takes care of factors, and other data frame conversions)
#' @keywords internal
#' @param dta any data frame
#' @param compbl compress multiple blank spaces to single blank space for all character or factor variables in the dataset

fixData <- function(dta,compbl=FALSE) {
  dta<-as.data.frame(dta) # have to convert to dataframe from local dataframe from readxl
  # run through the data
  colnames(dta) <- tolower(trimws(colnames(dta)))

  # remove rows that have all NAs (EM)
  countnas=as.numeric(apply(data.frame(dta),1,function(x) length(which(is.na(x)))))
  if (length(which(countnas==ncol(dta)))>0) {
    dta=dta[-c(which(countnas==ncol(dta))),]
  }

  cls <- sapply(dta, class)
  # do conversions for data types: integer to numeric and dates are identified by date in name

  if (length(which(cls == "integer")) > 0) {
    for (ind in which(cls == "integer"))
      dta[,ind] <- as.numeric(dta[,ind])
  }

  if (length(which(cls == "factor")) > 0) {
    for (ind in which(cls == "factor"))
      dta[,ind] <- trimws(as.character(dta[,ind])) # trim and convert to character
  }
  if (length(which(cls == "character")) > 0) {
    for (indc in names(dta)) {
      if(class(dta[, indc]) %in% c("factor", "character")){
        if (compbl==TRUE)
          dta[, indc] <- gsub("\\s+", " ",trimws(dta[, indc])) # compress duplicate blanks
        else
          dta[, indc] <- trimws(dta[, indc])
      }
    }

  }

  return(dta)
} # end fixData function


