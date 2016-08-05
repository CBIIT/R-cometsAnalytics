#' Code that creates the "compileuids.RData file"
# dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
# csvfile <- file.path(dir, "uid.csv")
# masteruids=fixData(read.delim(csvfile,sep=",",header=T,fileEncoding="latin1"))
# 
# library(dplyr)
# library(tidyr)
# # Compile indices of name columns
# uidsource.ind<-grep("_name",names(masteruids))
# 
# # get master cohortlist list
# mastercname<-aggregate(uidsource~metid+uid_01+biochemical+super_pathway,masteruids %>%
#                         gather("uidsource", "metid",c(min(uidsource.ind):max(uidsource.ind))) %>%
#                         filter(metid!="") %>%
#                         distinct(metid,uid_01,uidsource) %>%
#                         mutate(metid=tolower(metid)) %>%
#                         arrange(metid) %>%
#                         filter(uidsource != ""),paste,collapse=";",na.action=na.pass)
# 
# 
# # get master HMDB list
# masterhmdb<-aggregate(hmdb~metid+uid_01,masteruids %>%
#                         gather("uidsource", "metid",c(min(uidsource.ind):max(uidsource.ind))) %>%
#                         filter(metid!="") %>%
#                         gather("hmdbsrc", "hmdb",hmdb_id1,hmdb_id2) %>%
#                         select(metid,uid_01,hmdb) %>%
#                         distinct(metid,uid_01,hmdb) %>%
#                         mutate(metid=tolower(metid)) %>%
#                         arrange(metid) %>%
#                         filter(hmdb != ""),paste,collapse=";",na.action=na.pass)
# 
# 
# #use this table for harmonization:
# masteruidl<-full_join(mastercname,masterhmdb) %>% mutate(hmdb=ifelse(is.na(hmdb)," ",hmdb))%>% arrange(metid)
# 
# 
# # make unique metid and concatenate rows
# mastermetid<-aggregate(cbind(uid_01,hmdb,uidsource,biochemical,super_pathway)~metid,data=masteruidl,paste,collapse="#",na.action = na.pass)
# 
# rm(uidsource.ind,mastercname,masterhmdb,masteruids,masteruidl,csvfile,fixData,dir)
#' 
#' 
#' 
#' # TESTING
#' # # join by metabolite_id only keep those with a match
#' # harmlistg<-inner_join(dtalist$metab,mastermetid,by=c("metabid"="metid"))
#' # 
#' # harmlistc<-left_join(anti_join(dtalist$metab,mastermetid,by=c("metabid"="metid")),mastermetid,by=c("metabolite_name"="metid"))
#' # 
#' # harmlist<-rbind(harmlistg,harmlistc)%>%mutate(multrows=grepl("#",uid_01))
