#' #' Code that creates the "compileuids.RData file"
#' 
#' masteruids=fixData(read.delim("uid.csv",sep=",",header=T,fileEncoding="latin1"))
#' 
#' # Compile indices of name columns
#' cnames.ind<-grep("_name",names(masteruids))
#' 
#' # get master cohortlist list
#' mastercname<-aggregate(cname~metid+uid_01+biochemical,masteruids %>% 
#'                         gather("cname", "metid",c(min(cnames.ind):max(cnames.ind))) %>% 
#'                         filter(metid!="") %>%
#'                         distinct(metid,uid_01,cname) %>%
#'                         mutate(metid=tolower(metid)) %>%
#'                         arrange(metid) %>%
#'                         filter(cname != ""),paste,collapse=";",na.action=na.pass) 
#' 
#' 
#' # get master HMDB list
#' masterhmdb<-aggregate(hmdb~metid+uid_01,masteruids %>% 
#'                         gather("cname", "metid",c(min(cnames.ind):max(cnames.ind))) %>% 
#'                         filter(metid!="") %>%
#'                         gather("hmdbsrc", "hmdb",hmdb_id1,hmdb_id2) %>% 
#'                         select(metid,uid_01,hmdb) %>%
#'                         distinct(metid,uid_01,hmdb) %>%
#'                         mutate(metid=tolower(metid)) %>%
#'                         arrange(metid) %>%
#'                         filter(hmdb != ""),paste,collapse=";",na.action=na.pass) 
#' 
#' 
#' #use this table for harmonization:
#' masteruidl<-full_join(mastercname,masterhmdb) %>% mutate(hmdb=ifelse(is.na(hmdb)," ",hmdb))%>% arrange(metid)
#' 
#' 
#' # make unique metid and concatenate rows
#' mastermetid<-aggregate(cbind(uid_01,hmdb,cname,biochemical)~metid,data=masteruidl,paste,collapse="#",na.action = na.pass)
#' 
#' rm(cnames.ind,mastercname,masterhmdb,masteruids,masteruidl)
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
