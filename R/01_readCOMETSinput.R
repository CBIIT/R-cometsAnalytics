#' Read in CSV file that contains metabolite abundances and metab data
#'
#' @param csvfilePath path of CSV file to be read in
#' @param modelspec if "BATCH", then runs multiple models (default is "NoBATCH")
#'
#' @return a list comprising:
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInput.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#'
#' @export

readCOMETSinput <- function(csvfilePath,modelspec="Interactive") {
    stopifnot(is.character(csvfilePath))
    if (!file.exists(csvfilePath)) {
        stop("CSV input file does not exist")
    }

    #metabolite meta data
    dta.metab<-fixData(readxl::read_excel(csvfilePath, 1))
    #subject metabolite data
    dta.smetab<-fixData(readxl::read_excel(csvfilePath, 2))
    #subject data
    dta.sdata<-fixData(readxl::read_excel(csvfilePath, 3))
    #variable mapping
    dta.vmap<-fixData(readxl::read_excel(csvfilePath, 4))
    #batch model specifications
    dta.models<-fixData(readxl::read_excel(csvfilePath, 5))

  # Check file integrity:
    ckintegrity=checkIntegrity(dta.metab=dta.metab,dta.smetab=dta.smetab,
	dta.sdata=dta.sdata,dta.vmap=dta.vmap,dta.models=dta.models)
    integritymessage=ckintegrity$outmessage
    dta.metab=ckintegrity$dta.metab
    dta.smetab=ckintegrity$dta.smetab
    dta.sdata=ckintegrity$dta.sdata
    print("Printing Integrity Message:")
    print(integritymessage)


# If an error was found during integrity check (e.g. not all metabolites or subjects
# in the SubjectMetabolite sheet are annotated in the respective metadatasheets Subjects
# and Metabolites), then return only integrity check
   if (length(grep("Error", ckintegrity$outmessage))>0) {
	dtalist=list(integritymessage=integritymessage,mods=dta.models) }
  else {
	dta <- dplyr::inner_join(dta.sdata, dta.smetab)

      idvar<-tolower(dta.vmap[['cohortvariable']][dta.vmap[['varreference']] == 'id'])
      metabvar<-tolower(dta.vmap[['cohortvariable']][dta.vmap[['varreference']] == "metabolite_id"])

     #rename variables if batch mode so we can run models
    if (modelspec == 'Batch') {
      # take only vars that are named differently for the cohort
      tst<-dplyr::filter(dta.vmap,!is.na(dta.vmap[["cohortvariable"]]) & dta.vmap[["varreference"]] != "metabolite_id")

      newnames <- plyr::mapvalues(names(dta),
                            from = c(tolower(tst$cohortvariable)),
                            to = c(tolower(tst$varreference)))

      names(dta) <- newnames

       #rename cohort metabid to metabolite id
       names(dta.metab)<-plyr::mapvalues(names(dta.metab),from=metabvar,to="metabolite_id")
    }

    # run through all vmap specifications to create variables
    dtalist <- list(subjdata = dta, # metabolite abundances
      allMetabolites = names(dta.smetab)[-1], # metabolite names
      allSubjectMetaData = names(dta.sdata)[-1], # subject meta data
      allSubjects = dta.sdata[,idvar], # subject names
      subjId = idvar, # id used for subject names
      metabId = metabvar, # id used for metabolite names
      metab = dta.metab, # metabolite meta data
      mods = dta.models, # model specification information
      integritymessage = integritymessage # message for integrity check
    )

    # Harmonize metabolites
    dtalist<-Harmonize(dtalist)

    # Determine whether data is already transformed:
    mymets=dtalist$metab[[dtalist$metabId]]
    if(min(dtalist$subjdata[,mymets],na.rm=T)<0) {transformation=TRUE} 
    else {transformation=FALSE}
    dtalist$transformation=transformation

    # add summary statistics
    log2metvar=as.numeric(lapply(mymets, function(x) {
      temp=which(colnames(dtalist$subjdata)==x)
      if(length(temp)==0) {return(NA)}
      else {
          if(transformation==TRUE) return(stats::var(dtalist$subjdata[[x]],na.rm=TRUE))
          else return(stats::var(log2(dtalist$subjdata[[x]]),na.rm=TRUE))
      }
    }))
    num.min=as.numeric(lapply(mymets, function(x) {
      temp=which(colnames(dtalist$subjdata)==x)
      if(length(temp)==0) {return(NA)}
      else return(length(which(dtalist$subjdata[[x]]==min(dtalist$subjdata[[x]],na.rm=TRUE))))
    }))

    dtalist$metab$var=log2metvar
    dtalist$metab$num.min=num.min

#    print(paste("end of readdata",Sys.time()))
    } # end else integritycheck doesn't contain error

    return(dtalist)
}
