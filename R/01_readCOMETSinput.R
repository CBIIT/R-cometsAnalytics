#' Read in CSV file that contains metabolite abundances and metab data
#'
#' @param csvfilePath path of CSV file to be read in
#' @param modelspec if "BATCH", then runs multiple models (default is "NoBATCH")
#'
#' @return a list comprising:
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#'
#' @export

readCOMETSinput <- function(csvfilePath,modelspec="Interactive") {
    stopifnot(is.character(csvfilePath))
    if (!file.exists(csvfilePath)) {
        stop("CSV input file does not exist")
    }

    #metabolite meta data
    dta.metab<-suppressWarnings(fixData(readxl::read_excel(csvfilePath, "Metabolites")))
    #subject metabolite data
    dta.smetab<-suppressWarnings(fixData(readxl::read_excel(csvfilePath, "SubjectMetabolites")))
    #subject data
    dta.sdata<-suppressWarnings(fixData(readxl::read_excel(csvfilePath, "SubjectData")))
    #variable mapping
    dta.vmap<-suppressWarnings(fixData(readxl::read_excel(csvfilePath, "VarMap")))
    #batch model specifications
    dta.models<-suppressWarnings(fixData(readxl::read_excel(csvfilePath, "Models")))

  # Check file integrity:
    ckintegrity=checkIntegrity(dta.metab=dta.metab,dta.smetab=dta.smetab,
	dta.sdata=dta.sdata,dta.vmap=dta.vmap,dta.models=dta.models)
    integritymessage=ckintegrity$outmessage
    dta.metab=ckintegrity$dta.metab
    dta.smetab=ckintegrity$dta.smetab
    dta.sdata=ckintegrity$dta.sdata


# If an error was found during integrity check (e.g. not all metabolites or subjects
# in the SubjectMetabolite sheet are annotated in the respective metadatasheets Subjects
# and Metabolites), then return only integrity check
   if (length(grep("Error", ckintegrity$outmessage))>0) {
	dtalist=list(integritymessage=integritymessage,mods=dta.models) }
  else {
	dta <- dplyr::inner_join(dta.sdata, dta.smetab)

      idvar<-base::tolower(dta.vmap[['cohortvariable']][dta.vmap[['varreference']] == 'id'])
      metabvar<-base::tolower(dta.vmap[['cohortvariable']][dta.vmap[['varreference']] == "metabolite_id"])

     #rename variables if batch mode so we can run models
    if (modelspec == 'Batch') {
      # take only vars that are named differently for the cohort
      tst<-dplyr::filter(dta.vmap,!is.na(dta.vmap[["cohortvariable"]]) & dta.vmap[["varreference"]] != "metabolite_id")

      newnames <- plyr::mapvalues(names(dta),
                            from = c(base::tolower(tst$cohortvariable)),
                            to = c(base::tolower(tst$varreference)))

      names(dta) <- newnames

       #rename cohort metabid to metabolite id
#       names(dta.metab)<-plyr::mapvalues(names(dta.metab),from=metabvar,to="metabolite_id")
    }

# Change Models so that they grab the correct cohortvariable name
    modelvar=unique(c(dta.models$outcomes,dta.models$exposure, dta.models$adjustment))
    modelvar=base::tolower(base::setdiff(unique(modelvar[!is.na(modelvar)]),
	"All metabolites"))

    for (i in modelvar) {
	newmodelvar=base::tolower(dta.vmap$cohortvariable[which(dta.vmap$varreference==i)])
        dta.models$outcomes[which(dta.models$outcomes==i)]=newmodelvar
        dta.models$exposure[which(dta.models$exposure==i)]=newmodelvar
        dta.models$adjustment[which(dta.models$adjustment==i)]=newmodelvar
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

    # check to see which columns have non-missing values
    havedata<-base::apply(dtalist$subjdata,2, function(x)all(is.na(x)))

    # keep only columns with non-missing values
    mymets=dtalist$metab[[dtalist$metabId]] # get complete list of metabolites
    mymets=mymets[mymets %in% names(havedata[havedata==FALSE])]

    if (length(mymets)>0) {
      # Determine whether data is already transformed:
        tst<-min(dtalist$subjdata[,c(mymets)],na.rm=T)
        if(tst<0) {transformation=TRUE}
        else {transformation=FALSE}

      # ?? et 10/4/16 in the future, we might need to check whether each metabolite is transformed for now any negative value applies to all

        # add summary statistics
        log2metvar=as.numeric(base::lapply(mymets, function(x) {
          temp=which(colnames(dtalist$subjdata)==x)
          if(length(temp)==0) {return(NA)} else {
            if(transformation==TRUE) {
		return(stats::var(dtalist$subjdata[[x]],na.rm=TRUE))} else {
		return(stats::var(log2(dtalist$subjdata[[x]]),na.rm=TRUE))}
          }
        }))

        # number of observations at minimum value
        num.min=as.numeric(lapply(mymets, function(x) {
          temp=which(colnames(dtalist$subjdata)==x)
          if(length(temp)==0) {return(NA)}
          else return(length(which(dtalist$subjdata[[x]]==min(dtalist$subjdata[[x]],na.rm=TRUE))))
        }))
        dtalist$transformation=transformation
        dtalist$metab$var[dtalist$metab[,dtalist$metabId] %in% mymets]=log2metvar
        dtalist$metab$num.min[dtalist$metab[,dtalist$metabId] %in% mymets]=num.min
    }
    else { # if all subject metabolite data is missing
      dtalist$transformation=NA
      dtalist$metab$var = NA
      dtalist$metab$num.min = NA
    }

    # vector of variables to display in table
    dtalist$dispvars <- c("outcome","exposure","adjvars","corr","pvalue","n","cohort")




    # et it is possible that we can have non-numeric subject data for example, sex etc. 10/5/16
    # Check that all exposure variables are mode numeric:
#    modes=unique(sapply(dtalist$subjdata[,dtalist$allSubjectMetaData],class))
#    if(length(modes)>1 || modes != "numeric") {
#       stop("One of the variables used in the analysis (defined in the SubjectData sheet) includes non-numeric value.  These variables should include only numeric values.")
    print(integritymessage)
    return(dtalist)
}}
