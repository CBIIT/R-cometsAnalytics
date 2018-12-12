#' Read in Excell file that contains metabolite abundances and metab data
#'
#' @param csvfilePath path of Excell file to be read in
#' @return a list comprising:
#'
#' @examples
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#'
#' @export

readCOMETSinput <- function(csvfilePath) {
  stopifnot(is.character(csvfilePath))
  if (!file.exists(csvfilePath)) {
    stop("CSV input file does not exist")
  }

  #metabolite meta data
  dta.metab <-
    suppressWarnings(fixData(readxl::read_excel(csvfilePath, "Metabolites")))
  print("Metabolites sheet is read in")
  #subject metabolite data
  dta.smetab <-
    suppressWarnings(fixData(readxl::read_excel(csvfilePath, "SubjectMetabolites",skip=1,col_names=F)))
  print("SubjectMetabolites sheet is read in")

  # Read in the first row (column names) of subject metabolite data (this is now the dictionary of metabolite names)
  dict_metabnames <- 
	tolower(fixData(readxl::read_excel(csvfilePath, "SubjectMetabolites",col_names=F,n_max=1)))
  names(dict_metabnames) <- colnames(dta.smetab)
 
  #subject data
  dta.sdata <-
    suppressWarnings(fixData(readxl::read_excel(csvfilePath, "SubjectData")))
  print("SubjectData sheet is read in")
  #variable mapping
  dta.vmap <-
    suppressWarnings(fixData(readxl::read_excel(csvfilePath, "VarMap")))
  print("VarMap sheet is read in")
  #batch model specifications
  dta.models <-
    suppressWarnings(fixData(readxl::read_excel(csvfilePath, "Models"),compbl=TRUE))
  print("Models sheet is read in")

  # Go through the varmap VARTYPE column and convert categorical entries into factors
  myfactors <- dta.vmap$cohortvariable[which(dta.vmap$vartype=="categorical" & dta.vmap$varreference!="metabolite_id")]
  print(paste("There are",length(myfactors),"categorical variables"))
  for (i in myfactors) {
	dta.sdata[,i] <- factor(dta.sdata[,i])
  }

  # Check file integrity:
  ckintegrity = checkIntegrity(
    dta.metab = dta.metab,
    dta.smetab = dta.smetab,
    dta.sdata = dta.sdata,
    dta.vmap = dta.vmap,
    dta.models = dta.models,
    dict_metabnames = dict_metabnames
  )
  integritymessage = ckintegrity$outmessage
  dta.metab = ckintegrity$dta.metab
  dta.smetab = ckintegrity$dta.smetab
  dta.sdata = ckintegrity$dta.sdata

  # If an error was found during integrity check (e.g. not all metabolites or subjects
  # in the SubjectMetabolite sheet are annotated in the respective metadatasheets Subjects
  # and Metabolites), then return only integrity check
  if (length(grep("Error", ckintegrity$outmessage)) > 0) {
    dtalist = list(integritymessage = integritymessage, mods = dta.models)
  }
  else {
    dta <- dplyr::inner_join(dta.sdata, dta.smetab)

    idvar <-
      base::tolower(dta.vmap[['cohortvariable']][dta.vmap[['varreference']] == 'id'])
    metabvar <-
      base::tolower(dta.vmap[['cohortvariable']][dta.vmap[['varreference']] == "metabolite_id"])

    # run through all vmap specifications to create variables
    dtalist <- list(
      subjdata = dta,
      # metabolite abundances
      allMetabolites = names(dta.smetab)[-1],
      # metabolite names
      allSubjectMetaData = names(dta.sdata)[-1],
      # subject meta data
      allSubjects = dta.sdata[, idvar],
      # subject names
      subjId = idvar,
      # id used for subject names
      metabId = metabvar,
      # id used for metabolite names
      metab = dta.metab,
      # metabolite meta data
      dict_metabnames = dict_metabnames,
      mods = dta.models,
      # model specification information
      integritymessage = integritymessage,
      # message for integrity check
      vmap = dplyr::filter(dta.vmap, !is.na(dta.vmap[["cohortvariable"]]) &
                             dta.vmap[["varreference"]] != "metabolite_id") # variable mapping
    )

    # Harmonize metabolites
    dtalist <- Harmonize(dtalist)

    # check to see which columns have non-missing values
    havedata <-
      base::apply(dtalist$subjdata, 2, function(x)
        all(is.na(x)))

    # keep only columns with non-missing values
    mymets = dtalist$metab[[dtalist$metabId]] # get complete list of metabolites
    # convert the names to indexes from dictionary:
    mymets <- as.character(lapply(mymets,function(x) names(dict_metabnames)[which(dict_metabnames==x)]))
    mymets = mymets[mymets %in% names(havedata[havedata == FALSE])]

    if (length(mymets) > 0) {
      # Determine whether data is already transformed:
      tst <- min(dtalist$subjdata[, c(mymets)], na.rm = T)
      if (tst < 0) {
        transformation = TRUE
      }
      else {
        transformation = FALSE
      }

      # ?? et 10/4/16 in the future, we might need to check whether each metabolite is transformed for now any negative value applies to all

      # add summary statistics
      log2metvar = as.numeric(base::lapply(mymets, function(x) {
        temp = which(colnames(dtalist$subjdata) == x)
        if (length(temp) == 0) {
          return(NA)
        } else {
          if (transformation == TRUE) {
            return(stats::var(dtalist$subjdata[[x]], na.rm = TRUE))
          } else {
            return(stats::var(log2(dtalist$subjdata[[x]]), na.rm = TRUE))
          }
        }
      }))

      # number of observations at minimum value
      num.min = as.numeric(lapply(mymets, function(x) {
        temp = which(colnames(dtalist$subjdata) == x)
        if (length(temp) == 0) {
          return(NA)
        } else
          return(length(which(
            dtalist$subjdata[[x]] == min(dtalist$subjdata[[x]], na.rm = TRUE)
          )))
      }))
      dtalist$transformation = transformation
      dtalist$metab$var[dtalist$metab[, dtalist$metabId] %in% dict_metabnames[mymets]] =
        log2metvar
      dtalist$metab$num.min[dtalist$metab[, dtalist$metabId] %in% dict_metabnames[mymets]] =
        num.min
    } else {
      # if all subject metabolite data is missing
      dtalist$transformation = NA
      dtalist$metab$var = NA
      dtalist$metab$num.min = NA
    }

    # vector of variables to display in table
    dtalist$dispvars <-
      c("outcome",
        "exposure",
        "adjvars",
        "corr",
        "pvalue",
        "n",
        "stratavar",
        "strata")

    print(integritymessage)
    return(dtalist)
  }
}


#' This function provides a description of the input data (for categorical data, the number of samples of each type; for continous data, the median and other statistics for each variable)
#' @param readData list from readComets
#'
#' @return a list with 2 data frames, continuous and categorical summaries. Type of variable is defined in varmap
#'
#' @examples
#' \dontrun{
#' dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
#' csvfile <- file.path(dir, "cometsInputAge.xlsx")
#' exmetabdata <- readCOMETSinput(csvfile)
#' allmodeloutput <- runAllModels(exmetabdata)
#' # Get descriptive data
#' descdata <-runDescrip(exmetabdata)
#' OutputXLSResults(filename="corr",datal=descdata,cohort="DPP")
#' }
#' @export
runDescrip<- function(readData){
  sumcat<-variable<-value<-cohort<-NULL
  # check if vartype is in vmap to see whether anyvars are categorical
  if (length(which(grepl("vartype",names(readData$vmap))))>0){

        catvars<-names(readData$subjdata)[which(sapply(readData$subjdata, is.factor)==TRUE)]

        msdata<-readData$subjdata %>%
                select_(catvars)
        msdata <- suppressWarnings(data.table::melt(readData$subjdata,measure.vars=catvars))
        sumcat <- msdata %>%
        group_by(variable, value) %>%
        summarise (n = n()) %>%
        mutate(proportion = n / sum(n))
  }

  sumcnt <-as.data.frame(psych::describe(readData$subjdata,
	quant = c(.05,.25,.5,.75,.95)))
  sumcnt$vars<-rownames(sumcnt)

  # Retrieve the original names for metabolites
  newvars <- data.frame(vars=as.character(names(readData$dict_metabname)),
		new=as.character(readData$dict_metabname),stringsAsFactors = FALSE)
  sumcnt <- suppressWarnings(dplyr::left_join(sumcnt, newvars,by=c("vars","vars"))) %>% 
	dplyr::mutate(vars = ifelse(!is.na(new), new, vars)) %>%
	dplyr::select(-new)
  colnames(newvars)[which(colnames(newvars)=="vars")]="variable"
  sumcat <- suppressWarnings(dplyr::left_join(as.data.frame(sumcat), newvars,by=c("variable","variable"))) %>%
        dplyr::mutate(variable = ifelse(!is.na(new), new, variable)) %>%
        dplyr::select(-new)

  return(list(sum_categorical=sumcat,sum_continuous=sumcnt))
}

