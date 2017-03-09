# ---------------------------------------------------------------------------
# fixData function ----------------------------------------------------------
# ---------------------------------------------------------------------------
#' Fixes input CSV data (e.g. takes care of factors, and other data frame conversions)
#' @keywords internal
#' @param dta results of reading a CSV data sheet (with read_excel)

fixData <- function(dta) {
  dta<-as.data.frame(dta) # have to convert to dataframe from local dataframe from readxl
  # run through the data
  colnames(dta) <- tolower(trimws(colnames(dta)))

  # remove rows that have all NAs (EM)
  countnas=as.numeric(apply(data.frame(dta),1,function(x) length(which(is.na(x)))))

  #trim white space
  #dta<-data.frame(lapply(dta,trimws))


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
        dta[, indc] <- trimws(dta[, indc])
      }
    }

  }

  return(dta)
} # end fixData function




# ---------------------------------------------------------------------------
# checkIntegrity function ---------------------------------------------------
# ---------------------------------------------------------------------------
#' Checks integrity of sheets in the user input CSV file
#' @keywords internal
#' @param dta.metab dta.metab
#' @param dta.smetab dta.smetab
#' @param dta.sdata dta.sdata
#' @param dta.vmap dta.vmap
#' @param dta.models dta.models
checkIntegrity <- function (dta.metab,dta.smetab, dta.sdata,dta.vmap,dta.models) {

    print("Running Integrity Check...")
    # get the cohort equivalent of metabolite_id and subject id
    metabid = tolower(dta.vmap$cohortvariable[tolower(dta.vmap$varreference) == "metabolite_id"])
    subjid = tolower(dta.vmap$cohortvariable[tolower(dta.vmap$varreference) == 'id'])
#    allmodelparams=c(dta.models$outcomes,dta.models$exposure, dta.models$adjustment)
    allmodelparams=c(dta.models$outcomes,dta.models$exposure, 
          unlist(lapply(dta.models$adjustment,function(x) strsplit(x,";"))))
    allmodelparams=tolower(unique(allmodelparams[!is.na(allmodelparams)]))
    outmessage = c()
    if (length(metabid) == 0) {
      stop("metabid is not found as a parameter in VarMap sheet!  Specify which column should be used for metabolite id")
    }
    else if (length(intersect(allmodelparams,
         tolower(c("All metabolites",  dta.vmap$varreference)))) !=length(allmodelparams))
# tolower(c("All metabolites", colnames(dta.smetab), colnames(dta.sdata)))))!=length(allmodelparams))
{
         stop("Parameters in model data ('Models' sheet in input file) do not exist!  Check the naming!")
    }
    else if (length(subjid) == 0) {
        stop("id (for subject id) is not found as a parameter in VarMap sheet!  Specify which column should be used for subject id")
    }
    else if (length(intersect(subjid,colnames(dta.sdata))) != 1) {
        stop("The user input id in the 'COHORTVARIABLE' column of the Varmap Sheet is not found in the 'SubjectData' sheet. Check the input file.")
    }
    else if (length(intersect(subjid,colnames(dta.smetab))) != 1) {
        stop("The user input id in the 'COHORTVARIABLE' column of the Varmap Sheet is not found in the 'SubjectMetabolites' sheet. Check the input file.")
    }
    else if (length(intersect(metabid,colnames(dta.metab))) != 1) {
        stop("The user input metabolite_id in the 'COHORTVARIABLE' column of the Varmap Sheet is not found in the 'Metabolites' sheet. Check the input file.")
    }
    else {
      #print("Passed the checks")
      dta.metab[[metabid]] = tolower(dta.metab[[metabid]])
      dta.sdata[[subjid]] = tolower(dta.sdata[[subjid]])
      dta.smetab[[subjid]] = tolower(dta.smetab[[subjid]])
      if (length(grep(metabid,colnames(dta.metab))) == 0) {
          stop("Error: Metabolite ID from 'VarMap Sheet' (",metabid,") does not match column name from 'Metabolites Sheet'")
      }
      else if (length(grep(subjid,colnames(dta.sdata))) == 0) {
          stop("Error: Sample ID from 'VarMap Sheet' (",subjid,") does not match a column name in 'SubjectData Sheet'")
      }
      else if (length(unique(dta.sdata[,subjid])) != length(unique(dta.smetab[,subjid]))) {
        outmessage = c(
          outmessage,"Warning: Number of subjects in SubjectData sheet does not match number of subjects in SubjectMetabolites sheet"
        )
      }
      else if (length(unique(colnames(dta.smetab))) != ncol(dta.smetab)) {
        outmessage = c(
          outmessage,"Warning: Metabolite abundances sheet (SubjectMetabolites) contains duplicate columns (metabolite names)"
        )
      }
      else if (length(unique(unlist(dta.sdata[,subjid]))) != nrow(dta.sdata)) {
        outmessage = c(
          outmessage,"Warning: Sample Information sheet (SubjectData) contains duplicate ids"
        )
      }
      else if (length(unique(unlist(dta.metab[,metabid]))) != nrow(dta.metab)) {
        outmessage = c(
          outmessage,"Warning: Metabolite Information sheet (Metabolites) contains duplicate metabolite ids"
        )
      }
      else {
        nummetab = length(unique(colnames(dta.smetab)[-c(which(colnames(dta.smetab) ==
                                                                 subjid))]))
        numsamples = length(unique(dta.smetab[[subjid]]))
        if (length(intersect(as.character(unlist(dta.metab[,metabid])),colnames(dta.smetab)[-c(which(colnames(dta.smetab) ==
                                                                                                     subjid))])) == nummetab &&
            length(intersect(as.character(unlist(dta.sdata[,subjid])),dta.smetab[[subjid]])) ==
            numsamples) {
          outmessage = c(
            outmessage,"Passed all integrity checks, analyses can proceed. If you are part of COMETS, please download metabolite list below and submit to the COMETS harmonization group."
          )
        }
        else {
          if (length(intersect(tolower(dta.metab[[metabid]]),tolower(colnames(dta.smetab)))) !=
              nummetab) {
              stop("Error: Metabolites in SubjectMetabolites DO NOT ALL match metabolite ids in Metabolites Sheet")
          }
          if (length(intersect(dta.sdata[[subjid]],dta.smetab[[subjid]])) !=
              numsamples) {
              stop("Error: Sample ids in SubjectMetabolites DO NOT ALL match subject ids in SubjectData sheet")
          }
        }
      }
    }
    if (is.null(outmessage)) {
      outmessage = "Input data has passed QC (metabolite and sample names match in all input files)"
    }

    return(
      list(
        dta.smetab = dta.smetab,dta.metab = dta.metab, dta.sdata = dta.sdata,outmessage =
          outmessage
      )
    )
  } # end checkIntegriy


# ---------------------------------------------------------------------------
# Harmonize ---------------------------------------------------
# ---------------------------------------------------------------------------
#' Fixes input CSV data (e.g. takes care of factors, and other data frame conversions)
#' @keywords internal
#' @param dtalist results of reading a CSV data sheet (with read_excel)

Harmonize<-function(dtalist){
  mastermetid=metabolite_name=metlower=uid_01=NULL
  # Load processed UIDs file:
  dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
  masterfile <- file.path(dir, "compileduids.RData")
  load(masterfile)

  # rename metid to be the same as metabid
  colnames(mastermetid)[which(colnames(mastermetid)=="metid")]=dtalist$metabId


  # join by metabolite_id only keep those with a match
  harmlistg<-dplyr::inner_join(dtalist$metab,mastermetid,by=c(dtalist$metabId))

  # join by metabolite_name only keep those with a match
  harmlistc<-dplyr::left_join(dplyr::anti_join(dtalist$metab,mastermetid,
        by=c(dtalist$metabId)) %>%
          dplyr::mutate(metlower=gsub("\\*$","",tolower(metabolite_name))), # take out * in metabolite name
        mastermetid,by=c("metlower"=dtalist$metabId)) %>% dplyr::select(-metlower)

  # combine the 2 data frames
  dtalist$metab<-rbind(harmlistg,harmlistc) %>%
    dplyr::mutate(multrows=grepl("#",uid_01),harmflag=!is.na(uid_01))

  return(dtalist)

}




# ---------------------------------------------------------------------------
# prdebug ---------------------------------------------------
# ---------------------------------------------------------------------------
#' debug by printing object with time time
#' @keywords internal
#' @param lab label of object
#' @param x object
#
prdebug<-function(lab,x){
  print(paste(lab," = ",x," Time: ",Sys.time()))
}

