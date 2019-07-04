## ------------------------------------------------------------------------
# Retrieve the full path of the input data
dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
csvfile <- file.path(dir, "cometsInputAge.xlsx")
# Read in and process the input data
exmetabdata <- COMETS::readCOMETSinput(csvfile)

## ---- eval=T-------------------------------------------------------------
COMETS::plotVar(exmetabdata,titlesize=12)

## ---- eval=F-------------------------------------------------------------
#  COMETS::plotMinvalues(exmetabdata,titlesize=12)

## ------------------------------------------------------------------------
exmodeldata <- COMETS::getModelData(exmetabdata,modlabel="1 Gender adjusted")

## ------------------------------------------------------------------------
exmodeldata <- COMETS::getModelData(exmetabdata,modelspec="Interactive",colvars=c("age","bmi_grp"),where=c("age>40","bmi_grp>2"))

## ------------------------------------------------------------------------
excorrdata  <- COMETS::runCorr(exmodeldata,exmetabdata,"DPP")

## ------------------------------------------------------------------------
COMETS::OutputCSVResults(filename="corr",dataf=excorrdata,cohort="DPP")

## ------------------------------------------------------------------------
COMETS::showCorr(excorrdata,nlines=3)

## ----in-text-fig---------------------------------------------------------
COMETS::showHeatmap(excorrdata,plothgt=350,plotwid=400)

## ------------------------------------------------------------------------
exmodeldata<-COMETS::getModelData(exmetabdata,modelspec = "Interactive",colvars = c("bmi_grp","age"))
excorrdata  <- COMETS::runCorr(exmodeldata,exmetabdata,"DPP")
# COMETS::showHClust(excorrdata)

## ------------------------------------------------------------------------
  exmodeldata2 <- COMETS::getModelData(exmetabdata,modelspec="Interactive",rowvars=c("lactose","lactate"),
	colvars=c("age","bmi_grp"),strvars="race_grp")
  excorrdata2  <- COMETS::runCorr(exmodeldata2,exmetabdata,"DPP")

## ----eval=F--------------------------------------------------------------
#   allresults <- COMETS::runAllModels(exmetabdata,writeTofile=F)
#  

## ------------------------------------------------------------------------
sessionInfo()

