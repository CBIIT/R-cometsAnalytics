## ------------------------------------------------------------------------
# Retrieve the full path of the input data
dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
csvfile <- file.path(dir, "cometsInputAge.xlsx")
# Read in and process the input data
exmetabdata <- COMETS::readCOMETSinput(csvfile)

## ------------------------------------------------------------------------
COMETS::plotVar(exmetabdata,titlesize=12)

## ------------------------------------------------------------------------
COMETS::plotMinvalues(exmetabdata,titlesize=12)

## ------------------------------------------------------------------------
exmodeldata <- COMETS::getModelData(exmetabdata,modlabel="1.1 Unadjusted")

## ------------------------------------------------------------------------
exmodeldata <- COMETS::getModelData(exmetabdata,modelspec="Interactive",colvars=c("age","bmi"))

## ------------------------------------------------------------------------
excorrdata  <- COMETS::getCorr(exmodeldata,exmetabdata,"DPP")

## ------------------------------------------------------------------------
COMETS::OutputCSVResults(filename="corr",dataf=excorrdata,cohort="DPP")

## ------------------------------------------------------------------------
COMETS::showCorr(excorrdata,nlines=3)

## ----in-text-fig---------------------------------------------------------
#COMETS::showHeatmap(excorrdata,plothgt=350,plotwid=400)

## ------------------------------------------------------------------------
exmodeldata<-COMETS::getModelData(exmetabdata,modelspec = "Interactive",colvars = "age bmi")
excorrdata  <- COMETS::getCorr(exmodeldata,exmetabdata,"DPP")
COMETS::showHClust(excorrdata)

