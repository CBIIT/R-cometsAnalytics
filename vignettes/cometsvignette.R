## ------------------------------------------------------------------------
# Retrieve the full path of the input data
dir <- system.file("extdata", package="COMETS", mustWork=TRUE)
csvfile <- file.path(dir, "cometsInput.xlsx")
# Read in and process the input data
exmetabdata <- COMETS::readCOMETSinput(csvfile)

## ------------------------------------------------------------------------
exmodeldata <- COMETS::getModelData(exmetabdata,modbatch="1.1 Unadjusted")

## ------------------------------------------------------------------------
exmodeldata <- COMETS::getModelData(exmetabdata,modelspec="Interactive",colvars="age bmi")

## ------------------------------------------------------------------------
excorrdata  <- COMETS::getCorr(exmodeldata,exmetabdata,"DPP")

## ------------------------------------------------------------------------
COMETS::OutputCorrResults(filename="corr",dataf=excorrdata,cohort="DPP")

## ------------------------------------------------------------------------
showCorr(excorrdata,nlines=3)

## ----fig_width=6, fig_height=6-------------------------------------------
COMETS::showHeatmap(excorrdata,plothgt=350,plotwid=400)


## ------------------------------------------------------------------------
exmodeldata<-COMETS::getModelData(exmetabdata,modelspec = "Interactive",colvars = "age bmi")
excorrdata  <- COMETS::getCorr(exmodeldata,exmetabdata,"DPP")
COMETS::showHClust(excorrdata)

## ------------------------------------------------------------------------
sessionInfo()

